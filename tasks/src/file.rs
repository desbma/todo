//! Todo.txt file handling

use std::{
    env,
    fs::{self, File, OpenOptions},
    io::{self, BufRead as _, BufReader, BufWriter, Read as _, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::LazyLock,
};

use chrono::Duration;
use notify::Watcher;

use crate::task::{CreationCompletion, Date, Task};

#[derive(Debug, Eq, PartialEq)]
pub struct TodoFile {
    todo_path: PathBuf,
    done_path: PathBuf,
}

const UNDO_HISTORY_LEN: usize = 5;
const DONE_FILE_TASK_COUNT_COMPRESS_THRESHOLD: usize = 2000;
const DONE_FILE_TASK_COUNT_TARGET: usize = 1000;
const ZSTD_COMPRESSION_LEVEL: i32 = 19;
static AUTO_ARCHIVE_COMPLETED_THRESHOLD: LazyLock<Duration> = LazyLock::new(|| Duration::days(2));

impl TodoFile {
    pub fn new(todo_path: &Path, done_path: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            todo_path: todo_path.canonicalize()?,
            done_path: done_path.canonicalize()?,
        })
    }

    /// Create a `TodoFile` from the `TODO_FILE` and `DONE_FILE` environment variables
    pub fn from_env() -> anyhow::Result<Self> {
        let todotxt_var = env::var_os("TODO_FILE");
        let todotxt_path = todotxt_var
            .as_ref()
            .map(Path::new)
            .ok_or_else(|| anyhow::anyhow!("TODO_FILE environment variable is not set"))?;
        let done_var = env::var_os("DONE_FILE");
        let done_path = done_var
            .as_ref()
            .map(Path::new)
            .ok_or_else(|| anyhow::anyhow!("DONE_FILE environment variable is not set"))?;
        Self::new(todotxt_path, done_path)
    }

    /// Return the canonical path to the todo file
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.todo_path
    }

    pub fn load_tasks(&self) -> anyhow::Result<Vec<Task>> {
        let file = File::open(&self.todo_path)?;
        let reader = BufReader::new(file);
        reader
            .lines()
            .flat_map(|r| r.map(|l| l.parse()))
            .enumerate()
            .map(|(i, r)| {
                r.map(|mut t: Task| {
                    t.index = Some(i);
                    t
                })
            })
            .collect::<Result<_, _>>()
    }

    pub fn save_tasks(&self, tasks: Vec<Task>) -> anyhow::Result<()> {
        // Create new file
        let new_todo_file = tempfile::NamedTempFile::new_in(self.todo_path.parent().unwrap())?;
        let mut new_todo_file_writer = BufWriter::new(new_todo_file);

        // Write tasks to it
        for task in tasks {
            Self::write_task(&mut new_todo_file_writer, &task)?;
        }

        // Backup
        self.backup()?;

        // Overwrite task file
        #[expect(clippy::shadow_unrelated)]
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn watch(&self) -> anyhow::Result<(Box<dyn Watcher>, crossbeam_channel::Receiver<()>)> {
        let (event_tx, event_rx) = crossbeam_channel::bounded(1);
        let todo_path = self.todo_path.clone();
        let mut watcher = Box::new(notify::recommended_watcher(
            move |evt: notify::Result<notify::Event>| {
                log::debug!("Watcher event {evt:?}");
                if let Ok(evt) = evt {
                    match evt.kind {
                        notify::EventKind::Create(_)
                        | notify::EventKind::Modify(_)
                        | notify::EventKind::Remove(_) => {
                            if evt.paths.contains(&todo_path) {
                                let _ = event_tx.try_send(());
                            }
                        }
                        _ if evt.need_rescan() => {
                            let _ = event_tx.try_send(());
                        }
                        _ => {}
                    }
                }
            },
        )?);
        watcher.watch(
            self.todo_path.parent().unwrap(),
            notify::RecursiveMode::NonRecursive,
        )?;
        Ok((watcher, event_rx))
    }

    pub fn add_task(&self, mut new_task: Task, today: Date) -> anyhow::Result<()> {
        // Set task created date if needed
        if let CreationCompletion::Pending { created: None } = new_task.status {
            new_task.status = CreationCompletion::Pending {
                created: Some(today),
            };
        }

        // Backup
        self.backup()?;

        // Append to file
        let mut file = OpenOptions::new().append(true).open(&self.todo_path)?;
        Self::write_task(&mut file, &new_task)?;

        Ok(())
    }

    pub fn edit(&self, task: &Task) -> anyhow::Result<()> {
        // Create temporary copy
        let tmp_todo_file = tempfile::NamedTempFile::new_in(self.todo_path.parent().unwrap())?;
        fs::copy(&self.todo_path, tmp_todo_file.path())?;

        // Edit it
        let editor = env::var("EDITOR")?;
        Command::new(editor)
            .arg(format!(
                "{}:{}",
                tmp_todo_file.path().to_str().unwrap(),
                task.index.unwrap() + 1
            ))
            .status()?;

        // Backup and overwrite if different
        if !Self::same_content(&self.todo_path, tmp_todo_file.path())? {
            self.backup()?;
            tmp_todo_file.persist(&self.todo_path)?;
        }

        Ok(())
    }

    pub fn start(&self, task: &Task, today: Date) -> anyhow::Result<()> {
        // Create new file
        let new_todo_file = tempfile::NamedTempFile::new_in(self.todo_path.parent().unwrap())?;
        let mut new_todo_file_writer = BufWriter::new(new_todo_file);

        // Auto recur
        let mut tasks = self.load_tasks()?;
        Self::auto_recur(&mut tasks);

        // Auto archive
        self.auto_archive(&mut tasks, today)?;

        // Write tasks to it
        for mut cur_task in tasks {
            if cur_task == *task {
                cur_task.start(today);
            }
            Self::write_task(&mut new_todo_file_writer, &cur_task)?;
        }

        // Backup
        self.backup()?;

        // Overwrite task file
        #[expect(clippy::shadow_unrelated)]
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn set_done(&self, mut task: Task, today: Date) -> anyhow::Result<()> {
        // Create new file
        let new_todo_file = tempfile::NamedTempFile::new_in(self.todo_path.parent().unwrap())?;
        let mut new_todo_file_writer = BufWriter::new(new_todo_file);

        // Auto recur
        let mut tasks = self.load_tasks()?;
        Self::auto_recur(&mut tasks);

        // Auto archive
        self.auto_archive(&mut tasks, today)?;

        // Write other tasks to it
        for other_task in tasks.iter().filter(|t| **t != task) {
            Self::write_task(&mut new_todo_file_writer, other_task)?;
        }

        // Set task done and write it
        task.set_done(today)?;
        Self::write_task(&mut new_todo_file_writer, &task)?;

        // Write new recurring task if any
        if let Some(new_recur_task) = task.recur(today) {
            Self::write_task(&mut new_todo_file_writer, &new_recur_task)?;
        }

        // Backup
        self.backup()?;

        // Overwrite task file
        #[expect(clippy::shadow_unrelated)]
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn auto_archive(&self, tasks: &mut Vec<Task>, today: Date) -> anyhow::Result<usize> {
        let to_archive: Vec<_> = tasks
            .extract_if(.., |task| {
                if let CreationCompletion::Completed { completed, .. } = task.status
                    && ((today - completed) >= *AUTO_ARCHIVE_COMPLETED_THRESHOLD)
                {
                    true
                } else {
                    false
                }
            })
            .collect();

        let to_archive_count = to_archive.len();
        if !to_archive.is_empty() {
            // Append to file
            let done_file = OpenOptions::new().append(true).open(&self.done_path)?;
            let mut done_writer = BufWriter::new(done_file);
            for task in to_archive {
                Self::write_task(&mut done_writer, &task)?;
            }
        }

        let archived_count = to_archive_count;
        if archived_count > 0 {
            log::info!("Archived {archived_count} task(s)");
        }

        // Compress done tasks if needed
        // NB: optimized for common case, which is to count lines and bail out
        let done_file = File::open(&self.done_path)?;
        let done_reader = BufReader::new(done_file);
        let done_line_count = done_reader.lines().count();
        if done_line_count >= DONE_FILE_TASK_COUNT_COMPRESS_THRESHOLD {
            // Read done lines
            let done_lines: Vec<_> = fs::read_to_string(&self.done_path)?
                .lines()
                .map(ToString::to_string)
                .collect();

            // Split done file
            #[expect(clippy::indexing_slicing)]
            let new_done_lines = &done_lines[done_line_count - DONE_FILE_TASK_COUNT_TARGET..];
            debug_assert_eq!(new_done_lines.len(), DONE_FILE_TASK_COUNT_TARGET);
            #[expect(clippy::indexing_slicing)]
            let to_compress_lines = &done_lines[..done_line_count - DONE_FILE_TASK_COUNT_TARGET];

            // Open existing compressed file
            let compressed_filepath = self.done_path.with_file_name(format!(
                "{}.zst",
                self.done_path.file_name().unwrap().to_string_lossy(),
            ));
            let compressed_file_reader = if compressed_filepath.exists() {
                let compressed_file = File::open(&compressed_filepath)?;
                Some(zstd::Decoder::with_buffer(BufReader::new(compressed_file))?)
            } else {
                None
            };

            // Create new compressed file
            let new_compressed_file =
                tempfile::NamedTempFile::new_in(compressed_filepath.parent().unwrap())?;
            let mut new_compressed_file_writer =
                zstd::Encoder::new(BufWriter::new(new_compressed_file), ZSTD_COMPRESSION_LEVEL)?;

            // Write tasks that were already compressed and new ones
            if let Some(mut compressed_file_reader) = compressed_file_reader {
                io::copy(&mut compressed_file_reader, &mut new_compressed_file_writer)?;
            }
            for to_compress_line in to_compress_lines {
                writeln!(new_compressed_file_writer, "{to_compress_line}")?;
            }
            let new_compressed_file_writer = new_compressed_file_writer.finish()?;
            let compressed_task_count = to_compress_lines.len();

            // Create new done file
            let new_done_file = tempfile::NamedTempFile::new_in(self.done_path.parent().unwrap())?;
            let mut new_done_file_writer = BufWriter::new(new_done_file);

            // Write done lines
            for new_done_line in new_done_lines {
                writeln!(new_done_file_writer, "{new_done_line}")?;
            }

            // Overwrite compressed file
            #[expect(clippy::shadow_unrelated)]
            let new_compressed_file = new_compressed_file_writer.into_inner()?;
            new_compressed_file.persist(&compressed_filepath)?;

            // Overwrite done file
            #[expect(clippy::shadow_unrelated)]
            let new_done_file = new_done_file_writer.into_inner()?;
            new_done_file.persist(&self.done_path)?;

            log::info!("Compressed {compressed_task_count} archived tasks");
        }

        Ok(archived_count)
    }

    pub fn auto_recur(tasks: &mut Vec<Task>) -> usize {
        let mut new_tasks: Vec<_> = tasks
            .iter()
            .filter(|t| {
                matches!(t.status, CreationCompletion::Completed { .. })
                    && t.recurrence().is_some()
                    && !tasks.iter().any(|t2| {
                        matches!(t2.status, CreationCompletion::Pending { .. })
                            && t.is_same_recurring(t2)
                    })
            })
            .map(|t| {
                let CreationCompletion::Completed { completed, .. } = t.status else {
                    unreachable!()
                };
                t.recur(completed).unwrap()
            })
            .collect();

        let new_task_count = new_tasks.len();
        tasks.append(&mut new_tasks);

        if new_task_count > 0 {
            log::info!("Added {new_task_count} recurring task(s)");
        }
        new_task_count
    }

    pub fn backup(&self) -> anyhow::Result<()> {
        for src_idx in (1..UNDO_HISTORY_LEN).rev() {
            let src = self.backup_path(src_idx);
            if !src.is_file() {
                continue;
            }
            let dst = self.backup_path(src_idx + 1);
            if dst.is_file() && Self::same_content(&src, &dst)? {
                continue;
            }
            log::debug!("{src:?} -> {dst:?}");
            fs::rename(src, dst)?;
        }
        let dst = self.backup_path(1);
        log::debug!("{:?} -> {:?}", self.todo_path, dst);
        fs::copy(&self.todo_path, dst)?;
        Ok(())
    }

    fn backup_path(&self, index: usize) -> PathBuf {
        self.todo_path.with_file_name(format!(
            "{}.bak.{}",
            self.todo_path.file_name().unwrap().to_string_lossy(),
            index
        ))
    }

    pub fn undo_diff(&self) -> anyhow::Result<()> {
        let mut diff_child = Command::new("diff")
            .args([
                "-U",
                "0",
                self.todo_path.to_str().unwrap(),
                self.backup_path(1).to_str().unwrap(),
            ])
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .spawn()?;
        let delta_status = Command::new("delta")
            .args([
                "--default-language=todo.txt",
                "--no-gitconfig",
                "--file-style",
                "omit",
                "--hunk-header-style",
                "omit",
            ])
            .stdin(diff_child.stdout.take().unwrap())
            .status()?;
        diff_child.wait()?;
        if !delta_status.success() {
            anyhow::bail!("delta failed with code {delta_status:?}")
        }
        Ok(())
    }

    pub fn undo(&self) -> anyhow::Result<()> {
        for src_idx in 1..=UNDO_HISTORY_LEN {
            let src = self.backup_path(src_idx);
            if !src.is_file() {
                break;
            }
            let dst = if src_idx == 1 {
                self.todo_path.clone()
            } else {
                self.backup_path(src_idx - 1)
            };
            log::debug!("{src:?} -> {dst:?}");
            fs::rename(src, dst)?;
        }
        Ok(())
    }

    fn same_content(path1: &Path, path2: &Path) -> anyhow::Result<bool> {
        if path1.metadata()?.len() == path2.metadata()?.len() {
            // This is not the most efficient way to compare,
            // but for files of a few KB, it does not matter
            Ok(fs::read_to_string(path1)? == fs::read_to_string(path2)?)
        } else {
            Ok(false)
        }
    }

    fn write_task<W>(writer: &mut W, task: &Task) -> io::Result<()>
    where
        W: Write,
    {
        writeln!(writer, "{}", task.to_todotxt_line())
    }

    pub fn filter_all<F>(&self, f: F) -> anyhow::Result<Vec<Task>>
    where
        F: Copy + Fn(&Task) -> bool,
    {
        let todo_file = File::open(&self.todo_path)?;
        let done_file = File::open(&self.done_path)?;
        let reader = BufReader::new(todo_file).chain(BufReader::new(done_file));
        let r = reader
            .lines()
            .flat_map(|r| r.map(|l| l.parse()))
            .filter(|r| r.as_ref().map(f).unwrap_or(true))
            .collect::<Result<_, _>>()?;
        Ok(r)
    }
}

#[cfg(test)]
#[expect(clippy::shadow_unrelated)]
mod tests {
    use std::io::Write as _;

    use tempfile::NamedTempFile;

    use super::*;

    fn todotxtfiles(lines: &[&str]) -> (NamedTempFile, NamedTempFile) {
        let mut todo_file = NamedTempFile::new().unwrap();
        for line in lines {
            writeln!(todo_file, "{line}").unwrap();
        }
        let done_file = NamedTempFile::new().unwrap();
        (todo_file, done_file)
    }

    #[test]
    fn empty() {
        let (todo_file, done_file) = todotxtfiles(&[]);
        assert_eq!(
            TodoFile::new(todo_file.path(), done_file.path())
                .unwrap()
                .load_tasks()
                .unwrap(),
            vec![]
        );
    }

    #[test]
    fn simple() {
        let (todo_file, done_file) = todotxtfiles(&["task text", "(C) task2 text"]);
        assert_eq!(
            TodoFile::new(todo_file.path(), done_file.path())
                .unwrap()
                .load_tasks()
                .unwrap(),
            vec![
                Task {
                    text: "task text".to_owned(),
                    index: Some(0),
                    ..Task::default()
                },
                Task {
                    text: "task2 text".to_owned(),
                    priority: Some('C'),
                    index: Some(1),
                    ..Task::default()
                }
            ]
        );
    }

    fn date(y: i32, m: u32, d: u32) -> Date {
        Date::from_ymd_opt(y, m, d).unwrap()
    }

    fn todotxtfiles_with_done(
        todo_lines: &[&str],
        done_lines: &[&str],
    ) -> (NamedTempFile, NamedTempFile) {
        let mut todo_file = NamedTempFile::new().unwrap();
        for line in todo_lines {
            writeln!(todo_file, "{line}").unwrap();
        }
        let mut done_file = NamedTempFile::new().unwrap();
        for line in done_lines {
            writeln!(done_file, "{line}").unwrap();
        }
        (todo_file, done_file)
    }

    #[test]
    fn new_nonexistent_todo_path() {
        let done_file = NamedTempFile::new().unwrap();
        assert!(TodoFile::new(Path::new("/nonexistent/todo.txt"), done_file.path()).is_err());
    }

    #[test]
    fn new_nonexistent_done_path() {
        let todo_file = NamedTempFile::new().unwrap();
        assert!(TodoFile::new(todo_file.path(), Path::new("/nonexistent/done.txt")).is_err());
    }

    #[test]
    fn load_tasks_with_completed() {
        let (todo_file, done_file) =
            todotxtfiles(&["x 2024-01-15 2024-01-10 completed task pri:A"]);
        let tasks = TodoFile::new(todo_file.path(), done_file.path())
            .unwrap()
            .load_tasks()
            .unwrap();
        assert_eq!(tasks.len(), 1);
        assert_eq!(
            tasks[0].status,
            CreationCompletion::Completed {
                created: Some(date(2024, 1, 10)),
                completed: date(2024, 1, 15),
            }
        );
    }

    #[test]
    fn load_tasks_blank_lines_parsed_as_tasks() {
        let (todo_file, done_file) = todotxtfiles(&["task one", "", "task two"]);
        let tasks = TodoFile::new(todo_file.path(), done_file.path())
            .unwrap()
            .load_tasks()
            .unwrap();
        assert_eq!(tasks.len(), 3);
        assert_eq!(tasks[0].index, Some(0));
        assert_eq!(tasks[1].text, "");
        assert_eq!(tasks[1].index, Some(1));
        assert_eq!(tasks[2].index, Some(2));
    }

    #[test]
    fn save_tasks_roundtrip() {
        let (todo_file, done_file) =
            todotxtfiles(&["(A) 2024-03-01 important task", "simple task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        tf.save_tasks(tasks.clone()).unwrap();
        let reloaded = tf.load_tasks().unwrap();
        assert_eq!(tasks, reloaded);
    }

    #[test]
    fn save_tasks_overwrites() {
        let (todo_file, done_file) = todotxtfiles(&["old task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let new_tasks = vec![Task {
            text: "new task".to_owned(),
            ..Task::default()
        }];
        tf.save_tasks(new_tasks).unwrap();
        let loaded = tf.load_tasks().unwrap();
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].text, "new task");
    }

    #[test]
    fn save_empty_tasks() {
        let (todo_file, done_file) = todotxtfiles(&["old task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        tf.save_tasks(vec![]).unwrap();
        assert_eq!(tf.load_tasks().unwrap(), vec![]);
    }

    #[test]
    fn add_task_sets_created_date() {
        let (todo_file, done_file) = todotxtfiles(&[]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let task = Task {
            text: "new task".to_owned(),
            ..Task::default()
        };
        let today = date(2024, 6, 15);
        tf.add_task(task, today).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks.len(), 1);
        assert_eq!(
            tasks[0].status,
            CreationCompletion::Pending {
                created: Some(today),
            }
        );
    }

    #[test]
    fn add_task_preserves_existing_created_date() {
        let (todo_file, done_file) = todotxtfiles(&[]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let created = date(2024, 1, 1);
        let task = Task {
            text: "old task".to_owned(),
            status: CreationCompletion::Pending {
                created: Some(created),
            },
            ..Task::default()
        };
        tf.add_task(task, date(2024, 6, 15)).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(
            tasks[0].status,
            CreationCompletion::Pending {
                created: Some(created),
            }
        );
    }

    #[test]
    fn add_task_appends_to_existing() {
        let (todo_file, done_file) = todotxtfiles(&["existing task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let task = Task {
            text: "another task".to_owned(),
            ..Task::default()
        };
        tf.add_task(task, date(2024, 6, 15)).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks.len(), 2);
        assert_eq!(tasks[0].text, "existing task");
        assert_eq!(tasks[1].text, "another task");
    }

    #[test]
    fn start_sets_started_attribute() {
        let (todo_file, done_file) = todotxtfiles(&["task to start"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        let today = date(2024, 6, 15);
        tf.start(&tasks[0], today).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks[0].started_date(), Some(today));
    }

    #[test]
    fn start_preserves_other_tasks() {
        let (todo_file, done_file) = todotxtfiles(&["first task", "second task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        tf.start(&tasks[0], date(2024, 6, 15)).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks.len(), 2);
        assert_eq!(tasks[1].text, "second task");
        assert!(tasks[1].started_date().is_none());
    }

    #[test]
    fn set_done_removes_from_pending_and_appends_completed() {
        let (todo_file, done_file) = todotxtfiles(&["task one", "task two", "task three"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        let today = date(2024, 6, 15);
        tf.set_done(tasks[1].clone(), today).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks.len(), 3);
        assert_eq!(tasks[0].text, "task one");
        assert_eq!(tasks[1].text, "task three");
        assert_eq!(tasks[2].text, "task two");
        assert_eq!(
            tasks[2].status,
            CreationCompletion::Completed {
                created: None,
                completed: today,
            }
        );
    }

    #[test]
    fn set_done_with_recurrence_creates_new_task() {
        let (todo_file, done_file) = todotxtfiles(&["task with recurrence due:2024-06-15 rec:+7d"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        let today = date(2024, 6, 15);
        tf.set_done(tasks[0].clone(), today).unwrap();
        let tasks = tf.load_tasks().unwrap();
        assert_eq!(tasks.len(), 2);
        assert!(matches!(
            tasks[0].status,
            CreationCompletion::Completed { .. }
        ));
        assert!(matches!(
            tasks[1].status,
            CreationCompletion::Pending { .. }
        ));
        assert_eq!(tasks[1].due_date(), Some(date(2024, 6, 22)));
    }

    #[test]
    fn auto_archive_moves_old_completed_tasks() {
        let today = date(2024, 6, 15);
        let old_completed = "x 2024-06-12 2024-06-10 old done task".to_owned();
        let recent_completed = "x 2024-06-14 2024-06-13 recent done task".to_owned();
        let (todo_file, done_file) =
            todotxtfiles(&[&old_completed, &recent_completed, "pending task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let mut tasks = tf.load_tasks().unwrap();
        let archived = tf.auto_archive(&mut tasks, today).unwrap();
        assert_eq!(archived, 1);
        assert_eq!(tasks.len(), 2);
        assert!(tasks.iter().all(|t| t.text != "old done task"));

        let done_content = fs::read_to_string(done_file.path()).unwrap();
        assert!(done_content.contains("old done task"));
    }

    #[test]
    fn auto_archive_no_tasks_to_archive() {
        let today = date(2024, 6, 15);
        let (todo_file, done_file) = todotxtfiles(&["pending task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let mut tasks = tf.load_tasks().unwrap();
        let archived = tf.auto_archive(&mut tasks, today).unwrap();
        assert_eq!(archived, 0);
        assert_eq!(tasks.len(), 1);
    }

    #[test]
    fn auto_archive_completed_exactly_at_threshold_is_archived() {
        let today = date(2024, 6, 15);
        let exactly_threshold = "x 2024-06-13 2024-06-12 threshold task";
        let (todo_file, done_file) = todotxtfiles(&[exactly_threshold]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let mut tasks = tf.load_tasks().unwrap();
        let archived = tf.auto_archive(&mut tasks, today).unwrap();
        assert_eq!(archived, 1);
        assert!(tasks.is_empty());
    }

    #[test]
    fn auto_archive_completed_one_day_before_threshold_not_archived() {
        let today = date(2024, 6, 15);
        let just_under = "x 2024-06-14 2024-06-13 not yet task";
        let (todo_file, done_file) = todotxtfiles(&[just_under]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let mut tasks = tf.load_tasks().unwrap();
        let archived = tf.auto_archive(&mut tasks, today).unwrap();
        assert_eq!(archived, 0);
        assert_eq!(tasks.len(), 1);
    }

    #[test]
    fn auto_recur_creates_pending_for_completed_recurring_task() {
        let mut tasks: Vec<Task> = vec![
            "x 2024-06-15 2024-06-10 recurring task due:2024-06-15 rec:+7d"
                .parse()
                .unwrap(),
        ];
        let count = TodoFile::auto_recur(&mut tasks);
        assert_eq!(count, 1);
        assert_eq!(tasks.len(), 2);
        assert!(matches!(
            tasks[1].status,
            CreationCompletion::Pending { .. }
        ));
    }

    #[test]
    fn auto_recur_does_not_duplicate_if_pending_exists() {
        let mut tasks: Vec<Task> = vec![
            "x 2024-06-15 2024-06-10 recurring task due:2024-06-15 rec:+7d"
                .parse()
                .unwrap(),
            "2024-06-15 recurring task due:2024-06-22 rec:+7d"
                .parse()
                .unwrap(),
        ];
        let count = TodoFile::auto_recur(&mut tasks);
        assert_eq!(count, 0);
        assert_eq!(tasks.len(), 2);
    }

    #[test]
    fn auto_recur_no_recurring_tasks() {
        let mut tasks: Vec<Task> = vec!["plain task".parse().unwrap()];
        let count = TodoFile::auto_recur(&mut tasks);
        assert_eq!(count, 0);
        assert_eq!(tasks.len(), 1);
    }

    #[test]
    fn auto_recur_pending_recurring_task_not_recreated() {
        let mut tasks: Vec<Task> = vec!["pending task due:2024-06-15 rec:+7d".parse().unwrap()];
        let count = TodoFile::auto_recur(&mut tasks);
        assert_eq!(count, 0);
        assert_eq!(tasks.len(), 1);
    }

    #[test]
    fn backup_creates_backup_file() {
        let (todo_file, done_file) = todotxtfiles(&["task to backup"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        tf.backup().unwrap();
        let backup_path = todo_file.path().with_file_name(format!(
            "{}.bak.1",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        assert!(backup_path.exists());
        assert_eq!(
            fs::read_to_string(&backup_path).unwrap(),
            fs::read_to_string(todo_file.path()).unwrap()
        );
    }

    #[test]
    fn backup_rotates_history() {
        let (todo_file, done_file) = todotxtfiles(&["v1"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();

        tf.backup().unwrap();

        fs::write(todo_file.path(), "v2\n").unwrap();
        tf.backup().unwrap();

        fs::write(todo_file.path(), "v3\n").unwrap();
        tf.backup().unwrap();

        let bak1 = todo_file.path().with_file_name(format!(
            "{}.bak.1",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        let bak2 = todo_file.path().with_file_name(format!(
            "{}.bak.2",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        let bak3 = todo_file.path().with_file_name(format!(
            "{}.bak.3",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        assert_eq!(fs::read_to_string(&bak1).unwrap(), "v3\n");
        assert_eq!(fs::read_to_string(&bak2).unwrap(), "v2\n");
        assert_eq!(fs::read_to_string(&bak3).unwrap(), "v1\n");
    }

    #[test]
    fn backup_skips_rotation_when_destination_has_same_content() {
        let (todo_file, done_file) = todotxtfiles(&["same content"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();

        let bak_name = |i| {
            todo_file.path().with_file_name(format!(
                "{}.bak.{i}",
                todo_file.path().file_name().unwrap().to_string_lossy(),
            ))
        };

        // Pre-fill all backup slots with identical content
        for i in 1..=UNDO_HISTORY_LEN {
            fs::write(bak_name(i), "same content\n").unwrap();
        }

        tf.backup().unwrap();

        // All slots still have the same content and none were rotated past the limit
        for i in 1..=UNDO_HISTORY_LEN {
            assert!(bak_name(i).exists());
            assert_eq!(fs::read_to_string(bak_name(i)).unwrap(), "same content\n");
        }
        assert!(!bak_name(UNDO_HISTORY_LEN + 1).exists());
    }

    #[test]
    fn undo_restores_backup() {
        let (todo_file, done_file) = todotxtfiles(&["original"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();

        tf.backup().unwrap();
        fs::write(todo_file.path(), "modified\n").unwrap();

        tf.undo().unwrap();
        assert_eq!(fs::read_to_string(todo_file.path()).unwrap(), "original\n");
    }

    #[test]
    fn undo_shifts_backup_indices() {
        let (todo_file, done_file) = todotxtfiles(&["v1"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();

        tf.backup().unwrap();
        fs::write(todo_file.path(), "v2\n").unwrap();
        tf.backup().unwrap();
        fs::write(todo_file.path(), "v3\n").unwrap();

        tf.undo().unwrap();
        assert_eq!(fs::read_to_string(todo_file.path()).unwrap(), "v2\n");

        let bak1 = todo_file.path().with_file_name(format!(
            "{}.bak.1",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        assert_eq!(fs::read_to_string(&bak1).unwrap(), "v1\n");
    }

    #[test]
    fn undo_with_no_backup_is_noop() {
        let (todo_file, done_file) = todotxtfiles(&["content"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        tf.undo().unwrap();
        assert_eq!(fs::read_to_string(todo_file.path()).unwrap(), "content\n");
    }

    #[test]
    fn filter_all_from_both_files() {
        let (todo_file, done_file) = todotxtfiles_with_done(
            &["(A) pending task"],
            &["x 2024-06-15 2024-06-10 done task"],
        );
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let all = tf.filter_all(|_| true).unwrap();
        assert_eq!(all.len(), 2);
    }

    #[test]
    fn filter_all_filters_correctly() {
        let (todo_file, done_file) = todotxtfiles_with_done(
            &["(A) high prio", "(C) low prio"],
            &["x 2024-06-15 done task"],
        );
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let filtered = tf
            .filter_all(|t| matches!(t.status, CreationCompletion::Pending { .. }))
            .unwrap();
        assert_eq!(filtered.len(), 2);
        assert!(
            filtered
                .iter()
                .all(|t| matches!(t.status, CreationCompletion::Pending { .. }))
        );
    }

    #[test]
    fn filter_all_empty_files() {
        let (todo_file, done_file) = todotxtfiles_with_done(&[], &[]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let all = tf.filter_all(|_| true).unwrap();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn filter_all_matching_none() {
        let (todo_file, done_file) = todotxtfiles_with_done(&["task one"], &["x 2024-06-15 done"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let filtered = tf.filter_all(|_| false).unwrap();
        assert_eq!(filtered, vec![]);
    }

    #[test]
    fn save_then_load_preserves_attributes() {
        let (todo_file, done_file) =
            todotxtfiles(&["(B) 2024-01-01 +project task text due:2024-12-31 t:2024-06-01"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        let tasks = tf.load_tasks().unwrap();
        tf.save_tasks(tasks.clone()).unwrap();
        let reloaded = tf.load_tasks().unwrap();
        assert_eq!(tasks, reloaded);
    }

    #[test]
    fn save_creates_backup() {
        let (todo_file, done_file) = todotxtfiles(&["original task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        tf.save_tasks(vec![Task {
            text: "new task".to_owned(),
            ..Task::default()
        }])
        .unwrap();
        let backup_path = todo_file.path().with_file_name(format!(
            "{}.bak.1",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        assert!(backup_path.exists());
        let backup_content = fs::read_to_string(&backup_path).unwrap();
        assert!(backup_content.contains("original task"));
    }

    #[test]
    fn add_task_creates_backup() {
        let (todo_file, done_file) = todotxtfiles(&["existing task"]);
        let tf = TodoFile::new(todo_file.path(), done_file.path()).unwrap();
        tf.add_task(
            Task {
                text: "new".to_owned(),
                ..Task::default()
            },
            date(2024, 6, 15),
        )
        .unwrap();
        let backup_path = todo_file.path().with_file_name(format!(
            "{}.bak.1",
            todo_file.path().file_name().unwrap().to_string_lossy(),
        ));
        assert!(backup_path.exists());
    }
}
