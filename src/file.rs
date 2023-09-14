//! Todo.txt file handling

use std::env;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::mpsc;

use chrono::Duration;
use lazy_static::lazy_static;
use notify::Watcher;

use crate::task::{CreationCompletion, Date, Task};

#[derive(Debug, Eq, PartialEq)]
pub struct TodoFile {
    todo_path: PathBuf,
    done_path: PathBuf,
}

const UNDO_HISTORY_LEN: usize = 3;

impl TodoFile {
    pub fn new(todo_path: &Path, done_path: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            todo_path: todo_path.canonicalize()?,
            done_path: done_path.canonicalize()?,
        })
    }

    pub fn load_tasks(&self) -> anyhow::Result<Vec<Task>> {
        let file = File::open(&self.todo_path)?;
        let reader = BufReader::new(file);
        reader
            .lines()
            .flat_map(|l| l.map(|l| l.parse()))
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
            Self::write_task(&mut new_todo_file_writer, task)?;
        }

        // Backup
        self.backup()?;

        // Overwrite task file
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn watch(
        &self,
    ) -> anyhow::Result<(Box<dyn notify::Watcher>, mpsc::Receiver<notify::Event>)> {
        let (event_tx, event_rx) = mpsc::channel();
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
                                let _ = event_tx.send(evt);
                            }
                        }
                        _ => (),
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

    pub fn add_task(&self, mut new_task: Task, today: &Date) -> anyhow::Result<()> {
        // Set task created date if needed
        if let CreationCompletion::Pending { created: None } = new_task.status {
            new_task.status = CreationCompletion::Pending {
                created: Some(*today),
            };
        }

        // Backup
        self.backup()?;

        // Append to file
        let mut file = OpenOptions::new().append(true).open(&self.todo_path)?;
        Self::write_task(&mut file, new_task)?;

        Ok(())
    }

    pub fn edit(&self, task: &Task) -> anyhow::Result<()> {
        let editor = env::var("EDITOR")?;
        Command::new(editor)
            .arg(format!(
                "{}:{}",
                self.todo_path.to_str().unwrap(),
                task.index.unwrap() + 1
            ))
            .status()?;
        Ok(())
    }

    pub fn set_done(&self, mut task: Task, today: &Date) -> anyhow::Result<()> {
        // Create new file
        let new_todo_file = tempfile::NamedTempFile::new_in(self.todo_path.parent().unwrap())?;
        let mut new_todo_file_writer = BufWriter::new(new_todo_file);

        // Auto recur
        let mut tasks = self.load_tasks()?;
        self.auto_recur(&mut tasks)?;

        // Auto archive
        self.auto_archive(&mut tasks, today)?;

        // Write other tasks to it
        for other_task in tasks.into_iter().filter(|t| *t != task) {
            Self::write_task(&mut new_todo_file_writer, other_task)?;
        }

        // Set task done and write it
        task.set_done(today);
        Self::write_task(&mut new_todo_file_writer, task.clone())?;

        // Write new recurring task if any
        if let Some(new_recur_task) = task.recur(today) {
            Self::write_task(&mut new_todo_file_writer, new_recur_task)?;
        }

        // Backup
        self.backup()?;

        // Overwrite task file
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn auto_archive(&self, tasks: &mut Vec<Task>, today: &Date) -> anyhow::Result<usize> {
        lazy_static! {
            static ref AUTO_ARCHIVE_COMPLETED_THRESHOLD: Duration = Duration::days(2);
        }

        // TODO use https://doc.rust-lang.org/std/vec/struct.Vec.html#method.extract_if when stabilized
        let mut to_archive = Vec::new();
        let mut i = 0;
        while i < tasks.len() {
            if let CreationCompletion::Completed { completed, .. } = tasks[i].status {
                if *today - completed >= *AUTO_ARCHIVE_COMPLETED_THRESHOLD {
                    let task = tasks.remove(i);
                    to_archive.push(task);
                    continue;
                }
            }
            i += 1;
        }

        let to_archive_count = to_archive.len();
        if !to_archive.is_empty() {
            // Append to file
            let done_file = OpenOptions::new().append(true).open(&self.done_path)?;
            let mut done_writer = BufWriter::new(done_file);
            for task in to_archive {
                Self::write_task(&mut done_writer, task)?;
            }
        }

        if to_archive_count > 0 {
            log::info!("Archived {to_archive_count} task(s)");
        }
        Ok(to_archive_count)
    }

    pub fn auto_recur(&self, tasks: &mut Vec<Task>) -> anyhow::Result<usize> {
        let mut new_tasks: Vec<_> = tasks
            .iter()
            .filter(|t| {
                matches!(t.status, CreationCompletion::Completed { .. })
                    && t.recurrence().is_some()
                    && !tasks.iter().any(|t2| {
                        matches!(t2.status, CreationCompletion::Pending { .. }) && t.is_similar(t2)
                    })
            })
            .map(|t| {
                let completed_date = match t.status {
                    CreationCompletion::Completed { completed, .. } => completed,
                    _ => unreachable!(),
                };
                t.recur(&completed_date).unwrap()
            })
            .collect();

        let new_task_count = new_tasks.len();
        tasks.append(&mut new_tasks);

        if new_task_count > 0 {
            log::info!("Added {new_task_count} recurring task(s)");
        }
        Ok(new_task_count)
    }

    pub fn backup(&self) -> anyhow::Result<()> {
        for src_idx in (1..UNDO_HISTORY_LEN).rev() {
            let src = self.backup_path(src_idx);
            let dst = self.backup_path(src_idx + 1);
            if !src.is_file() {
                continue;
            }
            log::debug!("{src:?} -> {dst:?}");
            fs::copy(src, dst)?;
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

    #[allow(dead_code)]
    pub fn undo_diff(&self) -> anyhow::Result<String> {
        todo!();
    }

    #[allow(dead_code)]
    pub fn undo(&self) -> anyhow::Result<()> {
        todo!();
    }

    fn write_task<W>(writer: &mut W, mut task: Task) -> io::Result<()>
    where
        W: Write,
    {
        task.force_no_styling = true;
        writeln!(writer, "{task}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::io::Write;

    use tempfile::NamedTempFile;

    fn todotxtfiles(lines: &[&str]) -> (tempfile::NamedTempFile, tempfile::NamedTempFile) {
        let mut todo_file = NamedTempFile::new().unwrap();
        for line in lines {
            writeln!(todo_file, "{line}").unwrap();
        }
        let done_file = NamedTempFile::new().unwrap();
        (todo_file, done_file)
    }

    #[test]
    fn test_empty() {
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
    fn test_simple() {
        let (todo_file, done_file) = todotxtfiles(&["task text", "(C) task2 text"]);
        assert_eq!(
            TodoFile::new(todo_file.path(), done_file.path())
                .unwrap()
                .load_tasks()
                .unwrap(),
            vec![
                Task {
                    text: "task text".to_string(),
                    index: Some(0),
                    ..Task::default()
                },
                Task {
                    text: "task2 text".to_string(),
                    priority: Some('C'),
                    index: Some(1),
                    ..Task::default()
                }
            ]
        );
    }
}
