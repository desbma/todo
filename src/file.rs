//! Todo.txt file handling

use std::env;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use chrono::Duration;
use lazy_static::lazy_static;
use notify::Watcher;

use crate::task::{CreationCompletion, Date, Task};

#[derive(Debug, Eq, PartialEq)]
pub struct TodoFile {
    todo_path: PathBuf,
    done_path: PathBuf,
}

impl TodoFile {
    pub fn new(todo_path: &Path, done_path: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            todo_path: todo_path.to_path_buf(),
            done_path: done_path.to_path_buf(),
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
        for mut task in tasks {
            task.force_no_styling = true;
            writeln!(new_todo_file_writer, "{task}")?;
        }

        // Overwrite task file
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn watch<F>(&self, handler: F) -> anyhow::Result<Box<dyn notify::Watcher>>
    where
        F: notify::EventHandler,
    {
        let mut watcher = Box::new(notify::recommended_watcher(handler)?);
        watcher.watch(&self.todo_path, notify::RecursiveMode::NonRecursive)?;
        Ok(watcher)
    }

    pub fn add_task(&self, mut new_task: Task, today: &Date) -> anyhow::Result<()> {
        // Set task created date if needed
        if let CreationCompletion::Pending { created: None } = new_task.status {
            new_task.status = CreationCompletion::Pending {
                created: Some(*today),
            };
        }

        // Append to file
        let mut file = OpenOptions::new().append(true).open(&self.todo_path)?;
        new_task.force_no_styling = true;
        writeln!(file, "{new_task}")?;

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

        // Auto archive
        let mut tasks = self.load_tasks()?;
        self.auto_archive(&mut tasks, today)?;

        // TODO auto recur

        // Write other tasks to it
        for mut other_task in tasks.into_iter().filter(|t| *t != task) {
            other_task.force_no_styling = true;
            writeln!(new_todo_file_writer, "{other_task}")?;
        }

        // Set task done and write it
        task.set_done(today);
        task.force_no_styling = true;
        writeln!(new_todo_file_writer, "{task}")?;

        // Write new recurring task if any
        if let Some(mut new_recur_task) = task.recur(today) {
            new_recur_task.force_no_styling = true;
            writeln!(new_todo_file_writer, "{new_recur_task}")?;
        }

        // Overwrite task file
        let new_todo_file = new_todo_file_writer.into_inner()?;
        new_todo_file.persist(&self.todo_path)?;

        Ok(())
    }

    pub fn auto_archive(&self, tasks: &mut Vec<Task>, today: &Date) -> anyhow::Result<()> {
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

        if !to_archive.is_empty() {
            // Append to file
            let done_file = OpenOptions::new().append(true).open(&self.done_path)?;
            let mut done_writer = BufWriter::new(done_file);
            let to_archive_count = to_archive.len();
            for mut task in to_archive {
                task.force_no_styling = true;
                writeln!(done_writer, "{task}")?;
            }
            log::info!("Archived {to_archive_count} tasks");
        }

        Ok(())
    }

    #[allow(dead_code)]
    #[allow(clippy::ptr_arg)]
    pub fn auto_recur(&self, _tasks: &mut Vec<Task>, _today: &Date) -> anyhow::Result<()> {
        todo!();
    }

    #[allow(dead_code)]
    pub fn autobackup(&self, _today: &Date) -> anyhow::Result<()> {
        todo!();
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
