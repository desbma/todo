//! Todo.txt file handling

use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::Command;

use notify::Watcher;

use crate::task::Task;

#[derive(Debug, Eq, PartialEq)]
pub struct TodoFile {
    path: PathBuf,
}

impl TodoFile {
    pub fn new(path: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            path: path.to_path_buf(),
        })
    }

    pub fn load_tasks(&self) -> anyhow::Result<Vec<Task>> {
        let file = File::open(&self.path)?;
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

    pub fn save_tasks(&self, tasks: &[Task]) -> anyhow::Result<()> {
        todo!();
    }

    pub fn watch<F>(&self, handler: F) -> anyhow::Result<Box<dyn notify::Watcher>>
    where
        F: notify::EventHandler,
    {
        let mut watcher = Box::new(notify::recommended_watcher(handler)?);
        watcher.watch(&self.path, notify::RecursiveMode::NonRecursive)?;
        Ok(watcher)
    }

    pub fn edit(&self, task: &Task) -> anyhow::Result<()> {
        let editor = env::var("EDITOR")?;
        Command::new(editor)
            .arg(format!(
                "{}:{}",
                self.path.to_str().unwrap(),
                task.index.unwrap() + 1
            ))
            .status()?;
        Ok(())
    }

    pub fn set_done(&self, task: &Task) -> anyhow::Result<()> {
        // TODO create new task file with tempfile, in same dir as task file
        // TODO write all tasks except done task to it
        // TODO write done task to it
        // TODO if rec attribute, compute delay relative to now or due
        // TODO add new task from recurrence
        // TODO atomically move to task file with tempfile's persist method

        // TODO use our native 'do' code and don't rely on todo.sh
        let status = Command::new("todo.sh")
            .args(["do", &format!("{}", task.index.unwrap() + 1)])
            .status()?;
        anyhow::ensure!(status.success());
        Ok(())
    }

    pub fn autoarchive(&self) -> anyhow::Result<()> {
        todo!();
    }

    pub fn autorecur(&self) -> anyhow::Result<()> {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::io::Write;

    use tempfile::NamedTempFile;

    fn todotxtfile(lines: &[&str]) -> tempfile::NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        for line in lines {
            writeln!(file, "{line}").unwrap();
        }
        file
    }

    #[test]
    fn test_empty() {
        let file = todotxtfile(&[]);
        assert_eq!(
            TodoFile::new(file.path()).unwrap().load_tasks().unwrap(),
            vec![]
        );
    }

    #[test]
    fn test_simple() {
        let file = todotxtfile(&["task text", "(C) task2 text"]);
        assert_eq!(
            TodoFile::new(file.path()).unwrap().load_tasks().unwrap(),
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
