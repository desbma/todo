//! Todo.txt file handling

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

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

    pub fn tasks(&self) -> anyhow::Result<Vec<Task>> {
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

    pub fn watch<F>(&self, handler: F) -> anyhow::Result<Box<dyn notify::Watcher>>
    where
        F: notify::EventHandler,
    {
        let mut watcher = Box::new(notify::recommended_watcher(handler)?);
        watcher.watch(&self.path, notify::RecursiveMode::NonRecursive)?;
        Ok(watcher)
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
        assert_eq!(TodoFile::new(file.path()).unwrap().tasks().unwrap(), vec![]);
    }

    #[test]
    fn test_simple() {
        let file = todotxtfile(&["task text", "(C) task2 text"]);
        assert_eq!(
            TodoFile::new(file.path()).unwrap().tasks().unwrap(),
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
