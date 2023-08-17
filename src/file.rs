//! Todo.txt file handling

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use crate::task::Task;

#[derive(Debug, Eq, PartialEq)]
pub struct TodoFile {
    path: PathBuf,
    // TODO inotify watch
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
            .collect::<Result<_, _>>()
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
                    ..Task::default()
                },
                Task {
                    text: "task2 text".to_string(),
                    priority: Some('C'),
                    ..Task::default()
                }
            ]
        );
    }
}
