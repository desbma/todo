//! Command line arguments handling

use std::path::PathBuf;

use clap::Parser;

/// todo.txt files to use, if not set use `TODO_FILE` env var
#[derive(clap::Args, Debug)]
pub(crate) struct Files {
    pub files: Vec<PathBuf>,
}

#[derive(clap::Args, Debug)]
#[group(required = true, multiple = true)]
pub(crate) struct ConflictPaths {
    /// Path to the conflict copy of `todo.txt`
    #[arg(long)]
    pub todo_conflict: Option<PathBuf>,
    /// Path to the conflict copy of `done.txt`
    #[arg(long)]
    pub done_conflict: Option<PathBuf>,
}

#[derive(Parser, Debug)]
#[command(version, about)]
pub(crate) enum Action {
    /// Add new task
    Add {
        /// todo.txt file to use, if not set use `TODO_FILE` env var
        #[arg(short, long)]
        file: Option<PathBuf>,
        args: Vec<String>,
    },
    /// Auto archive and auto create recurring tasks if needed
    Auto {
        #[command(flatten)]
        files: Files,
    },
    /// List all tasks, ordered by urgency
    List {
        #[command(flatten)]
        files: Files,
    },
    /// Interactive task menu
    Menu {
        #[command(flatten)]
        files: Files,
    },
    /// Get most urgent task
    Next {
        /// Short output
        #[arg(short, long, default_value_t = false)]
        simple: bool,
        #[command(flatten)]
        files: Files,
    },
    /// Send notification for overdue tasks
    NotifyOverdue {
        #[command(flatten)]
        files: Files,
    },
    /// Get pending task count
    PendingCount {
        #[command(flatten)]
        files: Files,
    },
    /// Get tasks created or done in the most recent days
    Report {
        days: usize,
        #[command(flatten)]
        files: Files,
    },
    /// Resolve a file sync conflict
    ResolveConflict {
        /// Path to the local todo.txt file
        todo_file: PathBuf,
        /// Path to the local done.txt file
        done_file: PathBuf,
        #[command(flatten)]
        conflicts: ConflictPaths,
    },
    /// Undo last action
    Undo {
        #[command(flatten)]
        files: Files,
    },
}
