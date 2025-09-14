//! Command line arguments handling

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about)]
pub(crate) enum Action {
    /// Add new task
    Add { args: Vec<String> },
    /// Auto archive and auto create recurring tasks if needed
    Auto,
    /// List all tasks, ordered by urgency
    List,
    /// Interactive task menu
    Menu {
        #[arg(short, long, default_value_t = false)]
        no_watch: bool,
    },
    /// Get most urgent task
    Next {
        /// Short output
        #[arg(short, long, default_value_t = false)]
        simple: bool,
    },
    /// Send notification for overdue tasks
    NotifyOverdue,
    /// Get pending task count
    PendingCount,
    /// Get tasks created or done in the most recent days
    Report { days: usize },
    /// Undo last action
    Undo,
}
