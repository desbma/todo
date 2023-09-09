//! Command line arguments handling

use clap::Parser;

#[derive(Parser, Debug)]
pub enum Action {
    /// List all tasks, ordered by urgency
    List,
    /// Get most urgent task
    Next {
        /// Short output
        #[arg(short, long, default_value_t = false)]
        simple: bool,
    },
    /// Add new task
    Add { args: Vec<String> },
    /// Get pending task count
    PendingCount,
    /// Interactive task menu
    Menu {
        #[arg(short, long, default_value_t = false)]
        no_watch: bool,
    },
    /// Auto archive and auto create recurring tasks if needed
    Auto,
}
