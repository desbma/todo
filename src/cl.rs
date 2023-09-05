//! Command line arguments handling

use clap::Parser;

#[derive(Parser, Debug)]
pub enum Action {
    List,
    Next {
        #[arg(short, long, default_value_t = false)]
        simple: bool,
    },
    PendingCount,
    Menu {
        #[arg(short, long, default_value_t = false)]
        no_watch: bool,
    },
}
