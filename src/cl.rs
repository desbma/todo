//! Command line arguments handling

use clap::Parser;

#[derive(Parser, Debug)]
pub enum Action {
    List,
}
