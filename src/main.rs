use std::env;
use std::path::Path;

use anyhow::Context;
use clap::Parser;

mod cl;
mod file;
mod task;

fn main() -> anyhow::Result<()> {
    // Init logger
    simple_logger::SimpleLogger::new()
        .init()
        .context("Failed to init logger")?;

    // Parse CL args
    let todotxt_var = env::var_os("TODO_FILE");
    let todotxt_path = todotxt_var
        .as_ref()
        .map(Path::new)
        .ok_or_else(|| anyhow::anyhow!("TODO_FILE environment variable is not set"))?;
    let cl_args = cl::Action::parse();
    match cl_args {
        cl::Action::List => {
            let task_file = file::TodoFile::new(todotxt_path)?;
            let tasks = task_file.tasks()?;
            log::trace!("{tasks:#?}");
            // TODO sort
            for task in tasks {
                println!("{task}");
            }
        }
    }

    Ok(())
}
