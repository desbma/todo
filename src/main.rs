use std::env;
use std::path::Path;
use std::process::Command;

use anyhow::Context;
use clap::Parser;

mod cl;
mod file;
mod task;

const TASK_ACTIONS: [&str; 2] = ["Mark as done", "Edit"];

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
            let mut tasks = task_file.tasks()?;
            log::trace!("{tasks:#?}");
            tasks.sort_unstable();
            tasks.reverse();
            for task in tasks {
                println!("{task}");
            }
        }
        cl::Action::Menu => {
            let term = dialoguer::console::Term::stderr();
            let task_file = file::TodoFile::new(todotxt_path)?;
            let theme = dialoguer::theme::ColorfulTheme::default();
            loop {
                let mut tasks = task_file.tasks()?;
                tasks.sort_unstable();
                tasks.reverse();

                let task_selection = dialoguer::FuzzySelect::with_theme(&theme)
                    .items(&tasks)
                    .default(0)
                    .interact_on_opt(&term)?;
                match task_selection {
                    None => break,
                    Some(task_idx) => {
                        let task = tasks.get(task_idx).unwrap();
                        let action_selection = dialoguer::Select::with_theme(&theme)
                            .items(&TASK_ACTIONS)
                            .default(0)
                            .interact_on_opt(&term)?;
                        match action_selection {
                            Some(0) => {
                                // TODO use our native 'do' code and don't rely on todo.sh
                                let status = Command::new("todo.sh")
                                    .args(["do", &format!("{}", task.index.unwrap() + 1)])
                                    .status()?;
                                anyhow::ensure!(status.success());
                            }
                            Some(1) => {
                                let editor = env::var("EDITOR")?;
                                Command::new(editor)
                                    .arg(format!(
                                        "{}:{}",
                                        todotxt_path.to_str().unwrap(),
                                        task.index.unwrap() + 1
                                    ))
                                    .status()?;
                            }
                            Some(_) => unreachable!(),
                            None => (),
                        }
                    }
                }
                term.clear_screen()?;
            }
        }
    }

    Ok(())
}
