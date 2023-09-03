use std::env;
use std::iter;
use std::path::Path;
use std::process::Command;
use std::sync::mpsc::channel;
use std::time::Duration;

use anyhow::Context;
use clap::Parser;
use wait_timeout::ChildExt;

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
        cl::Action::Menu { no_watch } => {
            let term = dialoguer::console::Term::stdout();
            let task_file = file::TodoFile::new(todotxt_path)?;
            if !no_watch {
                let (event_tx, event_rx) = channel();
                let _watcher = task_file.watch(event_tx).unwrap();
                let child_exe = env::current_exe()?;
                let child_args: Vec<_> = env::args()
                    .skip(1)
                    .chain(iter::once("--no-watch".to_string()))
                    .collect();
                loop {
                    log::debug!("Spawning child");
                    let mut child = Command::new(&child_exe).args(&child_args).spawn()?;

                    let mut restart_child = false;
                    while !restart_child {
                        // Wait a bit to debounce events and detect child exit
                        const CHILD_WAIT_DURATION: Duration = Duration::from_millis(500);
                        if let Some(status) = child.wait_timeout(CHILD_WAIT_DURATION)? {
                            if !status.success() {
                                log::warn!("Child exited with code {status:?}");
                            }
                            return Ok(());
                        };

                        for evt in event_rx.try_iter() {
                            let evt = evt?;
                            log::debug!("{evt:?}");
                            match evt.kind {
                                notify::EventKind::Create(_)
                                | notify::EventKind::Modify(_)
                                | notify::EventKind::Remove(_) => {
                                    restart_child = true;
                                }
                                _ => (),
                            }
                        }
                    }

                    if restart_child {
                        nix::sys::signal::kill(
                            nix::unistd::Pid::from_raw(child.id().try_into().unwrap()),
                            nix::sys::signal::Signal::SIGTERM,
                        )
                        .unwrap();
                        child.wait()?;
                        term.clear_screen()?;
                    }
                }
            } else {
                // Warning: console's themes do not support nesting styles
                let theme = dialoguer::theme::SimpleTheme;
                loop {
                    let mut tasks = task_file.tasks()?;
                    tasks.sort_unstable();
                    tasks.reverse();

                    let task_selection = dialoguer::FuzzySelect::with_theme(&theme)
                        .items(&tasks)
                        .default(0)
                        .highlight_matches(false)
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
    }

    Ok(())
}
