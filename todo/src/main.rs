use std::{
    env, iter,
    os::unix::process::CommandExt,
    path::Path,
    process::Command,
    sync::mpsc,
    time::{Duration, Instant},
};

use anyhow::Context;
use clap::Parser;
use fzf_wrapped::Fzf;
use notify::Watcher;
use wait_timeout::ChildExt;

mod cl;

use tasks::{CreationCompletion, Date, StyleContext, TodoFile};

fn today() -> Date {
    chrono::Local::now().date_naive()
}

const TASK_ACTIONS: [&str; 3] = ["Mark as done", "Edit", "Start"];

fn watch_current_exe(exe_path: &Path) -> anyhow::Result<(Box<dyn Watcher>, mpsc::Receiver<()>)> {
    let (event_tx, event_rx) = mpsc::channel();
    let parent_dir = exe_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Path {exe_path:?} has no parent"))?;
    let exe_path = exe_path.to_owned();
    let mut watcher = Box::new(notify::recommended_watcher(
        move |evt: notify::Result<notify::Event>| {
            log::debug!("Watcher event {evt:?}");
            if let Ok(evt) = evt {
                match evt.kind {
                    notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_) => {
                        if evt.paths.contains(&exe_path) {
                            let _ = event_tx.send(());
                        }
                    }
                    _ => (),
                }
            }
        },
    )?);
    watcher.watch(parent_dir, notify::RecursiveMode::NonRecursive)?;
    Ok((watcher, event_rx))
}

/// exec current binary with same environment and args as it was started with
fn exec_self(current_exe: &Path) -> anyhow::Result<()> {
    let err = Command::new(current_exe)
        .args(env::args_os().skip(1))
        .exec();
    Err(err.into())
}

#[allow(clippy::too_many_lines)]
fn main() -> anyhow::Result<()> {
    // Init logger
    simple_logger::SimpleLogger::new()
        .with_level(if cfg!(debug_assertions) {
            log::LevelFilter::Debug
        } else {
            log::LevelFilter::Info
        })
        .env()
        .init()
        .context("Failed to setup logger")?;

    // Parse CL args
    let todotxt_var = env::var_os("TODO_FILE");
    let todotxt_path = todotxt_var
        .as_ref()
        .map(Path::new)
        .ok_or_else(|| anyhow::anyhow!("TODO_FILE environment variable is not set"))?;
    let done_var = env::var_os("DONE_FILE");
    let done_path = done_var
        .as_ref()
        .map(Path::new)
        .ok_or_else(|| anyhow::anyhow!("DONE_FILE environment variable is not set"))?;
    let today = today();
    let cl_args = cl::Action::parse();
    match cl_args {
        cl::Action::List => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let mut tasks = task_file.load_tasks()?;
            log::trace!("{tasks:#?}");
            let tasks2 = tasks.clone();
            tasks.sort_by(|a, b| b.cmp(a, &tasks2));
            for task in &tasks {
                println!(
                    "{}",
                    task.to_string(Some(&StyleContext {
                        today: &today,
                        other_tasks: &tasks
                    }))
                );
            }
        }
        cl::Action::Next { simple } => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let tasks = task_file.load_tasks()?;
            if let Some(task) = tasks
                .iter()
                .filter(|t| t.is_ready(today, &tasks))
                .max_by(|a, b| a.cmp(b, &tasks))
            {
                if simple {
                    println!(
                        "{}{}",
                        if let Some(priority) = task.priority {
                            format!("({priority}) ")
                        } else {
                            String::new()
                        },
                        task.text
                    );
                } else {
                    println!(
                        "{}",
                        task.to_string(Some(&StyleContext {
                            today: &today,
                            other_tasks: &tasks
                        }))
                    );
                }
            }
        }
        cl::Action::Add { args } => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let new_task = args.join(" ").parse()?;
            log::debug!("{new_task:?}");
            task_file.add_task(new_task, today)?;
        }
        cl::Action::Undo => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            task_file.undo_diff()?;
            let term = dialoguer::console::Term::stdout();
            if dialoguer::Confirm::new()
                .with_prompt("Apply above undo change?")
                .default(false)
                .interact_on(&term)?
            {
                task_file.undo()?;
            }
        }
        cl::Action::PendingCount => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let tasks = task_file.load_tasks()?;
            let pending = tasks.iter().filter(|t| t.is_pending(today)).count();
            println!("{pending}");
        }
        cl::Action::Report { days } => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let date_limit = today - chrono::Duration::days(i64::try_from(days)?);
            let mut tasks = task_file.filter_all(|t| match t.status {
                CreationCompletion::Pending { created } => {
                    created.is_some_and(|c| c >= date_limit) && t.recurrence().is_none()
                }
                CreationCompletion::Completed { created, completed } => {
                    created.is_some_and(|c| c >= date_limit) || (completed >= date_limit)
                }
            })?;
            tasks.sort_by_key(|t| t.completed_date().or_else(|| t.created_date()));
            let style_ctx = StyleContext {
                today: &today,
                other_tasks: &tasks,
            };
            for task in &tasks {
                println!("{}", task.to_string(Some(&style_ctx)));
            }
        }
        cl::Action::Menu { no_watch } => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            if no_watch {
                // Warning: console's themes do not support nesting styles
                let term = dialoguer::console::Term::stdout();
                let theme = dialoguer::theme::SimpleTheme;
                let mut fzf_builder = Fzf::builder();
                fzf_builder
                    .ansi(true)
                    .layout(fzf_wrapped::Layout::Reverse)
                    .no_mouse(true)
                    .no_scrollbar(true)
                    .custom_args(["--no-info", "--color=gutter:-1", "--with-nth=2..", "-d#"]);
                loop {
                    let mut tasks = task_file.load_tasks()?;
                    let tasks2 = tasks.clone();
                    tasks.sort_by(|a, b| b.cmp(a, &tasks2));

                    let style_ctx = StyleContext {
                        today: &today,
                        other_tasks: &tasks,
                    };
                    let fzf = fzf_builder.build()?;
                    let fzf_lines = &tasks
                        .iter()
                        .enumerate()
                        .map(|t| format!("{}#{}", t.0, t.1.to_string(Some(&style_ctx))))
                        .collect::<Vec<_>>();
                    let task_selection: Option<usize> =
                        fzf_wrapped::run_with_output(fzf, fzf_lines)
                            .and_then(|s| s.split_once('#').and_then(|t| t.0.parse().ok()));
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
                                    task_file.set_done(task.clone(), today)?;
                                }
                                Some(1) => {
                                    task_file.edit(task)?;
                                }
                                Some(2) => {
                                    task_file.start(task, today)?;
                                }
                                Some(_) => unreachable!(),
                                None => (),
                            }
                        }
                    }
                    term.clear_screen()?;
                }
            } else {
                const MAX_CHILD_AGE: Duration = Duration::from_secs(60 * 60);
                let (_txt_watcher, txt_event_rx) = task_file.watch()?;
                let current_exe = env::current_exe()?;
                let (_exe_watcher, exe_event_rx) = watch_current_exe(&current_exe)?;
                let mut has_exe_event = false;
                let child_args: Vec<_> = env::args()
                    .skip(1)
                    .chain(iter::once("--no-watch".to_owned()))
                    .collect();
                while !has_exe_event {
                    log::debug!("Spawning child");
                    let mut child = Command::new(&current_exe).args(&child_args).spawn()?;
                    let child_start = Instant::now();

                    let mut has_txt_event = false;
                    while !has_txt_event
                        && !has_exe_event
                        && Instant::now().duration_since(child_start) < MAX_CHILD_AGE
                    {
                        // Wait a bit to debounce events and detect child exit
                        const CHILD_WAIT_DURATION: Duration = Duration::from_millis(500);
                        if let Some(status) = child.wait_timeout(CHILD_WAIT_DURATION)? {
                            if !status.success() {
                                log::warn!("Child exited with code {status:?}");
                            }
                            return Ok(());
                        };

                        for () in txt_event_rx.try_iter() {
                            log::debug!("Received txt watcher event");
                            has_txt_event = true;
                        }
                        for () in exe_event_rx.try_iter() {
                            log::debug!("Received exe watcher event");
                            has_exe_event = true;
                        }
                    }

                    // Kill fzf child, which will cleanup term and bubble up termination to child
                    Command::new("pkill")
                        .args(["-INT", "-P", &child.id().to_string(), "-x", "fzf"])
                        .status()?;
                    child.wait()?;
                }

                // If we get here, restart current executable
                exec_self(&current_exe).context("Failed to exec self")?;
            }
        }
        cl::Action::Auto => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let mut tasks = task_file.load_tasks()?;
            let added_count = TodoFile::auto_recur(&mut tasks);
            let archived_count = task_file.auto_archive(&mut tasks, today)?;
            if (archived_count > 0) || (added_count > 0) {
                task_file.save_tasks(tasks)?;
            }
        }
    }

    Ok(())
}
