use std::{env, path::Path};

use anyhow::Context as _;
use clap::Parser as _;

mod cl;
mod menu;

use tasks::{CreationCompletion, Date, TodoFile};

fn today() -> Date {
    chrono::Local::now().date_naive()
}

fn print_styled_task(task: &tasks::Task, today: Date, all_tasks: &[tasks::Task]) {
    let line = menu::styled_task_line(task, today, all_tasks);
    println!("{}", menu::line_to_ansi(&line));
}

#[expect(clippy::too_many_lines)]
fn main() -> anyhow::Result<()> {
    // Parse CL args first to decide log level
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

    // Init logger — use Warn level for menu to avoid corrupting TUI output
    let default_log_level = if matches!(cl_args, cl::Action::Menu) {
        log::LevelFilter::Warn
    } else if cfg!(debug_assertions) {
        log::LevelFilter::Debug
    } else {
        log::LevelFilter::Info
    };
    simple_logger::SimpleLogger::new()
        .with_level(default_log_level)
        .env()
        .init()
        .context("Failed to setup logger")?;

    match cl_args {
        cl::Action::Add { args } => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let new_task = args.join(" ").parse()?;
            log::debug!("{new_task:?}");
            task_file.add_task(new_task, today)?;
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
        cl::Action::List => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let mut tasks = task_file.load_tasks()?;
            log::trace!("{tasks:#?}");
            let tasks2 = tasks.clone();
            tasks.sort_by(|a, b| b.cmp(a, &tasks2));
            for task in &tasks {
                print_styled_task(task, today, &tasks);
            }
        }
        cl::Action::Menu => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            menu::run(&task_file, today)?;
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
                    print_styled_task(task, today, &tasks);
                }
            }
        }
        cl::Action::NotifyOverdue => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            let tasks = task_file.load_tasks()?;
            let mut overdue_tasks: Vec<_> = tasks
                .iter()
                .filter(|t| t.is_overdue(today) && t.is_ready(today, &tasks))
                .collect();
            if !overdue_tasks.is_empty() {
                overdue_tasks.sort_by(|a, b| b.cmp(a, &tasks));
                let body = overdue_tasks
                    .iter()
                    .map(|t| t.text.as_str())
                    .collect::<Vec<_>>()
                    .join("\n");
                notify_rust::Notification::new()
                    .summary(&format!(
                        "{}: {} overdue task(s)",
                        env!("CARGO_PKG_NAME"),
                        overdue_tasks.len(),
                    ))
                    .body(&body)
                    .icon("task-past-due")
                    .show()?;
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
            for task in &tasks {
                print_styled_task(task, today, &tasks);
            }
        }
        cl::Action::Undo => {
            let task_file = TodoFile::new(todotxt_path, done_path)?;
            task_file.undo_diff()?;
            eprint!("Apply above undo change? [y/N] ");
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            if input.trim().eq_ignore_ascii_case("y") {
                task_file.undo()?;
            }
        }
    }

    Ok(())
}
