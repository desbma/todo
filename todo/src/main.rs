use std::{
    fs::OpenOptions,
    path::{Path, PathBuf},
};

use anyhow::Context as _;
use clap::Parser as _;
use deunicode::deunicode;
use itertools::Itertools as _;

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

/// A resolved source for multi-file menu mode
struct MenuSource {
    todo_file: TodoFile,
    display_tag: Option<String>,
}

/// Derive a deterministic slug from a source path for use as a synthetic `@tag`
fn source_slug(todo_path: &Path) -> Option<String> {
    // Use parent directory basename
    let raw = todo_path
        .parent()
        .and_then(Path::file_name)
        .map(|s| s.to_string_lossy().into_owned())?;

    // Transliterate Unicode to ASCII, then lowercase, replace non-alphanumeric with `-`
    let ascii = deunicode(&raw);
    let slug: String = ascii
        .to_ascii_lowercase()
        .chars()
        .map(|c: char| if c.is_ascii_alphanumeric() { c } else { '-' })
        .dedup_by(|a, b| *a == '-' && *b == '-')
        .collect();
    let slug = slug.trim_matches('-');

    (!slug.is_empty()).then(|| slug.to_owned())
}

const DEFAULT_DONE_FILENAME: &str = "done.txt";

/// Resolve multi-file menu sources from CLI paths
fn resolve_menu_sources(menu_files: &[PathBuf]) -> anyhow::Result<Vec<MenuSource>> {
    let mut sources = Vec::with_capacity(menu_files.len());
    for todo_path in menu_files {
        let canon = todo_path
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize menu source path: {todo_path:?}"))?;
        let parent = canon
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Cannot determine parent for {canon:?}"))?;
        let done_path = parent.join(DEFAULT_DONE_FILENAME);

        // Ensure done file exists
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(&done_path)
            .with_context(|| format!("Failed to create done file {done_path:?}"))?;

        let display_tag = source_slug(&canon);
        let todo_file = TodoFile::new(&canon, &done_path)?;
        sources.push(MenuSource {
            todo_file,
            display_tag,
        });
    }
    Ok(sources)
}

#[expect(clippy::too_many_lines)]
fn main() -> anyhow::Result<()> {
    // Parse CL args first so env-var requirements can depend on subcommand
    let cl_args = cl::Action::parse();
    let today = today();

    // Init logger — use Warn level for menu to avoid corrupting TUI output
    let default_log_level = if matches!(cl_args, cl::Action::Menu { .. }) {
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
            let task_file = TodoFile::from_env()?;
            let new_task = args.join(" ").parse()?;
            log::debug!("{new_task:?}");
            task_file.add_task(new_task, today)?;
        }
        cl::Action::Auto => {
            let task_file = TodoFile::from_env()?;
            let mut tasks = task_file.load_tasks()?;
            let added_count = TodoFile::auto_recur(&mut tasks);
            let archived_count = task_file.auto_archive(&mut tasks, today)?;
            if (archived_count > 0) || (added_count > 0) {
                task_file.save_tasks(tasks)?;
            }
        }
        cl::Action::List => {
            let task_file = TodoFile::from_env()?;
            let mut tasks = task_file.load_tasks()?;
            log::trace!("{tasks:#?}");
            let tasks2 = tasks.clone();
            tasks.sort_by(|a, b| b.cmp(a, &tasks2));
            for task in &tasks {
                print_styled_task(task, today, &tasks);
            }
        }
        cl::Action::Menu { files } => {
            if files.is_empty() {
                let task_file = TodoFile::from_env()?;
                menu::run(vec![menu::MenuSource::new(task_file, None)], today)?;
            } else {
                let sources = resolve_menu_sources(&files)?;
                let menu_sources: Vec<menu::MenuSource> = sources
                    .into_iter()
                    .map(|s| menu::MenuSource::new(s.todo_file, s.display_tag))
                    .collect();
                menu::run(menu_sources, today)?;
            }
        }
        cl::Action::Next { simple } => {
            let task_file = TodoFile::from_env()?;
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
            let task_file = TodoFile::from_env()?;
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
            let task_file = TodoFile::from_env()?;
            let tasks = task_file.load_tasks()?;
            let pending = tasks.iter().filter(|t| t.is_pending(today)).count();
            println!("{pending}");
        }
        cl::Action::Report { days } => {
            let task_file = TodoFile::from_env()?;
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
            let task_file = TodoFile::from_env()?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slug_from_parent_dir() {
        assert_eq!(
            source_slug(Path::new("/home/user/project-a/todo.txt")),
            Some("project-a".to_owned())
        );
    }

    #[test]
    fn slug_lowercase_and_strip_special() {
        assert_eq!(
            source_slug(Path::new("/home/user/My Projects!/todo.txt")),
            Some("my-projects".to_owned())
        );
    }

    #[test]
    fn slug_no_parent_dir() {
        assert_eq!(source_slug(Path::new("/tasks.txt")), None);
    }

    #[test]
    fn slug_unicode_transliterated() {
        assert_eq!(
            source_slug(Path::new("/home/café/todo.txt")),
            Some("cafe".to_owned())
        );
    }

    #[test]
    fn slug_empty_returns_none() {
        assert_eq!(source_slug(Path::new("////")), None);
    }

    #[test]
    fn slug_spaces_and_punctuation() {
        assert_eq!(
            source_slug(Path::new("/home/a  b--c/todo.txt")),
            Some("a-b-c".to_owned())
        );
    }

    #[test]
    fn slug_dedup() {
        assert_eq!(
            source_slug(Path::new("/home/aaaa  bb--ccc--c/todo.txt")),
            Some("aaaa-bb-ccc-c".to_owned())
        );
    }
}
