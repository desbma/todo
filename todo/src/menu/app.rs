//! Runtime loop wiring terminal, file watcher, and task I/O

use std::{
    env, io, os::unix::process::CommandExt as _, path::Path, process::Command, rc::Rc, sync::mpsc,
    time::Duration,
};

use anyhow::Context as _;
use crossterm::{
    event::{self, Event},
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use notify::Watcher as _;
use ratatui::{Terminal, backend::CrosstermBackend};

use super::{
    render,
    state::{App, MenuSource, MenuTask, TaskAction},
    update::{self, Effect, Msg},
};

const POLL_TIMEOUT: Duration = Duration::from_millis(250);

/// Load tasks from all sources
fn load_all_tasks(sources: &[Rc<MenuSource>]) -> anyhow::Result<Vec<MenuTask>> {
    let mut all = Vec::new();
    for source in sources {
        let tasks = source.todo_file.load_tasks()?;
        for task in tasks {
            all.push(MenuTask {
                task,
                source: Rc::clone(source),
            });
        }
    }
    Ok(all)
}

/// Watch the current executable for replacement, returning a watcher and a notification receiver
fn watch_exe(exe_path: &Path) -> anyhow::Result<(Box<dyn notify::Watcher>, mpsc::Receiver<()>)> {
    let (event_tx, event_rx) = mpsc::channel();
    let parent_dir = exe_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Path {exe_path:?} has no parent"))?;
    let exe_path = exe_path.to_owned();
    let mut watcher = Box::new(notify::recommended_watcher(
        move |evt: notify::Result<notify::Event>| {
            log::debug!("Exe watcher event {evt:?}");
            if let Ok(evt) = evt {
                match evt.kind {
                    notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_) => {
                        if evt.paths.contains(&exe_path) {
                            let _ = event_tx.send(());
                        }
                    }
                    _ if evt.need_rescan() => {
                        let _ = event_tx.send(());
                    }
                    _ => {}
                }
            }
        },
    )?);
    watcher.watch(parent_dir, notify::RecursiveMode::NonRecursive)?;
    Ok((watcher, event_rx))
}

/// Replace the current process with a fresh instance of the same executable
fn exec_self(current_exe: &Path) -> anyhow::Result<()> {
    let err = Command::new(current_exe)
        .args(env::args_os().skip(1))
        .exec();
    Err(err.into())
}

/// Run the interactive menu TUI
pub(crate) fn run(sources: Vec<MenuSource>, today: tasks::Date) -> anyhow::Result<()> {
    let multi_source = sources.len() > 1;
    let rc_sources: Vec<Rc<MenuSource>> = sources.into_iter().map(Rc::new).collect();

    // Load and sort tasks
    let mut tasks = load_all_tasks(&rc_sources)?;
    let all_plain: Vec<_> = tasks.iter().map(|mt| mt.task.clone()).collect();
    tasks.sort_by(|a, b| b.task.cmp(&a.task, &all_plain));

    let mut app = App::new(tasks, today, multi_source);

    // Set up file watchers (one per source)
    let mut watchers = Vec::new();
    let mut file_event_rxs = Vec::new();
    for source in &rc_sources {
        let (watcher, rx) = source.todo_file.watch()?;
        watchers.push(watcher);
        file_event_rxs.push(rx);
    }

    // Set up exe watcher
    let current_exe = env::current_exe().context("Failed to get current executable path")?;
    let (_exe_watcher, exe_event_rx) = watch_exe(&current_exe)?;

    // Set up terminal
    terminal::enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let result = run_loop(
        &mut terminal,
        &mut app,
        &rc_sources,
        &file_event_rxs,
        &exe_event_rx,
    );

    // Restore terminal
    terminal::disable_raw_mode()?;
    crossterm::execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    match result {
        Ok(ExitReason::Quit) => Ok(()),
        Ok(ExitReason::ExecSelf) => exec_self(&current_exe),
        Err(e) => Err(e),
    }
}

/// Why the run loop exited
enum ExitReason {
    Quit,
    ExecSelf,
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
    sources: &[Rc<MenuSource>],
    file_event_rxs: &[crossbeam_channel::Receiver<()>],
    exe_event_rx: &mpsc::Receiver<()>,
) -> anyhow::Result<ExitReason> {
    loop {
        terminal.draw(|frame| render::draw(frame, app))?;

        let msg = next_msg(file_event_rxs, exe_event_rx)?;
        let effect = match msg {
            Msg::Key(key) => update::handle_key(app, key),
            Msg::Resize => Effect::None, // redraw happens at top of loop
            Msg::TasksFileChanged(changed) => {
                update::handle_file_changed(app, changed);
                Effect::None
            }
            Msg::ExeFileChanged => {
                update::handle_exe_changed(app);
                Effect::None
            }
            Msg::Tick => update::handle_tick(app),
        };

        match effect {
            Effect::None => {}
            Effect::ReloadTasks => {
                if !app.pending_reload_sources.is_empty() {
                    let paths: Vec<_> = app
                        .pending_reload_sources
                        .drain()
                        .filter_map(|i| sources.get(i))
                        .map(|s| format!("{:?}", s.todo_file.path()))
                        .collect();
                    if !paths.is_empty() {
                        app.set_toast(format!("{} reloaded", paths.join(", ")));
                    }
                }
                let tasks = load_all_tasks(sources)?;
                app.reload_tasks(tasks);
            }
            Effect::PerformAction(action) => {
                run_action(terminal, app, sources, action)?;
            }
            Effect::ExecSelf => return Ok(ExitReason::ExecSelf),
            Effect::Quit => return Ok(ExitReason::Quit),
        }

        if app.should_quit {
            return Ok(ExitReason::Quit);
        }
    }
}

fn next_msg(
    file_event_rxs: &[crossbeam_channel::Receiver<()>],
    exe_event_rx: &mpsc::Receiver<()>,
) -> anyhow::Result<Msg> {
    // Check exe watcher events first (non-blocking)
    if exe_event_rx.try_recv().is_ok() {
        return Ok(Msg::ExeFileChanged);
    }

    // Check file watcher events (non-blocking), tracking which sources changed
    let mut changed = Vec::new();
    for (i, rx) in file_event_rxs.iter().enumerate() {
        if rx.try_recv().is_ok() {
            changed.push(i);
        }
    }
    if !changed.is_empty() {
        return Ok(Msg::TasksFileChanged(changed));
    }

    // Poll terminal events
    if event::poll(POLL_TIMEOUT).context("Failed to poll events")? {
        match event::read().context("Failed to read event")? {
            Event::Key(key) => return Ok(Msg::Key(key)),
            Event::Resize(_, _) => return Ok(Msg::Resize),
            _ => {}
        }
    }

    Ok(Msg::Tick)
}

fn run_action(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
    sources: &[Rc<MenuSource>],
    action: TaskAction,
) -> anyhow::Result<()> {
    let Some(menu_task) = app.selected_menu_task() else {
        return Ok(());
    };
    let task = menu_task.task.clone();
    let source = Rc::clone(&menu_task.source);
    let source_path = source.todo_file.path();

    match action {
        TaskAction::MarkDone => {
            source.todo_file.set_done(task, app.today)?;
            app.set_toast(format!("Task marked as done ({source_path:?})"));
        }
        TaskAction::Edit => {
            // Leave TUI for external editor
            terminal::disable_raw_mode()?;
            crossterm::execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
            terminal.show_cursor()?;

            source.todo_file.edit(&task)?;

            // Re-enter TUI
            terminal::enable_raw_mode()?;
            crossterm::execute!(terminal.backend_mut(), EnterAlternateScreen)?;
            terminal.clear()?;
        }
        TaskAction::Start => {
            source.todo_file.start(&task, app.today)?;
            app.set_toast(format!("Task started ({source_path:?})"));
        }
    }

    // Reload all sources after any action
    let tasks = load_all_tasks(sources)?;
    app.reload_tasks(tasks);

    Ok(())
}
