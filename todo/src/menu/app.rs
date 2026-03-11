//! Runtime loop wiring terminal, file watcher, and task I/O

use std::{io, sync::mpsc, time::Duration};

use anyhow::Context as _;
use crossterm::{
    event::{self, Event},
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{Terminal, backend::CrosstermBackend};
use tasks::TodoFile;

use super::{
    render,
    state::{App, TaskAction},
    update::{self, Effect, Msg},
};

const POLL_TIMEOUT: Duration = Duration::from_millis(250);

/// Run the interactive menu TUI
pub(crate) fn run(task_file: &TodoFile, today: tasks::Date) -> anyhow::Result<()> {
    // Load and sort tasks
    let mut tasks = task_file.load_tasks()?;
    let tasks_clone = tasks.clone();
    tasks.sort_by(|a, b| b.cmp(a, &tasks_clone));

    let mut app = App::new(tasks, today);

    // Set up file watcher
    let (_watcher, file_event_rx) = task_file.watch()?;

    // Set up terminal
    terminal::enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let result = run_loop(&mut terminal, &mut app, task_file, &file_event_rx);

    // Restore terminal
    terminal::disable_raw_mode()?;
    crossterm::execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    result
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
    task_file: &TodoFile,
    file_event_rx: &mpsc::Receiver<()>,
) -> anyhow::Result<()> {
    loop {
        terminal.draw(|frame| render::draw(frame, app))?;

        let msg = next_msg(file_event_rx)?;
        let effect = match msg {
            Msg::Key(key) => update::handle_key(app, key),
            Msg::Resize => Effect::None, // redraw happens at top of loop
            Msg::TasksFileChanged => {
                update::handle_file_changed(app);
                Effect::None
            }
            Msg::Tick => update::handle_tick(app),
        };

        match effect {
            Effect::None => {}
            Effect::ReloadTasks => {
                let tasks = task_file.load_tasks()?;
                app.reload_tasks(tasks);
            }
            Effect::PerformAction(action) => {
                run_action(terminal, app, task_file, action)?;
            }
            Effect::Quit => break,
        }

        if app.should_quit {
            break;
        }
    }
    Ok(())
}

fn next_msg(file_event_rx: &mpsc::Receiver<()>) -> anyhow::Result<Msg> {
    // Check watcher events first (non-blocking)
    if file_event_rx.try_recv().is_ok() {
        // Drain any remaining
        while file_event_rx.try_recv().is_ok() {}
        return Ok(Msg::TasksFileChanged);
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
    task_file: &TodoFile,
    action: TaskAction,
) -> anyhow::Result<()> {
    let Some(task) = app.selected_task().cloned() else {
        return Ok(());
    };

    match action {
        TaskAction::MarkDone => {
            task_file.set_done(task, app.today)?;
            app.status_message = Some("Task marked as done".to_owned());
        }
        TaskAction::Edit => {
            // Leave TUI for external editor
            terminal::disable_raw_mode()?;
            crossterm::execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
            terminal.show_cursor()?;

            task_file.edit(&task)?;

            // Re-enter TUI
            terminal::enable_raw_mode()?;
            crossterm::execute!(terminal.backend_mut(), EnterAlternateScreen)?;
            terminal.clear()?;
        }
        TaskAction::Start => {
            task_file.start(&task, app.today)?;
            app.status_message = Some("Task started".to_owned());
        }
    }

    // Reload after any action
    let tasks = task_file.load_tasks()?;
    app.reload_tasks(tasks);
    app.status_message = None;

    Ok(())
}
