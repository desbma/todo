//! Pure state transitions for the menu TUI

use std::time::{Duration, Instant};

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use strum::{EnumCount as _, IntoEnumIterator as _};

use super::state::{App, Mode, TaskAction};

/// Domain events processed by the update loop
pub(crate) enum Msg {
    Key(KeyEvent),
    Resize,
    TasksFileChanged(Vec<usize>),
    Tick,
}

const RELOAD_DEBOUNCE: Duration = Duration::from_millis(200);

/// Side effects produced by state transitions
pub(crate) enum Effect {
    None,
    ReloadTasks,
    PerformAction(TaskAction),
    Quit,
}

/// Process a key event and return the resulting effect
pub(crate) fn handle_key(app: &mut App, key: KeyEvent) -> Effect {
    match app.mode {
        Mode::Normal => handle_key_normal(app, key),
        Mode::ActionPopup => handle_key_popup(app, key),
    }
}

fn handle_key_normal(app: &mut App, key: KeyEvent) -> Effect {
    match key.code {
        KeyCode::Esc => {
            if app.query.is_empty() {
                app.should_quit = true;
                Effect::Quit
            } else {
                app.query.clear();
                app.refilter();
                Effect::None
            }
        }
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
            app.should_quit = true;
            Effect::Quit
        }
        KeyCode::Enter => {
            if app.selected_task().is_some() {
                app.mode = Mode::ActionPopup;
                app.action_index = 0;
            }
            Effect::None
        }
        KeyCode::Up | KeyCode::Char('k')
            if !key.modifiers.contains(KeyModifiers::CONTROL) || key.code == KeyCode::Up =>
        {
            move_selection(app, -1);
            Effect::None
        }
        KeyCode::Down | KeyCode::Char('j')
            if !key.modifiers.contains(KeyModifiers::CONTROL) || key.code == KeyCode::Down =>
        {
            move_selection(app, 1);
            Effect::None
        }
        KeyCode::Home => {
            if !app.visible.is_empty() {
                app.list_state.select(Some(0));
            }
            Effect::None
        }
        KeyCode::End => {
            if !app.visible.is_empty() {
                app.list_state.select(Some(app.visible.len() - 1));
            }
            Effect::None
        }
        KeyCode::Char(c) => {
            app.query.push(c);
            app.refilter();
            Effect::None
        }
        KeyCode::Backspace => {
            app.query.pop();
            app.refilter();
            Effect::None
        }
        _ => Effect::None,
    }
}

fn handle_key_popup(app: &mut App, key: KeyEvent) -> Effect {
    match key.code {
        KeyCode::Esc => {
            app.mode = Mode::Normal;
            Effect::None
        }
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
            app.should_quit = true;
            Effect::Quit
        }
        KeyCode::Up | KeyCode::Char('k') => {
            if app.action_index > 0 {
                app.action_index -= 1;
            }
            Effect::None
        }
        KeyCode::Down | KeyCode::Char('j') => {
            if app.action_index < TaskAction::COUNT - 1 {
                app.action_index += 1;
            }
            Effect::None
        }
        KeyCode::Enter => {
            app.mode = Mode::Normal;
            if let Some(action) = TaskAction::iter().nth(app.action_index) {
                Effect::PerformAction(action)
            } else {
                Effect::None
            }
        }
        _ => Effect::None,
    }
}

fn move_selection(app: &mut App, delta: i32) {
    if app.visible.is_empty() {
        return;
    }
    let current = app.list_state.selected().unwrap_or(0);
    #[expect(clippy::cast_sign_loss)]
    let new = if delta < 0 {
        current.saturating_sub(delta.unsigned_abs() as usize)
    } else {
        (current + delta as usize).min(app.visible.len() - 1)
    };
    app.list_state.select(Some(new));
}

/// Handle a file-changed notification: schedule a debounced reload
pub(crate) fn handle_file_changed(app: &mut App, changed_sources: Vec<usize>) {
    for idx in changed_sources {
        app.pending_reload_sources.insert(idx);
    }
    app.pending_reload_at = Some(Instant::now() + RELOAD_DEBOUNCE);
}

/// Handle a tick: check if a pending reload is due
pub(crate) fn handle_tick(app: &mut App) -> Effect {
    if let Some(deadline) = app.pending_reload_at {
        if Instant::now() >= deadline {
            app.pending_reload_at = None;
            return Effect::ReloadTasks;
        }
    }

    // Check for date rollover
    let new_today = chrono::Local::now().date_naive();
    if new_today != app.today {
        app.today = new_today;
        return Effect::ReloadTasks;
    }

    Effect::None
}
