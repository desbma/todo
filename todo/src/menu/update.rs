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
    ExeFileChanged,
    Tick,
}

const RELOAD_DEBOUNCE: Duration = Duration::from_millis(200);

/// Side effects produced by state transitions
#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub(crate) enum Effect {
    None,
    ReloadTasks,
    PerformAction(TaskAction),
    ExecSelf,
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
            if !app.query.is_empty() {
                app.query.clear();
                app.refilter();
                Effect::None
            } else if app.clear_tab_filter() {
                Effect::None
            } else {
                app.should_quit = true;
                Effect::Quit
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
        KeyCode::PageUp => {
            let page = i32::from(app.list_height).max(1);
            move_selection(app, -page);
            Effect::None
        }
        KeyCode::PageDown => {
            let page = i32::from(app.list_height).max(1);
            move_selection(app, page);
            Effect::None
        }
        KeyCode::Tab => {
            app.select_next_tab();
            Effect::None
        }
        KeyCode::BackTab => {
            app.select_prev_tab();
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

/// Handle an exe-file-changed notification: schedule a debounced re-exec
pub(crate) fn handle_exe_changed(app: &mut App) {
    app.pending_exe_reload_at = Some(Instant::now() + RELOAD_DEBOUNCE);
}

/// Handle a tick: check if a pending reload is due
pub(crate) fn handle_tick(app: &mut App) -> Effect {
    // Check exe reload first — takes priority
    if let Some(deadline) = app.pending_exe_reload_at {
        if Instant::now() >= deadline {
            app.pending_exe_reload_at = None;
            return Effect::ExecSelf;
        }
    }

    if let Some(deadline) = app.pending_reload_at {
        if Instant::now() >= deadline {
            app.pending_reload_at = None;
            return Effect::ReloadTasks;
        }
    }

    // Clear expired toast
    app.expire_toast();

    // Check for date rollover
    let new_today = chrono::Local::now().date_naive();
    if new_today != app.today {
        app.today = new_today;
        return Effect::ReloadTasks;
    }

    Effect::None
}

#[cfg(test)]
mod tests {
    use std::{io::Write as _, rc::Rc, time::Instant};

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use tasks::TodoFile;

    use super::*;
    use crate::menu::state::{MenuSource, MenuTask, Mode, TaskAction};

    fn today() -> tasks::Date {
        chrono::NaiveDate::from_ymd_opt(2026, 3, 18).unwrap()
    }

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn make_source() -> Rc<MenuSource> {
        let mut todo = tempfile::NamedTempFile::new().unwrap();
        let mut done = tempfile::NamedTempFile::new().unwrap();
        writeln!(todo, "placeholder").unwrap();
        writeln!(done, "placeholder").unwrap();
        Rc::new(MenuSource::new(
            TodoFile::new(todo.path(), done.path()).unwrap(),
            None,
        ))
    }

    fn make_app(lines: &[&str]) -> App {
        let source = make_source();
        let tasks = lines
            .iter()
            .map(|l| MenuTask {
                task: l.parse().unwrap(),
                source: Rc::clone(&source),
            })
            .collect();
        App::new(tasks, today(), false)
    }

    #[test]
    fn esc_empty_query_quits() {
        let mut app = make_app(&["Buy milk"]);
        let effect = handle_key(&mut app, key(KeyCode::Esc));
        assert_eq!(effect, Effect::Quit);
        assert!(app.should_quit);
    }

    #[test]
    fn esc_nonempty_query_clears() {
        let mut app = make_app(&["Buy milk"]);
        app.query = "milk".to_owned();
        app.refilter();

        let effect = handle_key(&mut app, key(KeyCode::Esc));
        assert_eq!(effect, Effect::None);
        assert!(app.query.is_empty());
    }

    #[test]
    fn esc_with_selected_tab_clears_tab_before_quitting() {
        let mut app = make_app(&["@home Buy milk", "@work Walk dog"]);
        app.select_next_tab();

        let effect = handle_key(&mut app, key(KeyCode::Esc));
        assert_eq!(effect, Effect::None);
        assert_eq!(app.active_tab(), None);
        assert!(!app.should_quit);
    }

    #[test]
    fn ctrl_c_quits() {
        let mut app = make_app(&["Buy milk"]);
        let effect = handle_key(&mut app, ctrl('c'));
        assert_eq!(effect, Effect::Quit);
        assert!(app.should_quit);
    }

    #[test]
    fn enter_with_selection_opens_popup() {
        let mut app = make_app(&["Buy milk"]);
        let effect = handle_key(&mut app, key(KeyCode::Enter));
        assert_eq!(effect, Effect::None);
        assert_eq!(app.mode, Mode::ActionPopup);
        assert_eq!(app.action_index, 0);
    }

    #[test]
    fn enter_without_selection_noop() {
        let mut app = make_app(&[]);
        let effect = handle_key(&mut app, key(KeyCode::Enter));
        assert_eq!(effect, Effect::None);
        assert_eq!(app.mode, Mode::Normal);
    }

    #[test]
    fn down_moves_selection() {
        let mut app = make_app(&["Task A", "Task B", "Task C"]);
        assert_eq!(app.list_state.selected(), Some(0));

        handle_key(&mut app, key(KeyCode::Down));
        assert_eq!(app.list_state.selected(), Some(1));
    }

    #[test]
    fn up_at_top_stays() {
        let mut app = make_app(&["Task A", "Task B"]);
        assert_eq!(app.list_state.selected(), Some(0));

        handle_key(&mut app, key(KeyCode::Up));
        assert_eq!(app.list_state.selected(), Some(0));
    }

    #[test]
    fn j_moves_down() {
        let mut app = make_app(&["Task A", "Task B"]);
        handle_key(&mut app, key(KeyCode::Char('j')));
        assert_eq!(app.list_state.selected(), Some(1));
    }

    #[test]
    fn k_moves_up() {
        let mut app = make_app(&["Task A", "Task B"]);
        handle_key(&mut app, key(KeyCode::Down));
        handle_key(&mut app, key(KeyCode::Char('k')));
        assert_eq!(app.list_state.selected(), Some(0));
    }

    #[test]
    fn home_jumps_to_first() {
        let mut app = make_app(&["Task A", "Task B", "Task C"]);
        handle_key(&mut app, key(KeyCode::End));
        handle_key(&mut app, key(KeyCode::Home));
        assert_eq!(app.list_state.selected(), Some(0));
    }

    #[test]
    fn end_jumps_to_last() {
        let mut app = make_app(&["Task A", "Task B", "Task C"]);
        handle_key(&mut app, key(KeyCode::End));
        assert_eq!(app.list_state.selected(), Some(2));
    }

    #[test]
    fn pagedown_moves_by_page() {
        let mut app = make_app(&[
            "Task A", "Task B", "Task C", "Task D", "Task E", "Task F", "Task G", "Task H",
        ]);
        app.list_height = 3;
        handle_key(&mut app, key(KeyCode::PageDown));
        assert_eq!(app.list_state.selected(), Some(3));
    }

    #[test]
    fn pageup_moves_by_page() {
        let mut app = make_app(&[
            "Task A", "Task B", "Task C", "Task D", "Task E", "Task F", "Task G", "Task H",
        ]);
        app.list_height = 3;
        app.list_state.select(Some(5));
        handle_key(&mut app, key(KeyCode::PageUp));
        assert_eq!(app.list_state.selected(), Some(2));
    }

    #[test]
    fn pagedown_clamps_at_end() {
        let mut app = make_app(&["Task A", "Task B", "Task C"]);
        app.list_height = 10;
        handle_key(&mut app, key(KeyCode::PageDown));
        assert_eq!(app.list_state.selected(), Some(2));
    }

    #[test]
    fn pageup_clamps_at_start() {
        let mut app = make_app(&["Task A", "Task B", "Task C"]);
        app.list_height = 10;
        app.list_state.select(Some(1));
        handle_key(&mut app, key(KeyCode::PageUp));
        assert_eq!(app.list_state.selected(), Some(0));
    }

    #[test]
    fn tab_selects_next_tab() {
        let mut app = make_app(&["@home Buy milk", "@work Walk dog"]);
        handle_key(&mut app, key(KeyCode::Tab));
        assert_eq!(app.active_tab(), Some("home"));
    }

    #[test]
    fn backtab_selects_previous_tab() {
        let mut app = make_app(&["@home Buy milk", "@work Walk dog"]);
        handle_key(&mut app, key(KeyCode::BackTab));
        assert_eq!(app.active_tab(), Some("work"));
    }

    #[test]
    fn tab_wraps_back_to_all() {
        let mut app = make_app(&["@home Buy milk"]);
        handle_key(&mut app, key(KeyCode::Tab));
        assert_eq!(app.active_tab(), Some("home"));

        handle_key(&mut app, key(KeyCode::Tab));
        assert_eq!(app.active_tab(), None);
    }

    #[test]
    fn char_appends_to_query() {
        let mut app = make_app(&["Buy milk"]);
        handle_key(&mut app, key(KeyCode::Char('m')));
        assert_eq!(app.query, "m");
    }

    #[test]
    fn backspace_pops_from_query() {
        let mut app = make_app(&["Buy milk"]);
        app.query = "mi".to_owned();
        handle_key(&mut app, key(KeyCode::Backspace));
        assert_eq!(app.query, "m");
    }

    #[test]
    fn popup_esc_returns_to_normal() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;

        let effect = handle_key(&mut app, key(KeyCode::Esc));
        assert_eq!(effect, Effect::None);
        assert_eq!(app.mode, Mode::Normal);
    }

    #[test]
    fn popup_up_down_moves_action_index() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;
        app.action_index = 0;

        handle_key(&mut app, key(KeyCode::Down));
        assert_eq!(app.action_index, 1);

        handle_key(&mut app, key(KeyCode::Up));
        assert_eq!(app.action_index, 0);
    }

    #[test]
    fn popup_up_at_zero_stays() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;
        app.action_index = 0;

        handle_key(&mut app, key(KeyCode::Up));
        assert_eq!(app.action_index, 0);
    }

    #[test]
    fn popup_down_clamps_at_max() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;
        app.action_index = TaskAction::COUNT - 1;

        handle_key(&mut app, key(KeyCode::Down));
        assert_eq!(app.action_index, TaskAction::COUNT - 1);
    }

    #[test]
    fn popup_enter_performs_action() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;
        app.action_index = 1; // Edit

        let effect = handle_key(&mut app, key(KeyCode::Enter));
        assert_eq!(effect, Effect::PerformAction(TaskAction::Edit));
        assert_eq!(app.mode, Mode::Normal);
    }

    #[test]
    fn popup_ctrl_c_quits() {
        let mut app = make_app(&["Buy milk"]);
        app.mode = Mode::ActionPopup;

        let effect = handle_key(&mut app, ctrl('c'));
        assert_eq!(effect, Effect::Quit);
    }

    #[test]
    fn popup_tab_does_not_change_tab_filter() {
        let mut app = make_app(&["@home Buy milk", "@work Walk dog"]);
        app.mode = Mode::ActionPopup;

        handle_key(&mut app, key(KeyCode::Tab));
        assert_eq!(app.active_tab(), None);
    }

    #[test]
    fn file_changed_inserts_sources_and_sets_deadline() {
        let mut app = make_app(&["Buy milk"]);
        assert!(app.pending_reload_at.is_none());

        handle_file_changed(&mut app, vec![0, 2]);
        assert!(app.pending_reload_at.is_some());
        assert!(app.pending_reload_sources.contains(&0));
        assert!(app.pending_reload_sources.contains(&2));
    }

    #[test]
    fn tick_past_deadline_reloads() {
        let mut app = make_app(&["Buy milk"]);
        app.pending_reload_at = Instant::now().checked_sub(Duration::from_millis(1));

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::ReloadTasks);
        assert!(app.pending_reload_at.is_none());
    }

    #[test]
    fn tick_before_deadline_no_reload() {
        let mut app = make_app(&["Buy milk"]);
        app.pending_reload_at = Some(Instant::now() + Duration::from_secs(60));
        // Prevent the date-rollover branch from firing.
        app.today = chrono::Local::now().date_naive();

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::None);
        assert!(app.pending_reload_at.is_some());
    }

    #[test]
    fn exe_changed_sets_deadline() {
        let mut app = make_app(&["Buy milk"]);
        assert!(app.pending_exe_reload_at.is_none());

        handle_exe_changed(&mut app);
        assert!(app.pending_exe_reload_at.is_some());
    }

    #[test]
    fn tick_past_exe_deadline_exec_self() {
        let mut app = make_app(&["Buy milk"]);
        app.pending_exe_reload_at = Instant::now().checked_sub(Duration::from_millis(1));

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::ExecSelf);
        assert!(app.pending_exe_reload_at.is_none());
    }

    #[test]
    fn tick_before_exe_deadline_no_exec() {
        let mut app = make_app(&["Buy milk"]);
        app.pending_exe_reload_at = Some(Instant::now() + Duration::from_secs(60));
        // Prevent the date-rollover branch from firing.
        app.today = chrono::Local::now().date_naive();

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::None);
        assert!(app.pending_exe_reload_at.is_some());
    }

    #[test]
    fn exe_reload_takes_priority_over_task_reload() {
        let mut app = make_app(&["Buy milk"]);
        app.pending_exe_reload_at = Instant::now().checked_sub(Duration::from_millis(1));
        app.pending_reload_at = Instant::now().checked_sub(Duration::from_millis(1));

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::ExecSelf);
    }

    #[test]
    fn tick_clears_expired_toast() {
        let mut app = make_app(&["Buy milk"]);
        app.toast = Some((
            "hello".to_owned(),
            Instant::now().checked_sub(Duration::from_secs(1)).unwrap(),
        ));
        // Prevent the date-rollover branch from firing.
        app.today = chrono::Local::now().date_naive();

        let effect = handle_tick(&mut app);
        assert_eq!(effect, Effect::None);
        assert!(app.toast.is_none());
    }
}
