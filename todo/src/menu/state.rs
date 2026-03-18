//! Menu TUI state

use std::{
    collections::HashSet,
    rc::Rc,
    time::{Duration, Instant},
};

use ratatui::widgets::ListState;
use tasks::{Date, Task, TodoFile};

const TOAST_DURATION: Duration = Duration::from_secs(5);

/// Active interaction mode
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum Mode {
    /// Browsing/filtering the task list
    Normal,
    /// Action popup is open for the selected task
    ActionPopup,
}

/// Actions available in the popup
#[derive(Debug, Clone, Copy, Eq, PartialEq, strum::EnumIter, strum::EnumCount)]
pub(crate) enum TaskAction {
    MarkDone,
    Edit,
    Start,
}

impl TaskAction {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::MarkDone => "Mark as done",
            Self::Edit => "Edit",
            Self::Start => "Start",
        }
    }
}

/// A source todo file with display metadata
pub(crate) struct MenuSource {
    pub todo_file: TodoFile,
    pub display_tag: Option<String>,
}

impl MenuSource {
    #[must_use]
    pub(crate) fn new(todo_file: TodoFile, display_tag: Option<String>) -> Self {
        Self {
            todo_file,
            display_tag,
        }
    }
}

/// A task annotated with its owning source
pub(crate) struct MenuTask {
    pub task: Task,
    pub source: Rc<MenuSource>,
}

pub(crate) struct App {
    pub tasks: Vec<MenuTask>,
    pub visible: Vec<usize>,
    pub query: String,
    pub list_state: ListState,
    pub mode: Mode,
    pub action_index: usize,
    pub today: Date,
    pub should_quit: bool,
    pub pending_reload_at: Option<Instant>,
    /// Source indices that changed since the last reload
    pub pending_reload_sources: HashSet<usize>,
    /// Transient toast notification (message, expiry time)
    pub toast: Option<(String, Instant)>,
    /// Whether we are operating in multi-source mode
    pub multi_source: bool,
}

impl App {
    pub(crate) fn new(tasks: Vec<MenuTask>, today: Date, multi_source: bool) -> Self {
        let mut app = Self {
            tasks,
            visible: Vec::new(),
            query: String::new(),
            list_state: ListState::default(),
            mode: Mode::Normal,
            action_index: 0,
            today,
            should_quit: false,
            pending_reload_at: None,
            pending_reload_sources: HashSet::new(),
            toast: None,
            multi_source,
        };
        app.refilter();
        if !app.visible.is_empty() {
            app.list_state.select(Some(0));
        }
        app
    }

    /// Currently selected task (if any)
    pub(crate) fn selected_task(&self) -> Option<&Task> {
        self.selected_menu_task().map(|mt| &mt.task)
    }

    /// Currently selected menu task with source (if any)
    pub(crate) fn selected_menu_task(&self) -> Option<&MenuTask> {
        let idx = self
            .list_state
            .selected()
            .and_then(|i| self.visible.get(i))?;
        self.tasks.get(*idx)
    }

    /// Collect all plain tasks (for comparator / readiness logic)
    pub(crate) fn all_tasks(&self) -> Vec<Task> {
        self.tasks.iter().map(|mt| mt.task.clone()).collect()
    }

    /// Recompute visible indices from query
    pub(crate) fn refilter(&mut self) {
        use fuzzy_matcher::FuzzyMatcher as _;

        let matcher = fuzzy_matcher::skim::SkimMatcherV2::default();

        if self.query.is_empty() {
            self.visible = (0..self.tasks.len()).collect();
        } else {
            let mut scored: Vec<(usize, i64)> = self
                .tasks
                .iter()
                .enumerate()
                .filter_map(|(i, mt)| {
                    // Include the synthetic source tag in the haystack for filtering
                    let mut haystack = mt.task.to_todotxt_line();
                    if let Some(tag) = &mt.source.display_tag {
                        haystack.push(' ');
                        haystack.push('@');
                        haystack.push_str(tag);
                    }
                    matcher
                        .fuzzy_match(&haystack, &self.query)
                        .map(|score| (i, score))
                })
                .collect();
            scored.sort_by(|a, b| b.1.cmp(&a.1));
            self.visible = scored.into_iter().map(|(i, _)| i).collect();
        }

        // Clamp selection
        if self.visible.is_empty() {
            self.list_state.select(None);
        } else {
            let sel = self
                .list_state
                .selected()
                .unwrap_or(0)
                .min(self.visible.len() - 1);
            self.list_state.select(Some(sel));
        }
    }

    /// Reload tasks, re-sort, re-filter, preserve selection best-effort
    pub(crate) fn reload_tasks(&mut self, tasks: Vec<MenuTask>) {
        // Try to preserve selection by stable key: (source path, index, todotxt line)
        let prev_key = self.selected_menu_task().map(|mt| {
            (
                mt.source.todo_file.path().to_owned(),
                mt.task.index,
                mt.task.to_todotxt_line(),
            )
        });

        self.tasks = tasks;
        let all = self.all_tasks();
        self.tasks.sort_by(|a, b| b.task.cmp(&a.task, &all));
        self.refilter();

        // Restore selection if possible
        if let Some((prev_path, prev_idx, prev_line)) = prev_key {
            if let Some(pos) = self.visible.iter().position(|idx| {
                self.tasks.get(*idx).is_some_and(|mt| {
                    mt.source.todo_file.path() == prev_path
                        && mt.task.index == prev_idx
                        && mt.task.to_todotxt_line() == prev_line
                })
            }) {
                self.list_state.select(Some(pos));
            }
        }
    }

    /// Show a transient toast notification that expires after `TOAST_DURATION`
    pub(crate) fn set_toast(&mut self, message: String) {
        self.toast = Some((message, Instant::now() + TOAST_DURATION));
    }

    /// Clear the toast if it has expired; returns `true` if it was cleared
    pub(crate) fn expire_toast(&mut self) -> bool {
        if self
            .toast
            .as_ref()
            .is_some_and(|(_, expiry)| Instant::now() >= *expiry)
        {
            self.toast = None;
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Write as _, rc::Rc, time::Instant};

    use tasks::TodoFile;

    use super::*;

    fn today() -> Date {
        chrono::NaiveDate::from_ymd_opt(2026, 3, 18).unwrap()
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

    fn make_tasks(lines: &[&str]) -> Vec<MenuTask> {
        let source = make_source();
        lines
            .iter()
            .map(|l| MenuTask {
                task: l.parse().unwrap(),
                source: Rc::clone(&source),
            })
            .collect()
    }

    // --- App::new ---

    #[test]
    fn new_populates_visible_and_selects_first() {
        let app = App::new(make_tasks(&["Task A", "Task B"]), today(), false);
        assert_eq!(app.visible, vec![0, 1]);
        assert_eq!(app.list_state.selected(), Some(0));
    }

    #[test]
    fn new_empty_list_no_selection() {
        let app = App::new(Vec::new(), today(), false);
        assert!(app.visible.is_empty());
        assert_eq!(app.list_state.selected(), None);
    }

    // --- refilter ---

    #[test]
    fn refilter_empty_query_shows_all() {
        let mut app = App::new(make_tasks(&["Buy milk", "Walk dog"]), today(), false);
        app.query.clear();
        app.refilter();
        assert_eq!(app.visible.len(), 2);
    }

    #[test]
    fn refilter_narrows_results() {
        let mut app = App::new(make_tasks(&["Buy milk", "Walk dog"]), today(), false);
        app.query = "milk".to_owned();
        app.refilter();
        assert_eq!(app.visible.len(), 1);
        assert_eq!(app.tasks[app.visible[0]].task.text, "Buy milk");
    }

    #[test]
    fn refilter_no_match_clears_selection() {
        let mut app = App::new(make_tasks(&["Buy milk"]), today(), false);
        app.query = "zzzzzzz".to_owned();
        app.refilter();
        assert!(app.visible.is_empty());
        assert_eq!(app.list_state.selected(), None);
    }

    #[test]
    fn refilter_clamps_selection() {
        let mut app = App::new(
            make_tasks(&["Buy milk", "Walk dog", "Cook dinner"]),
            today(),
            false,
        );
        app.list_state.select(Some(2));
        // Filter to fewer items → selection should clamp
        app.query = "milk".to_owned();
        app.refilter();
        assert!(app.list_state.selected().unwrap() < app.visible.len());
    }

    // --- selected_task ---

    #[test]
    fn selected_task_returns_correct_task() {
        let app = App::new(make_tasks(&["Buy milk", "Walk dog"]), today(), false);
        let task = app.selected_task().unwrap();
        assert_eq!(task.text, "Buy milk");
    }

    #[test]
    fn selected_task_none_when_empty() {
        let app = App::new(Vec::new(), today(), false);
        assert!(app.selected_task().is_none());
    }

    // --- reload_tasks ---

    #[test]
    fn reload_tasks_updates_and_refilters() {
        let mut app = App::new(make_tasks(&["Buy milk"]), today(), false);
        app.reload_tasks(make_tasks(&["Walk dog", "Cook dinner", "Read book"]));
        assert_eq!(app.tasks.len(), 3);
        assert_eq!(app.visible.len(), 3);
    }

    // --- set_toast / expire_toast ---

    #[test]
    fn set_toast_sets_message() {
        let mut app = App::new(Vec::new(), today(), false);
        app.set_toast("Done!".to_owned());
        assert!(app.toast.is_some());
        assert_eq!(app.toast.as_ref().unwrap().0, "Done!");
    }

    #[test]
    fn expire_toast_false_before_expiry() {
        let mut app = App::new(Vec::new(), today(), false);
        app.set_toast("Done!".to_owned());
        assert!(!app.expire_toast());
        assert!(app.toast.is_some());
    }

    #[test]
    fn expire_toast_true_after_expiry() {
        let mut app = App::new(Vec::new(), today(), false);
        app.toast = Some((
            "old".to_owned(),
            Instant::now().checked_sub(Duration::from_secs(1)).unwrap(),
        ));
        assert!(app.expire_toast());
        assert!(app.toast.is_none());
    }
}
