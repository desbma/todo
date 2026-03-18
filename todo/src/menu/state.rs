//! Menu TUI state

use std::{collections::HashSet, rc::Rc, time::Instant};

use ratatui::widgets::ListState;
use tasks::{Date, Task, TodoFile};

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
    pub status_message: Option<String>,
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
            status_message: None,
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
}
