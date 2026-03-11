//! Menu TUI state

use std::time::Instant;

use ratatui::widgets::ListState;
use tasks::{Date, Task};

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

pub(crate) struct App {
    pub tasks: Vec<Task>,
    pub visible: Vec<usize>,
    pub query: String,
    pub list_state: ListState,
    pub mode: Mode,
    pub action_index: usize,
    pub today: Date,
    pub should_quit: bool,
    pub pending_reload_at: Option<Instant>,
    pub status_message: Option<String>,
}

impl App {
    pub(crate) fn new(tasks: Vec<Task>, today: Date) -> Self {
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
            status_message: None,
        };
        app.refilter();
        if !app.visible.is_empty() {
            app.list_state.select(Some(0));
        }
        app
    }

    /// Currently selected task (if any)
    pub(crate) fn selected_task(&self) -> Option<&Task> {
        let idx = self
            .list_state
            .selected()
            .and_then(|i| self.visible.get(i))?;
        self.tasks.get(*idx)
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
                .filter_map(|(i, task)| {
                    let haystack = task.to_todotxt_line();
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
    pub(crate) fn reload_tasks(&mut self, tasks: Vec<Task>) {
        // Try to preserve selection by task identity
        let prev_selected = self.selected_task().cloned();

        self.tasks = tasks;
        let tasks_clone = self.tasks.clone();
        self.tasks.sort_by(|a, b| b.cmp(a, &tasks_clone));
        self.refilter();

        // Restore selection if possible
        if let Some(prev) = prev_selected {
            if let Some(pos) = self.visible.iter().position(|idx| {
                self.tasks
                    .get(*idx)
                    .is_some_and(|t| t.to_todotxt_line() == prev.to_todotxt_line())
            }) {
                self.list_state.select(Some(pos));
            }
        }
    }
}
