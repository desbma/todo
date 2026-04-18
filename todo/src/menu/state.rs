//! Menu TUI state

use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashSet},
    rc::Rc,
    time::{Duration, Instant},
};

use ratatui::widgets::ListState;
use tasks::{Date, TagKind, Task, TodoFile};

const TOAST_DURATION: Duration = Duration::from_secs(5);
const FUZZY_WEIGHT: f64 = 0.75;
const URGENCY_WEIGHT: f64 = 0.25;
const _: () = assert!((FUZZY_WEIGHT + URGENCY_WEIGHT - 1.0).abs() < 1e-9);

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

impl MenuTask {
    pub(crate) fn has_project_tag(&self, tag: &str) -> bool {
        self.task.tags.iter().any(|task_tag| {
            matches!(task_tag.kind(), TagKind::Arobase)
                && task_tag.value().eq_ignore_ascii_case(tag)
        }) || self
            .source
            .display_tag
            .as_deref()
            .is_some_and(|source_tag| source_tag.eq_ignore_ascii_case(tag))
    }
}

pub(crate) struct App {
    pub tasks: Vec<MenuTask>,
    pub visible: Vec<usize>,
    pub tabs: Vec<String>,
    pub selected_tab: usize,
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
    /// Whether the executable file has changed and needs re-exec after debounce
    pub pending_exe_reload_at: Option<Instant>,
    /// Height of the task list area (updated each frame by the renderer)
    pub list_height: u16,
}

impl App {
    pub(crate) fn new(tasks: Vec<MenuTask>, today: Date, multi_source: bool) -> Self {
        let tabs = collect_tabs(&tasks);
        let mut app = Self {
            tabs,
            selected_tab: 0,
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
            pending_exe_reload_at: None,
            list_height: 0,
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

    #[must_use]
    pub(crate) fn has_tabs(&self) -> bool {
        self.tabs.len() > 1
    }

    #[must_use]
    pub(crate) fn active_tab(&self) -> Option<&str> {
        (self.selected_tab > 0)
            .then(|| self.tabs.get(self.selected_tab))
            .flatten()
            .map(String::as_str)
    }

    pub(crate) fn clear_tab_filter(&mut self) -> bool {
        self.select_tab(0)
    }

    pub(crate) fn select_next_tab(&mut self) -> bool {
        if self.tabs.len() <= 1 {
            return false;
        }

        self.select_tab((self.selected_tab + 1) % self.tabs.len())
    }

    pub(crate) fn select_prev_tab(&mut self) -> bool {
        if self.tabs.len() <= 1 {
            return false;
        }

        let next = if self.selected_tab == 0 {
            self.tabs.len() - 1
        } else {
            self.selected_tab - 1
        };
        self.select_tab(next)
    }

    /// Recompute visible indices from query
    pub(crate) fn refilter(&mut self) {
        use fuzzy_matcher::FuzzyMatcher as _;

        let matcher = fuzzy_matcher::skim::SkimMatcherV2::default();
        let active_tab = self.active_tab().map(str::to_owned);

        if self.query.is_empty() {
            self.visible = self
                .tasks
                .iter()
                .enumerate()
                .filter(|(_, mt)| {
                    active_tab
                        .as_deref()
                        .is_none_or(|tag| mt.has_project_tag(tag))
                })
                .map(|(i, _)| i)
                .collect();
        } else {
            // Collect (task_index, fuzzy_score) for all matches
            let mut scored: Vec<(usize, i64)> = self
                .tasks
                .iter()
                .enumerate()
                .filter(|(_, mt)| {
                    active_tab
                        .as_deref()
                        .is_none_or(|tag| mt.has_project_tag(tag))
                })
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

            // Sort by fuzzy score descending to assign fuzzy ranks
            scored.sort_by(|a, b| b.1.cmp(&a.1));

            let n = scored.len();
            if n <= 1 {
                self.visible = scored.into_iter().map(|(i, _)| i).collect();
            } else {
                // Assign fuzzy rank (0 = best match) and blend with urgency
                // rank (task_index in the already urgency-sorted vec).
                // Both ranks are normalized to [0.0, 1.0] and blended.
                #[expect(clippy::cast_precision_loss)]
                let blended = {
                    let max_urgency =
                        scored.iter().map(|(idx, _)| *idx).max().unwrap_or(0).max(1) as f64;
                    let max_fuzzy = (n - 1).max(1) as f64;

                    let mut blended: Vec<(usize, f64)> = scored
                        .iter()
                        .enumerate()
                        .map(|(fuzzy_rank, &(task_idx, _))| {
                            let fuzzy_norm = fuzzy_rank as f64 / max_fuzzy;
                            let urgency_norm = task_idx as f64 / max_urgency;
                            (
                                task_idx,
                                FUZZY_WEIGHT * fuzzy_norm + URGENCY_WEIGHT * urgency_norm,
                            )
                        })
                        .collect();
                    blended.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));
                    blended
                };
                self.visible = blended.into_iter().map(|(i, _)| i).collect();
            }
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
        let prev_tab = self.active_tab().map(str::to_owned);

        self.tasks = tasks;
        let all = self.all_tasks();
        self.tasks.sort_by(|a, b| b.task.cmp(&a.task, &all));
        self.rebuild_tabs(prev_tab.as_deref());
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

    fn rebuild_tabs(&mut self, preferred: Option<&str>) {
        self.tabs = collect_tabs(&self.tasks);
        self.selected_tab = preferred
            .and_then(|tag| {
                self.tabs
                    .iter()
                    .position(|candidate| candidate.eq_ignore_ascii_case(tag))
            })
            .unwrap_or(0);
    }

    fn select_tab(&mut self, tab: usize) -> bool {
        if tab == self.selected_tab {
            return false;
        }

        self.selected_tab = tab;
        self.refilter();
        if self.visible.is_empty() {
            self.list_state.select(None);
        } else {
            self.list_state.select(Some(0));
        }
        true
    }
}

fn collect_tabs(tasks: &[MenuTask]) -> Vec<String> {
    let mut tabs = BTreeMap::new();

    for menu_task in tasks {
        for tag in &menu_task.task.tags {
            if matches!(tag.kind(), TagKind::Arobase) {
                tabs.entry(tag.value().to_ascii_lowercase())
                    .or_insert_with(|| tag.value().to_owned());
            }
        }

        if let Some(source_tag) = &menu_task.source.display_tag {
            tabs.entry(source_tag.to_ascii_lowercase())
                .or_insert_with(|| source_tag.clone());
        }
    }

    let mut all_tabs = vec!["All".to_owned()];
    all_tabs.extend(tabs.into_values());
    all_tabs
}

#[cfg(test)]
mod tests {
    use std::{io::Write as _, rc::Rc, time::Instant};

    use tasks::TodoFile;

    use super::*;

    fn today() -> Date {
        chrono::NaiveDate::from_ymd_opt(2026, 3, 18).unwrap()
    }

    fn make_source_with_tag(display_tag: Option<&str>) -> Rc<MenuSource> {
        let mut todo = tempfile::NamedTempFile::new().unwrap();
        let mut done = tempfile::NamedTempFile::new().unwrap();
        writeln!(todo, "placeholder").unwrap();
        writeln!(done, "placeholder").unwrap();
        Rc::new(MenuSource::new(
            TodoFile::new(todo.path(), done.path()).unwrap(),
            display_tag.map(str::to_owned),
        ))
    }

    fn make_source() -> Rc<MenuSource> {
        make_source_with_tag(None)
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

    fn make_tasks_with_source(lines: &[&str], display_tag: Option<&str>) -> Vec<MenuTask> {
        let source = make_source_with_tag(display_tag);
        lines
            .iter()
            .map(|l| MenuTask {
                task: l.parse().unwrap(),
                source: Rc::clone(&source),
            })
            .collect()
    }

    #[test]
    fn new_populates_visible_and_selects_first() {
        let app = App::new(make_tasks(&["Task A", "Task B"]), today(), false);
        assert_eq!(app.visible, vec![0, 1]);
        assert_eq!(app.list_state.selected(), Some(0));
        assert_eq!(app.tabs, vec!["All"]);
    }

    #[test]
    fn new_collects_tabs_from_real_and_source_tags() {
        let mut tasks = make_tasks_with_source(&["@office Buy milk", "Read book"], Some("work"));
        tasks.extend(make_tasks(&["@home Walk dog"]));

        let app = App::new(tasks, today(), true);
        assert_eq!(app.tabs, vec!["All", "home", "office", "work"]);
    }

    #[test]
    fn has_tabs_requires_non_all_entry() {
        let app = App::new(make_tasks(&["Task A", "Task B"]), today(), false);
        assert!(!app.has_tabs());
    }

    #[test]
    fn new_empty_list_no_selection() {
        let app = App::new(Vec::new(), today(), false);
        assert!(app.visible.is_empty());
        assert_eq!(app.list_state.selected(), None);
    }

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
    fn refilter_selected_tab_limits_results() {
        let mut app = App::new(
            make_tasks(&["@home Buy milk", "@work Walk dog", "Read book"]),
            today(),
            false,
        );

        assert!(app.select_next_tab());
        assert_eq!(app.active_tab(), Some("home"));
        assert_eq!(app.visible.len(), 1);
        assert_eq!(app.tasks[app.visible[0]].task.text, "Buy milk");
    }

    #[test]
    fn refilter_selected_tab_and_query_stack() {
        let mut app = App::new(
            make_tasks(&["@work Buy milk", "@work Call mom", "@home Buy bread"]),
            today(),
            false,
        );

        app.select_tab(2);
        app.query = "milk".to_owned();
        app.refilter();

        assert_eq!(app.active_tab(), Some("work"));
        assert_eq!(app.visible.len(), 1);
        assert_eq!(app.tasks[app.visible[0]].task.text, "Buy milk");
    }

    #[test]
    fn refilter_selected_tab_matches_source_tag() {
        let app = App::new(
            make_tasks_with_source(&["Buy milk"], Some("work")),
            today(),
            true,
        );
        assert_eq!(app.tabs, vec!["All", "work"]);
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

    #[test]
    fn reload_tasks_updates_and_refilters() {
        let mut app = App::new(make_tasks(&["Buy milk"]), today(), false);
        app.reload_tasks(make_tasks(&["Walk dog", "Cook dinner", "Read book"]));
        assert_eq!(app.tasks.len(), 3);
        assert_eq!(app.visible.len(), 3);
    }

    #[test]
    fn reload_tasks_falls_back_to_all_when_selected_tab_disappears() {
        let mut app = App::new(
            make_tasks(&["@home Buy milk", "@work Walk dog"]),
            today(),
            false,
        );
        app.select_tab(2);

        app.reload_tasks(make_tasks(&["@home Buy milk"]));

        assert_eq!(app.tabs, vec!["All", "home"]);
        assert_eq!(app.selected_tab, 0);
        assert_eq!(app.active_tab(), None);
    }

    #[test]
    fn reload_tasks_preserves_selected_tab_when_still_present() {
        let mut app = App::new(
            make_tasks(&["@home Buy milk", "@work Walk dog"]),
            today(),
            false,
        );
        app.select_tab(2);

        app.reload_tasks(make_tasks(&["@work Read book", "@home Buy milk"]));

        assert_eq!(app.selected_tab, 2);
        assert_eq!(app.active_tab(), Some("work"));
    }

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

    #[test]
    fn refilter_blends_fuzzy_and_urgency_order() {
        // Tasks are in urgency order: index 0 is most urgent.
        // "Buy milk" (idx 0) is an exact match for "milk" but less urgent
        // would be if it were at a higher index. Place it so urgency and
        // fuzzy disagree to verify blending.
        let mut app = App::new(
            make_tasks(&[
                "Walk the dog",      // idx 0 — most urgent, weak match for "milk"
                "Buy milk",          // idx 1 — medium urgency, strong match
                "Drink almond milk", // idx 2 — least urgent, strong match
            ]),
            today(),
            false,
        );

        app.query = "milk".to_owned();
        app.refilter();

        // Both milk tasks should appear; the exact-ish match "Buy milk"
        // should still rank first because it has the best fuzzy score and
        // a decent urgency position.
        let texts: Vec<&str> = app
            .visible
            .iter()
            .filter_map(|i| app.tasks.get(*i))
            .map(|mt| mt.task.text.as_str())
            .collect();
        assert!(texts.contains(&"Buy milk"));
        assert!(texts.contains(&"Drink almond milk"));
        assert_eq!(texts[0], "Buy milk");
    }

    #[test]
    fn refilter_urgency_breaks_tie_on_equal_fuzzy() {
        // Two tasks with equally strong matches; urgency (position) should
        // break the tie in favor of the earlier (more urgent) task.
        let mut app = App::new(
            make_tasks(&[
                "Buy milk today",    // idx 0 — more urgent
                "Buy milk tomorrow", // idx 1 — less urgent
            ]),
            today(),
            false,
        );

        app.query = "Buy milk".to_owned();
        app.refilter();

        let texts: Vec<&str> = app
            .visible
            .iter()
            .filter_map(|i| app.tasks.get(*i))
            .map(|mt| mt.task.text.as_str())
            .collect();
        assert_eq!(texts, vec!["Buy milk today", "Buy milk tomorrow"]);
    }

    #[test]
    fn refilter_single_match_returns_it() {
        let mut app = App::new(make_tasks(&["Walk the dog", "Buy milk"]), today(), false);
        app.query = "milk".to_owned();
        app.refilter();
        assert_eq!(app.visible.len(), 1);
        assert_eq!(app.tasks[app.visible[0]].task.text, "Buy milk");
    }
}
