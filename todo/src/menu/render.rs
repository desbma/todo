//! Ratatui rendering for the menu TUI

use std::fmt::Write as _;

use crossterm::style::{Attribute, Colored};
use ratatui::{
    Frame,
    layout::{Constraint, Layout, Rect},
    prelude::IntoCrossterm as _,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{
        Block, BorderType, Borders, Clear, HighlightSpacing, List, ListItem, Padding, Paragraph,
        Tabs,
    },
};
use strum::{EnumCount as _, IntoEnumIterator as _};
use tasks::{CreationCompletion, Date, TagKind, Task};

use super::state::{App, Mode, TaskAction};

/// Render the full UI
pub(crate) fn draw(frame: &mut Frame, app: &mut App) {
    let (tabs_area, search_area, list_area, footer_area) = if app.has_tabs() {
        let [tabs_area, search_area, list_area, footer_area] = Layout::vertical([
            Constraint::Length(1), // tabs
            Constraint::Length(1), // search input
            Constraint::Min(1),    // task list
            Constraint::Length(1), // footer
        ])
        .areas(frame.area());
        (Some(tabs_area), search_area, list_area, footer_area)
    } else {
        let [search_area, list_area, footer_area] = Layout::vertical([
            Constraint::Length(1), // search input
            Constraint::Min(1),    // task list
            Constraint::Length(1), // footer
        ])
        .areas(frame.area());
        (None, search_area, list_area, footer_area)
    };

    if let Some(tabs_area) = tabs_area {
        draw_tabs(frame, app, tabs_area);
    }
    draw_search_bar(frame, app, search_area);
    draw_task_list(frame, app, list_area);
    draw_footer(frame, app, footer_area);

    if app.toast.is_some() {
        draw_toast(frame, app);
    }

    if app.mode == Mode::ActionPopup {
        draw_action_popup(frame, app);
    }
}

fn draw_tabs(frame: &mut Frame, app: &App, area: Rect) {
    let tabs = Tabs::new(build_tab_titles(app))
        .select(app.selected_tab)
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .divider(Span::raw(""));
    frame.render_widget(tabs, area);
}

fn draw_search_bar(frame: &mut Frame, app: &App, area: Rect) {
    let input = Paragraph::new(build_search_line(app));
    frame.render_widget(input, area);

    // Place cursor after query text
    frame.set_cursor_position((area.x + search_cursor_offset(app), area.y));
}

fn draw_task_list(frame: &mut Frame, app: &mut App, area: Rect) {
    let all = app.all_tasks();
    let items: Vec<ListItem> = app
        .visible
        .iter()
        .filter_map(|idx| app.tasks.get(*idx))
        .map(|mt| {
            let extra_tag = if app.multi_source {
                // Only append if the task doesn't already have this @tag
                mt.source.display_tag.as_deref().filter(|tag_val| {
                    !mt.task.tags.iter().any(|t| {
                        matches!(t.kind(), TagKind::Arobase)
                            && t.value().eq_ignore_ascii_case(tag_val)
                    })
                })
            } else {
                None
            };
            let line = styled_task_line_with_source(&mt.task, app.today, &all, extra_tag);
            ListItem::new(line)
        })
        .collect();

    let list = List::new(items)
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .highlight_spacing(HighlightSpacing::Always);

    frame.render_stateful_widget(list, area, &mut app.list_state);
}

fn draw_footer(frame: &mut Frame, app: &App, area: Rect) {
    let text = match app.mode {
        Mode::Normal if app.has_tabs() => {
            "↑↓:navigate  Tab/S-Tab:tabs  Enter:actions  Esc:quit  type to filter"
        }
        Mode::Normal => "↑↓:navigate  Enter:actions  Esc:quit  type to filter",
        Mode::ActionPopup => "↑↓:navigate  Enter:select  Esc:cancel",
    };
    let footer = Paragraph::new(Span::styled(
        text,
        Style::default().add_modifier(Modifier::DIM),
    ))
    .block(Block::default().style(Style::default().bg(Color::Black)));
    frame.render_widget(footer, area);
}

fn draw_toast(frame: &mut Frame, app: &App) {
    let Some((msg, _)) = &app.toast else {
        return;
    };

    let area = frame.area();
    #[expect(clippy::cast_possible_truncation)]
    let width = (msg.len() as u16 + 4).min(area.width);
    let height = 3_u16; // top border + text + bottom border
    let x = area.width.saturating_sub(width) + area.x;
    // Place above footer (last row)
    let y = area.height.saturating_sub(height + 1) + area.y;
    let toast_area = Rect::new(x, y, width, height);

    frame.render_widget(Clear, toast_area);

    let toast = Paragraph::new(Span::styled(msg.as_str(), Style::default())).block(
        Block::default()
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .padding(Padding::horizontal(1)),
    );
    frame.render_widget(toast, toast_area);
}

fn draw_action_popup(frame: &mut Frame, app: &App) {
    let area = frame.area();
    let popup_width = 24_u16;
    #[expect(clippy::cast_possible_truncation)]
    let popup_height = (TaskAction::COUNT as u16) + 2; // +2 for borders
    let x = area.width.saturating_sub(popup_width) / 2 + area.x;
    let y = area.height.saturating_sub(popup_height) / 2 + area.y;
    let popup_area = Rect::new(
        x,
        y,
        popup_width.min(area.width),
        popup_height.min(area.height),
    );

    frame.render_widget(Clear, popup_area);

    let items: Vec<ListItem> = TaskAction::iter()
        .enumerate()
        .map(|(i, action)| {
            let style = if i == app.action_index {
                Style::default().add_modifier(Modifier::REVERSED)
            } else {
                Style::default()
            };
            ListItem::new(Span::styled(action.label(), style))
        })
        .collect();

    let popup = List::new(items).block(
        Block::default()
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .padding(Padding::horizontal(1))
            .title("Action"),
    );
    frame.render_widget(popup, popup_area);
}

fn build_tab_titles(app: &App) -> Vec<Line<'_>> {
    let all_tasks = app.all_tasks();

    app.tabs
        .iter()
        .map(|tab| {
            let style = tab_style(app, tab, &all_tasks);
            if tab == "All" {
                Line::from(Span::styled(tab.as_str(), style))
            } else {
                Line::from(Span::styled(format!("@{tab}"), style))
            }
        })
        .collect()
}

fn tab_style(app: &App, tab: &str, all_tasks: &[Task]) -> Style {
    let mut style = if tab == "All" {
        Style::default()
    } else {
        Style::default().fg(Color::Blue)
    };

    if !tab_has_ready_tasks(app, tab, all_tasks) {
        style = style.add_modifier(Modifier::DIM);
    }

    style
}

fn tab_has_ready_tasks(app: &App, tab: &str, all_tasks: &[Task]) -> bool {
    app.tasks
        .iter()
        .filter(|menu_task| tab == "All" || menu_task.has_project_tag(tab))
        .any(|menu_task| menu_task.task.is_ready(app.today, all_tasks))
}

fn build_search_line(app: &App) -> Line<'_> {
    let mut spans = vec![Span::styled("> ", Style::default().fg(Color::Cyan))];

    if let Some(tab) = app.active_tab() {
        spans.push(Span::styled(
            format!("@{tab}"),
            Style::default().fg(Color::Blue).add_modifier(Modifier::DIM),
        ));
        spans.push(Span::raw(" "));
    }

    spans.push(Span::raw(app.query.as_str()));

    Line::from(spans)
}

fn search_cursor_offset(app: &App) -> u16 {
    let mut offset = 2_u16;
    if let Some(tab) = app.active_tab() {
        #[expect(clippy::cast_possible_truncation)]
        {
            offset += (tab.len() + 2) as u16;
        }
    }
    #[expect(clippy::cast_possible_truncation)]
    {
        offset + app.query.len() as u16
    }
}

/// Build a styled ratatui `Line` for a task (convenience wrapper for non-menu callers)
pub(crate) fn styled_task_line<'a>(task: &'a Task, today: Date, all_tasks: &[Task]) -> Line<'a> {
    styled_task_line_with_source(task, today, all_tasks, None)
}

/// Build a styled ratatui `Line` for a task, optionally appending a synthetic source tag
#[expect(clippy::too_many_lines)]
fn styled_task_line_with_source<'a>(
    task: &'a Task,
    today: Date,
    all_tasks: &[Task],
    source_tag: Option<&str>,
) -> Line<'a> {
    let is_completed = matches!(task.status, CreationCompletion::Completed { .. });
    let blocked = !is_completed && task.is_blocked(all_tasks);
    let overdue = task.is_overdue(today);
    let pending = task.is_pending(today);
    let not_ready = !task.is_ready(today, all_tasks);

    let mut spans: Vec<Span<'a>> = Vec::new();

    // Priority or completion prefix
    match &task.status {
        CreationCompletion::Pending { created } => {
            if let Some(priority) = task.priority {
                let style = if not_ready {
                    Style::default()
                } else {
                    match priority {
                        'A' => Style::default().fg(Color::Red),
                        'B' => Style::default().fg(Color::Indexed(9)),
                        'C' => Style::default().fg(Color::Yellow),
                        _ => Style::default(),
                    }
                };
                spans.push(Span::styled(format!("({priority})"), style));
                spans.push(Span::raw(" "));
            }
            if let Some(created) = created {
                let style = if not_ready {
                    Style::default()
                } else {
                    let age = today.signed_duration_since(*created).num_days();
                    if (0..=7).contains(&age) {
                        Style::default().add_modifier(Modifier::DIM)
                    } else if (8..=30).contains(&age) {
                        Style::default()
                    } else {
                        Style::default().add_modifier(Modifier::BOLD)
                    }
                };
                spans.push(Span::styled(format!("{created}"), style));
                spans.push(Span::raw(" "));
            }
        }
        CreationCompletion::Completed {
            created,
            completed: completed_date,
        } => {
            spans.push(Span::raw(format!("x {completed_date}")));
            spans.push(Span::raw(" "));
            if let Some(created) = created {
                spans.push(Span::raw(format!("{created}")));
                spans.push(Span::raw(" "));
            }
        }
    }

    // Synthetic source tag (presentation-only, never persisted)
    if let Some(stag) = source_tag {
        let style = if not_ready {
            Style::default()
        } else {
            Style::default().fg(Color::Blue)
        };
        spans.push(Span::styled(format!("@{stag}"), style));
        spans.push(Span::raw(" "));
    }

    // Tags
    for tag in &task.tags {
        let style = if not_ready {
            Style::default()
        } else {
            match tag.kind() {
                TagKind::Plus => Style::default().fg(Color::Cyan),
                TagKind::Hash => Style::default().fg(Color::Yellow),
                TagKind::Arobase => Style::default().fg(Color::Blue),
            }
        };
        spans.push(Span::styled(
            format!("{}{}", tag.kind().as_ref(), tag.value()),
            style,
        ));
        spans.push(Span::raw(" "));
    }

    // Text
    spans.push(Span::raw(task.text.clone()));

    // Attributes
    for (key, value) in &task.attributes {
        spans.push(Span::raw(" "));
        let style = if not_ready {
            Style::default()
        } else if key == "due" && overdue {
            Style::default().fg(Color::Magenta)
        } else {
            Style::default().fg(Color::Green)
        };
        spans.push(Span::styled(format!("{key}:{value}"), style));
    }

    // Completed tasks: append pri:X
    if is_completed {
        if let Some(priority) = task.priority {
            spans.push(Span::raw(format!(" pri:{priority}")));
        }
    }

    // Whole-line modifiers
    let mut line = Line::from(spans);
    if is_completed {
        line = line.style(
            Style::default()
                .add_modifier(Modifier::DIM)
                .add_modifier(Modifier::CROSSED_OUT),
        );
    } else if blocked {
        let mut style = Style::default().add_modifier(Modifier::ITALIC);
        if overdue {
            style = style.fg(Color::Magenta);
        }
        line = line.style(style);
    } else if !pending {
        line = line.style(Style::default().add_modifier(Modifier::DIM));
    }

    line
}

/// Convert a ratatui `Line` to an ANSI-escaped string for plain terminal output
pub(crate) fn line_to_ansi(line: &Line<'_>) -> String {
    let reset = Attribute::Reset.to_string();

    let mut out = String::new();

    // Apply line-level style
    let line_ansi = style_to_ansi_open(&line.style);
    let has_line_style = !line_ansi.is_empty();
    if has_line_style {
        out.push_str(&line_ansi);
    }

    for span in &line.spans {
        let merged = merge_styles(&line.style, &span.style);
        let span_ansi = style_to_ansi_open(&merged);
        if !span_ansi.is_empty() {
            // Reset before each span to avoid style leaking, then apply merged style
            out.push_str(&reset);
            out.push_str(&span_ansi);
        }
        let _ = write!(out, "{}", span.content);
        if !span_ansi.is_empty() {
            out.push_str(&reset);
            // Re-apply line style after span
            if has_line_style {
                out.push_str(&line_ansi);
            }
        }
    }

    if has_line_style {
        out.push_str(&reset);
    }

    out
}

/// Merge a parent style with a child style (child overrides)
fn merge_styles(parent: &Style, child: &Style) -> Style {
    let mut merged = *parent;
    if child.fg.is_some() {
        merged.fg = child.fg;
    }
    if child.bg.is_some() {
        merged.bg = child.bg;
    }
    if !child.add_modifier.is_empty() {
        merged.add_modifier = merged.add_modifier.union(child.add_modifier);
    }
    if !child.sub_modifier.is_empty() {
        merged.sub_modifier = merged.sub_modifier.union(child.sub_modifier);
    }
    merged
}

fn style_to_ansi_open(style: &Style) -> String {
    let mut codes: Vec<String> = Vec::new();

    // Map ratatui Modifier flags to crossterm Attributes for their SGR codes
    let modifier_map: &[(Modifier, Attribute)] = &[
        (Modifier::BOLD, Attribute::Bold),
        (Modifier::DIM, Attribute::Dim),
        (Modifier::ITALIC, Attribute::Italic),
        (Modifier::CROSSED_OUT, Attribute::CrossedOut),
    ];
    for (modifier, attr) in modifier_map {
        if style.add_modifier.contains(*modifier) {
            codes.push(attr.sgr());
        }
    }

    if let Some(fg) = style.fg {
        codes.push(format!("{}", Colored::ForegroundColor(fg.into_crossterm())));
    }

    if codes.is_empty() {
        String::new()
    } else {
        format!("\x1b[{}m", codes.join(";"))
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Write as _, rc::Rc};

    use ratatui::{
        style::{Color, Modifier, Style},
        text::{Line, Span},
    };
    use tasks::{Task, TodoFile};

    use super::*;
    use crate::menu::state::{App, MenuSource, MenuTask};

    fn today() -> Date {
        chrono::NaiveDate::from_ymd_opt(2026, 3, 18).unwrap()
    }

    fn parse(s: &str) -> Task {
        s.parse().unwrap()
    }

    fn make_app(lines: &[&str], selected_tab: usize) -> App {
        make_app_with_source_tag(lines, selected_tab, Some("work"))
    }

    fn make_app_with_source_tag(
        lines: &[&str],
        selected_tab: usize,
        source_tag: Option<&str>,
    ) -> App {
        let mut todo = tempfile::NamedTempFile::new().unwrap();
        let mut done = tempfile::NamedTempFile::new().unwrap();
        writeln!(todo, "placeholder").unwrap();
        writeln!(done, "placeholder").unwrap();
        let source = Rc::new(MenuSource::new(
            TodoFile::new(todo.path(), done.path()).unwrap(),
            source_tag.map(str::to_owned),
        ));
        let tasks = lines
            .iter()
            .map(|line| MenuTask {
                task: line.parse().unwrap(),
                source: Rc::clone(&source),
            })
            .collect();
        let mut app = App::new(tasks, today(), true);
        app.selected_tab = selected_tab;
        app
    }

    #[test]
    fn priority_a_has_red_fg() {
        let task = parse("(A) Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let pri_span = &line.spans[0];
        assert_eq!(pri_span.content.as_ref(), "(A)");
        assert_eq!(pri_span.style.fg, Some(Color::Red));
    }

    #[test]
    fn priority_b_has_orange_fg() {
        let task = parse("(B) Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let pri_span = &line.spans[0];
        assert_eq!(pri_span.content.as_ref(), "(B)");
        assert_eq!(pri_span.style.fg, Some(Color::Indexed(9)));
    }

    #[test]
    fn priority_c_has_yellow_fg() {
        let task = parse("(C) Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let pri_span = &line.spans[0];
        assert_eq!(pri_span.content.as_ref(), "(C)");
        assert_eq!(pri_span.style.fg, Some(Color::Yellow));
    }

    #[test]
    fn completed_task_has_dim_and_crossed_out() {
        let task = parse("x 2026-03-17 Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        assert!(line.style.add_modifier.contains(Modifier::DIM));
        assert!(line.style.add_modifier.contains(Modifier::CROSSED_OUT));
    }

    #[test]
    fn plus_tag_has_cyan_fg() {
        let task = parse("+project Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let tag_span = line
            .spans
            .iter()
            .find(|s| s.content.starts_with('+'))
            .unwrap();
        assert_eq!(tag_span.style.fg, Some(Color::Cyan));
    }

    #[test]
    fn hash_tag_has_yellow_fg() {
        let task = parse("#tag Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let tag_span = line
            .spans
            .iter()
            .find(|s| s.content.starts_with('#'))
            .unwrap();
        assert_eq!(tag_span.style.fg, Some(Color::Yellow));
    }

    #[test]
    fn arobase_tag_has_blue_fg() {
        let task = parse("@context Buy milk");
        let line = styled_task_line(&task, today(), &[]);
        let tag_span = line
            .spans
            .iter()
            .find(|s| s.content.starts_with('@'))
            .unwrap();
        assert_eq!(tag_span.style.fg, Some(Color::Blue));
    }

    #[test]
    fn overdue_due_has_magenta_fg() {
        let task = parse("Buy milk due:2026-03-01");
        let line = styled_task_line(&task, today(), &[]);
        let due_span = line
            .spans
            .iter()
            .find(|s| s.content.starts_with("due:"))
            .unwrap();
        assert_eq!(due_span.style.fg, Some(Color::Magenta));
    }

    #[test]
    fn blocked_task_has_italic() {
        let blocker = parse("Buy milk id:1");
        let task = parse("Drink milk dep:1");
        let all = vec![blocker, task.clone()];
        let line = styled_task_line(&task, today(), &all);
        assert!(line.style.add_modifier.contains(Modifier::ITALIC));
    }

    #[test]
    fn source_tag_appended_with_blue() {
        let task = parse("Buy milk");
        let line = styled_task_line_with_source(&task, today(), &[], Some("work"));
        let source_span = line
            .spans
            .iter()
            .find(|s| s.content.as_ref() == "@work")
            .unwrap();
        assert_eq!(source_span.style.fg, Some(Color::Blue));
    }

    #[test]
    fn search_line_shows_dimmed_selected_tab() {
        let app = make_app(&["Buy milk"], 1);
        let line = build_search_line(&app);
        let tab_span = line
            .spans
            .iter()
            .find(|span| span.content.as_ref() == "@work")
            .unwrap();
        assert_eq!(tab_span.style.fg, Some(Color::Blue));
        assert!(tab_span.style.add_modifier.contains(Modifier::DIM));
    }

    #[test]
    fn search_line_places_selected_tab_before_query() {
        let mut app = make_app(&["Buy milk"], 1);
        app.query = "milk".to_owned();

        let line = build_search_line(&app);

        assert_eq!(line.spans[1].content.as_ref(), "@work");
        assert_eq!(line.spans[2].content.as_ref(), " ");
        assert_eq!(line.spans[3].content.as_ref(), "milk");
    }

    #[test]
    fn search_line_omits_tab_when_all_selected() {
        let app = make_app(&["Buy milk"], 0);
        let line = build_search_line(&app);
        assert!(
            line.spans
                .iter()
                .all(|span| span.content.as_ref() != "@work")
        );
    }

    #[test]
    fn tab_titles_prefix_projects_with_at() {
        let app = make_app(&["Buy milk"], 0);
        let titles = build_tab_titles(&app);
        assert_eq!(titles[0], Line::from(Span::styled("All", Style::default())));
        assert_eq!(
            titles[1],
            Line::from(Span::styled("@work", Style::default().fg(Color::Blue)))
        );
    }

    #[test]
    fn completed_only_project_tab_is_dimmed() {
        let app = make_app_with_source_tag(&["x 2026-03-17 @work Done"], 0, None);
        let titles = build_tab_titles(&app);
        let work_span = &titles[1].spans[0];
        assert_eq!(work_span.content.as_ref(), "@work");
        assert_eq!(work_span.style.fg, Some(Color::Blue));
        assert!(work_span.style.add_modifier.contains(Modifier::DIM));
    }

    #[test]
    fn blocked_only_project_tab_is_dimmed() {
        let app = make_app_with_source_tag(&["@work Follow up dep:1", "Blocker id:1"], 0, None);
        let titles = build_tab_titles(&app);
        let work_span = &titles[1].spans[0];
        assert_eq!(work_span.content.as_ref(), "@work");
        assert_eq!(work_span.style.fg, Some(Color::Blue));
        assert!(work_span.style.add_modifier.contains(Modifier::DIM));
    }

    #[test]
    fn project_tab_with_ready_task_is_not_dimmed() {
        let app = make_app_with_source_tag(
            &["@work Follow up dep:1", "@work Ready task", "Blocker id:1"],
            0,
            None,
        );
        let titles = build_tab_titles(&app);
        let work_span = &titles[1].spans[0];
        assert_eq!(work_span.content.as_ref(), "@work");
        assert_eq!(work_span.style.fg, Some(Color::Blue));
        assert!(!work_span.style.add_modifier.contains(Modifier::DIM));
    }

    #[test]
    fn empty_line_to_ansi() {
        let line = Line::from(Vec::<Span>::new());
        assert_eq!(line_to_ansi(&line), "");
    }

    #[test]
    fn plain_line_to_ansi() {
        let line = Line::from(vec![Span::raw("hello")]);
        assert_eq!(line_to_ansi(&line), "hello");
    }

    #[test]
    fn styled_span_produces_ansi_escapes() {
        let line = Line::from(vec![Span::styled("red", Style::default().fg(Color::Red))]);
        let ansi = line_to_ansi(&line);
        assert!(ansi.contains("\x1b["));
        assert!(ansi.contains("red"));
    }

    #[test]
    fn child_fg_overrides_parent() {
        let parent = Style::default().fg(Color::Red);
        let child = Style::default().fg(Color::Blue);
        let merged = merge_styles(&parent, &child);
        assert_eq!(merged.fg, Some(Color::Blue));
    }

    #[test]
    fn child_modifiers_union_with_parent() {
        let parent = Style::default().add_modifier(Modifier::BOLD);
        let child = Style::default().add_modifier(Modifier::ITALIC);
        let merged = merge_styles(&parent, &child);
        assert!(merged.add_modifier.contains(Modifier::BOLD));
        assert!(merged.add_modifier.contains(Modifier::ITALIC));
    }

    #[test]
    fn empty_child_preserves_parent() {
        let parent = Style::default().fg(Color::Red).add_modifier(Modifier::BOLD);
        let child = Style::default();
        let merged = merge_styles(&parent, &child);
        assert_eq!(merged.fg, Some(Color::Red));
        assert!(merged.add_modifier.contains(Modifier::BOLD));
    }
}
