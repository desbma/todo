//! Todo.txt task

use std::{
    cmp::Ordering,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    str::FromStr,
    sync::LazyLock,
};

use chrono::Duration;
use regex::{Regex, RegexBuilder};

pub type Date = chrono::naive::NaiveDate;

fn today() -> Date {
    chrono::Local::now().date_naive()
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, strum::EnumString, strum::AsRefStr)]
pub(crate) enum TagKind {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "#")]
    Hash,
    #[strum(serialize = "@")]
    Arobase,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Tag {
    kind: TagKind,
    value: String,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CreationCompletion {
    Pending {
        created: Option<Date>,
    },
    Completed {
        created: Option<Date>,
        completed: Date,
    },
}

impl Default for CreationCompletion {
    fn default() -> Self {
        Self::Pending { created: None }
    }
}

// See https://github.com/todotxt/todo.txt#todotxt-format-rules
#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct Task {
    pub priority: Option<char>,
    pub status: CreationCompletion,
    pub tags: Vec<Tag>,
    pub attributes: Vec<(String, String)>,
    pub text: String,
    pub index: Option<usize>,
}

#[derive(Debug, Eq, PartialEq)]
enum RecurrenceReference {
    Task,
    Completed,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Recurrence {
    delta: Duration,
    reference: RecurrenceReference,
}

const DATE_FORMAT: &str = "%Y-%m-%d";
const RECURRENCE_TAG_KEYS: [&str; 2] = ["rec", "rec2"];

static REC_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?<ref>\+?)(?<val>\d+)(?<unit>d|w|m|y)").unwrap());

impl FromStr for Recurrence {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let caps = REC_REGEX
            .captures(s)
            .ok_or_else(|| anyhow::anyhow!("Invalid recurrence"))?;
        let ref_ = caps.name("ref").unwrap().as_str();
        let val = caps.name("val").unwrap().as_str().parse::<u64>()?;
        let unit = caps.name("unit").unwrap().as_str();
        let delta = match unit {
            "d" => Duration::days(val.try_into()?),
            "w" => Duration::weeks(val.try_into()?),
            "m" => Duration::days((30 * val).try_into()?),
            "y" => Duration::days((365 * val).try_into()?),
            _ => unreachable!(),
        };
        let reference = match ref_ {
            "" => RecurrenceReference::Completed,
            "+" => RecurrenceReference::Task,
            _ => unreachable!(),
        };
        Ok(Self { delta, reference })
    }
}

pub struct StyleContext<'a> {
    pub today: &'a Date,
    pub other_tasks: &'a [Task],
}

impl Task {
    /// Is task not completed and before threshold?
    #[must_use]
    pub fn is_pending(&self, today: Date) -> bool {
        match self.status {
            CreationCompletion::Pending { .. } => {
                self.threshold_date().map_or(true, |t| t <= today)
            }
            CreationCompletion::Completed { .. } => false,
        }
    }

    /// Is task depending on another pending task?
    #[must_use]
    fn is_blocked(&self, others: &[Task]) -> bool {
        self.depends_on().iter().any(|id| {
            others.iter().any(|t| {
                matches!(t.status, CreationCompletion::Pending { .. })
                    && t.id().as_ref().is_some_and(|oid| oid == *id)
            })
        })
    }

    /// Is task ready to be worked on?
    #[must_use]
    pub fn is_ready(&self, today: Date, others: &[Task]) -> bool {
        self.is_pending(today) && !self.is_blocked(others)
    }

    #[must_use]
    fn attribute(&self, name: &str) -> Option<&str> {
        self.attributes
            .iter()
            .find(|a| a.0 == name)
            .map(|a| a.1.as_str())
    }

    #[must_use]
    fn id(&self) -> Option<String> {
        self.attributes
            .iter()
            .find(|a| a.0 == "id")
            .map(|a| a.1.clone())
            .or_else(|| {
                // Fallback to string generated from first tag's 3 first letters, and first letter of each text word
                if let Some(first_tag) = self.tags.first().map(|t| {
                    let mut v = t.value.to_ascii_lowercase();
                    v.truncate(3);
                    v
                }) {
                    let first_letters = self
                        .text
                        .split_whitespace()
                        .filter_map(|w| w.chars().next())
                        .map(|c| c.to_ascii_lowercase())
                        .collect::<String>();
                    Some(format!("{first_tag}{first_letters}"))
                } else {
                    None
                }
            })
    }

    #[must_use]
    fn depends_on(&self) -> Vec<&str> {
        self.attributes
            .iter()
            .filter(|a| a.0 == "dep")
            .flat_map(|a| a.1.split(','))
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn threshold_date(&self) -> Option<Date> {
        self.attribute("t")
            .and_then(|t| Date::parse_from_str(t, DATE_FORMAT).ok())
    }

    #[must_use]
    pub fn due_date(&self) -> Option<Date> {
        self.attribute("due")
            .and_then(|d| Date::parse_from_str(d, DATE_FORMAT).ok())
    }

    #[must_use]
    pub fn started_date(&self) -> Option<Date> {
        self.attribute("started")
            .and_then(|d| Date::parse_from_str(d, DATE_FORMAT).ok())
    }

    #[must_use]
    pub fn is_overdue(&self, today: Date) -> bool {
        self.due_date().is_some_and(|d| d <= today)
    }

    #[must_use]
    pub fn created_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Completed { created, .. }
            | CreationCompletion::Pending { created } => created,
        }
    }

    #[must_use]
    pub fn completed_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Pending { .. } => None,
            CreationCompletion::Completed { completed, .. } => Some(completed),
        }
    }

    #[must_use]
    pub fn recurrence(&self) -> Option<Recurrence> {
        self.attributes
            .iter()
            .find(|a| RECURRENCE_TAG_KEYS.contains(&a.0.as_str()))
            .and_then(|v| v.1.parse().ok())
    }

    pub fn set_done(&mut self, today: Date) -> anyhow::Result<()> {
        match self.status {
            CreationCompletion::Pending { created } => {
                self.status = CreationCompletion::Completed {
                    created,
                    completed: today,
                };
            }
            CreationCompletion::Completed { .. } => anyhow::bail!("Aleady completed"),
        }
        Ok(())
    }

    #[must_use]
    pub fn recur(&self, today: Date) -> Option<Self> {
        self.recurrence().and_then(|r| {
            // Update attributes
            let mut attributes = self.attributes.clone();
            if let Some(due) = self.due_date() {
                // Update due
                let ref_ = match r.reference {
                    RecurrenceReference::Task => due,
                    RecurrenceReference::Completed => today,
                };
                attributes.iter_mut().find(|a| a.0 == "due").unwrap().1 =
                    (ref_ + r.delta).format(DATE_FORMAT).to_string();

                // Update threshold
                if let Some(a) = attributes.iter_mut().find(|a| a.0 == "t") {
                    let threshold = self.threshold_date().unwrap();
                    // New threshold is new due date - delta betwwen old due and threshold
                    a.1 = ((ref_ + r.delta) - (due - threshold))
                        .format(DATE_FORMAT)
                        .to_string();
                }
            } else if let Some(threshold) = self.threshold_date() {
                // Update threshold
                let ref_ = match r.reference {
                    RecurrenceReference::Task => threshold,
                    RecurrenceReference::Completed => today,
                };
                attributes.iter_mut().find(|a| a.0 == "t").unwrap().1 =
                    (ref_ + r.delta).format(DATE_FORMAT).to_string();
            } else {
                return None;
            }

            // Remove started
            attributes.retain(|a| a.0 != "started");

            // Update status
            let status = CreationCompletion::Pending {
                created: Some(today),
            };

            Some(Self {
                status,
                attributes,
                index: None,
                ..self.clone()
            })
        })
    }

    /// Return true if this task is similar (same task from different recurrence)
    #[must_use]
    pub fn is_same_recurring(&self, other: &Task) -> bool {
        // We allow difference in tags and attributes to avoid auto recurrence from
        // creating duplicates when the pending task is edited
        (self.text == other.text)
            && (self
                .attributes
                .iter()
                .any(|(k, _v)| RECURRENCE_TAG_KEYS.contains(&k.as_str()))
                == other
                    .attributes
                    .iter()
                    .any(|(k, _v)| RECURRENCE_TAG_KEYS.contains(&k.as_str())))
    }

    /// Set or update started attribute
    pub fn start(&mut self, today: Date) {
        // Add/update started attribute
        let started_date = today.format(DATE_FORMAT).to_string();
        if let Some(started) = self.attributes.iter_mut().find(|a| a.0 == "started") {
            started.1 = started_date;
        } else {
            self.attributes.push(("started".to_owned(), started_date));
        }
    }

    /// Compare tasks for sorting
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn cmp(&self, other: &Self, others: &[Self]) -> Ordering {
        // Completed is obviously less urgent than pending
        match (&self.status, &other.status) {
            (CreationCompletion::Completed { .. }, CreationCompletion::Pending { .. }) => {
                log::trace!("status: {self:?} < {other:?}");
                return Ordering::Less;
            }
            (CreationCompletion::Pending { .. }, CreationCompletion::Completed { .. }) => {
                log::trace!("status: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            _ => (),
        }

        // Before threshold is less urgent
        let today = today();
        let threshold_diff = self
            .threshold_date()
            .map(|t| today.signed_duration_since(t));
        let other_threshold_diff = other
            .threshold_date()
            .map(|t| today.signed_duration_since(t));
        match (threshold_diff, other_threshold_diff) {
            (Some(d), Some(od)) if d < Duration::zero() && od >= Duration::zero() => {
                log::trace!("before threshold: {self:?} < {other:?}");
                return Ordering::Less;
            }
            (Some(d), Some(od)) if d >= Duration::zero() && od < Duration::zero() => {
                log::trace!("before threshold: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            (Some(d), None) if d < Duration::zero() => {
                log::trace!("before threshold: {self:?} < {other:?}");
                return Ordering::Less;
            }
            (None, Some(od)) if od < Duration::zero() => {
                log::trace!("before threshold: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            _ => (),
        }

        // Being blocked is less urgent that not being
        match (self.is_blocked(others), other.is_blocked(others)) {
            (true, false) => {
                log::trace!("blocked: {self:?} < {other:?}");
                return Ordering::Less;
            }
            (false, true) => {
                log::trace!("blocked: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            _ => (),
        }

        // Overdue
        let due = self.due_date();
        let other_due = other.due_date();
        match (
            self.is_overdue(today),
            due,
            other.is_overdue(today),
            other_due,
        ) {
            (true, Some(d), true, Some(od)) if (d != od) => {
                // Both overdue, compare due dates
                let cmp = od.cmp(&d);
                log::trace!("overdue: {self:?} {cmp:?} {other:?}");
                return cmp;
            }
            (true, _, false, _) => {
                log::trace!("overdue: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            (false, _, true, _) => {
                log::trace!("overdue: {self:?} < {other:?}");
                return Ordering::Less;
            }
            _ => (),
        }

        // Explicit priority, no priority is less urgent than 'D' priority
        match (self.priority, other.priority) {
            (Some(p), Some(op)) if p != op => {
                let cmp = op.cmp(&p);
                log::trace!("priority: {self:?} {cmp:?} {other:?}");
                return cmp;
            }
            (Some(p), None) if p < 'D' => {
                log::trace!("priority: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            (None, Some(op)) if op < 'D' => {
                log::trace!("priority: {self:?} < {other:?}");
                return Ordering::Less;
            }
            _ => (),
        }

        // Having due date is more important than not having one, if not before threshold
        match (due, other_due) {
            (Some(_), None) if self.is_pending(today) => {
                log::trace!("due: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            (None, Some(_)) if other.is_pending(today) => {
                log::trace!("due: {self:?} < {other:?}");
                return Ordering::Less;
            }
            _ => (),
        }

        // Started tasks are more important than not started
        match (self.started_date(), other.started_date()) {
            (Some(_), None) => {
                log::trace!("started: {self:?} > {other:?}");
                return Ordering::Greater;
            }
            (None, Some(_)) => {
                log::trace!("started: {self:?} < {other:?}");
                return Ordering::Less;
            }
            (Some(s), Some(o)) if s != o => {
                // Started first are more important
                let cmp = o.cmp(&s);
                log::trace!("started: {self:?} {cmp:?} {other:?}");
                return cmp;
            }
            _ => (),
        }

        // Due date
        match (due, other_due) {
            (Some(d), Some(od)) if d != od => {
                // Due first are more important
                let cmp = od.cmp(&d);
                log::trace!("due: {self:?} {cmp:?} {other:?}");
                return cmp;
            }
            _ => (),
        }

        // Created date
        match (self.created_date(), other.created_date()) {
            (Some(created), Some(other_created)) if created != other_created => {
                let cmp = other_created.cmp(&created);
                log::trace!("created: {self:?} {cmp:?} {other:?}");
                return cmp;
            }
            _ => {}
        }

        // Index
        if self.index != other.index {
            let cmp = other
                .index
                .unwrap_or(usize::MAX)
                .cmp(&self.index.unwrap_or(usize::MAX));
            log::trace!("index: {self:?} {cmp:?} {other:?}");
            return cmp;
        }

        // Hash
        let mut sh = DefaultHasher::new();
        let mut oh = DefaultHasher::new();
        self.hash(&mut sh);
        other.hash(&mut oh);
        let cmp = sh.finish().cmp(&oh.finish());
        log::trace!("hash: {self:?} {cmp:?} {other:?}");
        cmp
    }

    #[must_use]
    pub fn to_string(&self, style_ctx: Option<&StyleContext>) -> String {
        let mut segments = Vec::new();

        let (base_style, overdue, single_global_style) = if let Some(style_ctx) = style_ctx {
            (
                console::Style::new().for_stdout(),
                self.is_overdue(*style_ctx.today),
                !self.is_ready(*style_ctx.today, style_ctx.other_tasks),
            )
        } else {
            (console::Style::new().force_styling(false), false, false)
        };

        match self.status {
            CreationCompletion::Pending { created } => {
                if let Some(priority) = self.priority {
                    let priority_style = match priority {
                        'A' if !single_global_style => base_style.clone().red(),
                        'B' if !single_global_style => base_style.clone().color256(9),
                        'C' if !single_global_style => base_style.clone().yellow(),
                        _ => base_style.clone(),
                    };
                    segments.push(priority_style.apply_to(format!("({priority})")).to_string());
                }
                if let Some(created) = created {
                    let created_style = if let Some(style_ctx) = style_ctx {
                        match style_ctx.today.signed_duration_since(created).num_days() {
                            d if (0..=7).contains(&d) && !single_global_style => {
                                base_style.clone().dim()
                            }
                            d if (8..=30).contains(&d) && !single_global_style => {
                                base_style.clone()
                            }
                            _ if !single_global_style => base_style.clone().bold(),
                            _ => base_style.clone(),
                        }
                    } else {
                        base_style.clone()
                    };
                    segments.push(created_style.apply_to(format!("{created}")).to_string());
                }
            }
            CreationCompletion::Completed { created, completed } => {
                segments.push(format!("x {completed}"));
                if let Some(created) = created {
                    segments.push(format!("{created}"));
                }
            }
        }

        for tag in &self.tags {
            let tag_style = if single_global_style {
                base_style.clone()
            } else {
                match tag.kind {
                    TagKind::Plus => base_style.clone().cyan(),
                    TagKind::Hash => base_style.clone().yellow(),
                    TagKind::Arobase => base_style.clone().blue(),
                }
            };
            segments.push(
                tag_style
                    .apply_to(format!("{}{}", tag.kind.as_ref(), tag.value))
                    .to_string(),
            );
        }

        segments.push(self.text.clone());

        for (attribute_key, attribute_value) in &self.attributes {
            let attribute_style = match attribute_key.as_str() {
                "due" if overdue && !single_global_style => base_style.clone().magenta(),
                _ if !single_global_style => base_style.clone().green(),
                _ => base_style.clone(),
            };
            segments.push(
                attribute_style
                    .apply_to(format!("{attribute_key}:{attribute_value}"))
                    .to_string(),
            );
        }

        if matches!(self.status, CreationCompletion::Completed { .. }) {
            if let Some(priority) = self.priority {
                segments.push(format!("pri:{priority}"));
            }
        }

        // Merge & color whole line
        let mut line = segments.join(" ");
        if let Some(style_ctx) = style_ctx {
            if matches!(self.status, CreationCompletion::Completed { .. }) {
                line = base_style
                    .clone()
                    .dim()
                    .strikethrough()
                    .apply_to(line)
                    .to_string();
            } else if self.is_blocked(style_ctx.other_tasks) {
                let mut style = base_style.clone().italic();
                if overdue {
                    style = style.magenta();
                }
                line = style.apply_to(line).to_string();
            } else if !self.is_pending(*style_ctx.today) {
                line = base_style.clone().dim().apply_to(line).to_string();
            }
        }
        line
    }
}

static TASK_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    RegexBuilder::new(
        r"
^
(
    (
        x\ (?<completed>\d{4}-\d{2}-\d{2})\ 
        ((?<completed_created>\d{4}-\d{2}-\d{2})\ )?
    )
    |
    (
        ((?<priority>\([A-Z]\))\ )?
        ((?<created>\d{4}-\d{2}-\d{2})\ )?
    )
)?
(?<text>.*)
$
",
    )
    .ignore_whitespace(true)
    .build()
    .unwrap()
});

static ATTRIBUTE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r" ?(\w+:[^\s]+)").unwrap());

static TAG_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r" ?([+#@][^\s]+)").unwrap());

/// Parse task from line
/// Last time I checked, existing parsers were not a good fit:
/// - `todotxt` (<https://crates.io/crates/todotxt>) is extremely buggy (not usable even for trivial stuff)
/// - `todo_lib` (<https://crates.io/crates/todo_lib>) does not separate task text from the rest
///
/// So roll our own using the regex crate, and unit tests to cover most cases
/// Note on error handling: we can unwrap (panic) if a regex result expectation is broken, but never if the error can be
/// reached with invalid input line.
impl FromStr for Task {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let caps = TASK_REGEX
            .captures(s)
            .ok_or_else(|| anyhow::anyhow!("Unable to parse {s:?}"))?;

        let mut text = caps
            .name("text")
            .map(|m| m.as_str())
            .unwrap_or_default()
            .to_owned();

        let mut priority = caps
            .name("priority")
            .map(|m| m.as_str().chars().nth(1).unwrap());

        let mut attributes = Vec::new();
        while let Some(attribute_match) = ATTRIBUTE_REGEX.find_iter(&text).next() {
            let (mut key, val) = attribute_match.as_str().split_once(':').unwrap();
            key = key.trim_start();
            if (key == "pri") && priority.is_none() {
                priority = Some(val.chars().next().unwrap());
            } else {
                attributes.push((key.to_owned(), val.to_owned()));
            }
            text.replace_range(attribute_match.range(), "");
            text = text.trim().to_owned();
        }

        let mut tags = Vec::new();
        #[allow(clippy::string_slice)] // if the regex succeeds, we know the first char is ascii
        while let Some(tag_match) = TAG_REGEX.find_iter(&text).next() {
            let tag_str = tag_match.as_str().trim_start();
            let kind = tag_str[0..1].parse().unwrap();
            tags.push(Tag {
                kind,
                value: tag_str[1..tag_str.len()].to_owned(),
            });
            text.replace_range(tag_match.range(), "");
            text = text.trim().to_owned();
        }

        let status = if let Some(completed) = caps.name("completed") {
            CreationCompletion::Completed {
                created: caps
                    .name("completed_created")
                    .map(|m| m.as_str().parse::<Date>())
                    .map_or(Ok(None), |v| v.map(Some))?,
                completed: completed.as_str().parse::<Date>()?,
            }
        } else {
            CreationCompletion::Pending {
                created: caps
                    .name("created")
                    .map(|m| m.as_str().parse::<Date>())
                    .map_or(Ok(None), |v| v.map(Some))?,
            }
        };

        Ok(Task {
            priority,
            status,
            tags,
            attributes,
            text,
            index: None,
        })
    }
}

#[cfg(test)]
#[allow(clippy::too_many_lines, clippy::shadow_unrelated)]
mod tests {
    use super::*;

    #[test]
    fn test_display_empty() {
        let task = Task::default();
        assert_eq!(task.to_string(None), "");
    }

    #[test]
    fn test_display_simple() {
        let task = Task {
            text: "task text".to_owned(),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "task text");
    }

    #[test]
    fn test_display_prio() {
        let task = Task {
            text: "task text".to_owned(),
            priority: Some('C'),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "(C) task text");
    }

    #[test]
    fn test_display_created_date() {
        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending {
                created: Some(Date::from_ymd_opt(2023, 8, 20).unwrap()),
            },
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "2023-08-20 task text");
    }

    #[test]
    fn test_display_completed() {
        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                created: None,
                completed: Date::from_ymd_opt(2023, 8, 21).unwrap(),
            },
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "x 2023-08-21 task text");
    }

    #[test]
    fn test_display_completed_created_date() {
        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                created: Some(Date::from_ymd_opt(2023, 8, 20).unwrap()),
                completed: Date::from_ymd_opt(2023, 8, 21).unwrap(),
            },
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "x 2023-08-21 2023-08-20 task text");
    }

    #[test]
    fn test_display_completed_priority() {
        let task = Task {
            priority: Some('D'),
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                created: None,
                completed: Date::from_ymd_opt(2023, 8, 21).unwrap(),
            },
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "x 2023-08-21 task text pri:D");
    }

    #[test]
    fn test_display_attributes() {
        let task = Task {
            text: "task text".to_owned(),

            attributes: vec![
                ("attr1".to_owned(), "v1".to_owned()),
                ("attr2".to_owned(), "v2".to_owned()),
                ("attr3".to_owned(), "v3".to_owned()),
                ("attr4".to_owned(), "v4".to_owned()),
                ("rec".to_owned(), "+3d".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.to_string(None),
            "task text attr1:v1 attr2:v2 attr3:v3 attr4:v4 rec:+3d"
        );
    }

    #[test]
    fn test_display_tags() {
        let task = Task {
            text: "task text".to_owned(),
            tags: vec![
                Tag {
                    kind: TagKind::Plus,
                    value: "tag1".to_owned(),
                },
                Tag {
                    kind: TagKind::Arobase,
                    value: "tag2".to_owned(),
                },
                Tag {
                    kind: TagKind::Hash,
                    value: "tag3".to_owned(),
                },
                Tag {
                    kind: TagKind::Plus,
                    value: "tag4".to_owned(),
                },
            ],
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "+tag1 @tag2 #tag3 +tag4 task text");
    }

    #[test]
    fn test_parse_empty() {
        assert_eq!("".parse::<Task>().unwrap(), Task::default());
    }

    #[test]
    fn test_parse_simple() {
        assert_eq!(
            "task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_prio() {
        assert_eq!(
            "(C) task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                priority: Some('C'),
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_created_date() {
        assert_eq!(
            "2023-08-20 task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(Date::from_ymd_opt(2023, 8, 20).unwrap())
                },
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_completed() {
        assert_eq!(
            "x 2023-08-21 task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Completed {
                    created: None,
                    completed: Date::from_ymd_opt(2023, 8, 21).unwrap()
                },
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_completed_created_date() {
        assert_eq!(
            "x 2023-08-21 2023-08-20 task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Completed {
                    created: Some(Date::from_ymd_opt(2023, 8, 20).unwrap()),
                    completed: Date::from_ymd_opt(2023, 8, 21).unwrap()
                },
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_completed_priority() {
        assert_eq!(
            "x 2023-08-21 task text pri:D".parse::<Task>().unwrap(),
            Task {
                priority: Some('D'),
                text: "task text".to_owned(),
                status: CreationCompletion::Completed {
                    created: None,

                    completed: Date::from_ymd_opt(2023, 8, 21).unwrap()
                },
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_attributes() {
        assert_eq!(
            "attr1:v1 attr2:v2 task text attr3:v3 attr4:v4 rec:+3d"
                .parse::<Task>()
                .unwrap(),
            Task {
                text: "task text".to_owned(),
                attributes: vec![
                    ("attr1".to_owned(), "v1".to_owned()),
                    ("attr2".to_owned(), "v2".to_owned()),
                    ("attr3".to_owned(), "v3".to_owned()),
                    ("attr4".to_owned(), "v4".to_owned()),
                    ("rec".to_owned(), "+3d".to_owned())
                ],
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_tags() {
        assert_eq!(
            "+tag1 @tag2 task text #tag3 +tag4".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                tags: vec![
                    Tag {
                        kind: TagKind::Plus,
                        value: "tag1".to_owned()
                    },
                    Tag {
                        kind: TagKind::Arobase,
                        value: "tag2".to_owned()
                    },
                    Tag {
                        kind: TagKind::Hash,
                        value: "tag3".to_owned()
                    },
                    Tag {
                        kind: TagKind::Plus,
                        value: "tag4".to_owned()
                    }
                ],
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_recurrence() {
        assert_eq!(
            "+3d".parse::<Recurrence>().unwrap(),
            Recurrence {
                delta: Duration::days(3),
                reference: RecurrenceReference::Task,
            }
        );
        assert_eq!(
            "10w".parse::<Recurrence>().unwrap(),
            Recurrence {
                delta: Duration::weeks(10),
                reference: RecurrenceReference::Completed,
            }
        );
    }

    #[test]
    fn test_recur() {
        let today = Date::from_ymd_opt(2023, 9, 9).unwrap();

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            ..Task::default()
        };
        assert_eq!(task.recur(today), None);

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![("rec".to_owned(), "+1w".to_owned())],
            ..Task::default()
        };
        assert_eq!(task.recur(today), None);

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("due".to_owned(), "2023-09-10".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("due".to_owned(), "2023-09-17".to_owned()),
                    ("rec".to_owned(), "+1w".to_owned()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("due".to_owned(), "2023-09-10".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
                ("started".to_owned(), "2023-09-08".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("due".to_owned(), "2023-09-17".to_owned()),
                    ("rec".to_owned(), "+1w".to_owned()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-08".to_owned()),
                ("due".to_owned(), "2023-09-10".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_owned(), "2023-09-15".to_owned()),
                    ("due".to_owned(), "2023-09-17".to_owned()),
                    ("rec".to_owned(), "+1w".to_owned()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-08".to_owned()),
                ("due".to_owned(), "2023-09-10".to_owned()),
                ("rec".to_owned(), "1w".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_owned(), "2023-09-14".to_owned()),
                    ("due".to_owned(), "2023-09-16".to_owned()),
                    ("rec".to_owned(), "1w".to_owned()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-08".to_owned()),
                ("rec".to_owned(), "1w".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_owned(), "2023-09-16".to_owned()),
                    ("rec".to_owned(), "1w".to_owned()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-08".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(today),
            Some(Task {
                text: "task text".to_owned(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_owned(), "2023-09-15".to_owned()),
                    ("rec".to_owned(), "+1w".to_owned()),
                ],
                ..Task::default()
            })
        );
    }

    #[test]
    fn test_is_same_recurring() {
        assert!(Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-15".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![
                ("t".to_owned(), "2023-09-16".to_owned()),
                ("rec".to_owned(), "1w".to_owned()),
            ],
            ..Task::default()
        }));

        assert!(Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-15".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![
                ("t".to_owned(), "2023-09-16".to_owned()),
                ("rec".to_owned(), "1w".to_owned()),
                ("started".to_owned(), "2023-09-18".to_owned())
            ],
            ..Task::default()
        }));

        assert!(!Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-15".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![("t".to_owned(), "2023-09-15".to_owned()),],
            ..Task::default()
        }));

        assert!(!Task {
            text: "task text".to_owned(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_owned(), "2023-09-15".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text!".to_owned(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![
                ("t".to_owned(), "2023-09-15".to_owned()),
                ("rec".to_owned(), "+1w".to_owned()),
            ],
            ..Task::default()
        }));
    }

    #[test]
    fn test_autogenerate_id() {
        assert_eq!(
            Task {
                priority: None,
                status: CreationCompletion::Pending { created: None },
                tags: vec![Tag {
                    kind: TagKind::Plus,
                    value: "AbCdEf".to_owned()
                }],
                attributes: vec![],
                text: "ghijkl MNOPQR stuVw".to_owned(),
                index: None
            }
            .id(),
            Some("abcgms".to_owned())
        );
    }
}
