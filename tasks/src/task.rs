//! Todo.txt task

use std::cmp::Ordering;
use std::str::FromStr;

use chrono::Duration;
use regex::{Regex, RegexBuilder};

pub type Date = chrono::naive::NaiveDate;

fn today() -> Date {
    chrono::Local::now().date_naive()
}

#[derive(Debug, Clone, Eq, PartialEq, strum::EnumString, strum::AsRefStr)]
pub enum TagKind {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "#")]
    Hash,
    #[strum(serialize = "@")]
    Arobase,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Tag {
    kind: TagKind,
    value: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
#[derive(Debug, Clone, Default, Eq, PartialEq)]
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

lazy_static::lazy_static! {
    static ref REC_REGEX: Regex = Regex::new(r"(?<ref>\+?)(?<val>\d+)(?<unit>d|w|m|y)").unwrap();
}

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
    pub fn is_pending(&self, today: &Date) -> bool {
        match self.status {
            CreationCompletion::Pending { .. } => {
                self.threshold_date().map(|t| t <= *today).unwrap_or(true)
            }
            CreationCompletion::Completed { .. } => false,
        }
    }

    /// Is task depending on another pending task?
    fn is_blocked(&self, others: &[Task]) -> bool {
        self.depends_on().iter().any(|id| {
            others.iter().any(|t| {
                matches!(t.status, CreationCompletion::Pending { .. })
                    && t.attribute("id") == Some(*id)
            })
        })
    }

    /// Is task ready to be worked on?
    pub fn is_ready(&self, today: &Date, others: &[Task]) -> bool {
        self.is_pending(today) && !self.is_blocked(others)
    }

    fn attribute(&self, name: &str) -> Option<&str> {
        self.attributes
            .iter()
            .find(|a| a.0 == name)
            .map(|a| a.1.as_str())
    }

    fn depends_on(&self) -> Vec<&str> {
        self.attribute("dep")
            .map(|d| d.split(',').collect())
            .unwrap_or_default()
    }

    pub fn threshold_date(&self) -> Option<Date> {
        self.attribute("t")
            .and_then(|t| Date::parse_from_str(t, DATE_FORMAT).ok())
    }

    pub fn due_date(&self) -> Option<Date> {
        self.attribute("due")
            .and_then(|d| Date::parse_from_str(d, DATE_FORMAT).ok())
    }

    pub fn created_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Pending { created } => created,
            CreationCompletion::Completed { created, .. } => created,
        }
    }

    pub fn completed_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Pending { .. } => None,
            CreationCompletion::Completed { completed, .. } => Some(completed),
        }
    }

    pub fn recurrence(&self) -> Option<Recurrence> {
        self.attributes
            .iter()
            .find(|a| RECURRENCE_TAG_KEYS.contains(&a.0.as_str()))
            .and_then(|v| v.1.parse().ok())
    }

    pub fn set_done(&mut self, today: &Date) {
        match self.status {
            CreationCompletion::Pending { created } => {
                self.status = CreationCompletion::Completed {
                    created,
                    completed: *today,
                };
            }
            CreationCompletion::Completed { .. } => panic!("Aleady completed"),
        }
    }

    pub fn recur(&self, today: &Date) -> Option<Self> {
        self.recurrence().and_then(|r| {
            // Update attributes
            let mut attributes = self.attributes.clone();
            if let Some(due) = self.due_date() {
                let ref_ = match r.reference {
                    RecurrenceReference::Task => due,
                    RecurrenceReference::Completed => *today,
                };
                attributes.iter_mut().find(|a| a.0 == "due").unwrap().1 =
                    (ref_ + r.delta).format(DATE_FORMAT).to_string();
                if let Some(a) = attributes.iter_mut().find(|a| a.0 == "t") {
                    let threshold = self.threshold_date().unwrap();
                    // New threshold is new due date - delta betwwen old due and threshold
                    a.1 = ((ref_ + r.delta) - (due - threshold))
                        .format(DATE_FORMAT)
                        .to_string();
                }
            } else if let Some(threshold) = self.threshold_date() {
                let ref_ = match r.reference {
                    RecurrenceReference::Task => threshold,
                    RecurrenceReference::Completed => *today,
                };
                attributes.iter_mut().find(|a| a.0 == "t").unwrap().1 =
                    (ref_ + r.delta).format(DATE_FORMAT).to_string();
            } else {
                return None;
            }

            // Update status
            let status = CreationCompletion::Pending {
                created: Some(*today),
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
    pub fn start(&mut self, today: &Date) {
        // Add/update started attribute
        let started_date = today.format(DATE_FORMAT).to_string();
        if let Some(started) = self.attributes.iter_mut().find(|a| a.0 == "started") {
            started.1 = started_date;
        } else {
            self.attributes.push(("started".to_string(), started_date))
        }
    }

    /// Compare tasks for sorting
    pub fn cmp(&self, other: &Self, others: &[Self]) -> Ordering {
        // Completed is obviously less urgent than pending
        match (&self.status, &other.status) {
            (CreationCompletion::Completed { .. }, CreationCompletion::Pending { .. }) => {
                return Ordering::Less;
            }
            (CreationCompletion::Pending { .. }, CreationCompletion::Completed { .. }) => {
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
            (Some(d), Some(od))
                if d < chrono::Duration::zero() && od >= chrono::Duration::zero() =>
            {
                return Ordering::Less;
            }
            (Some(d), Some(od))
                if d >= chrono::Duration::zero() && od < chrono::Duration::zero() =>
            {
                return Ordering::Greater;
            }
            (Some(d), None) if d < chrono::Duration::zero() => {
                return Ordering::Less;
            }
            (None, Some(od)) if od < chrono::Duration::zero() => {
                return Ordering::Greater;
            }
            _ => (),
        }

        // Being blocked is less urgent that not being
        match (self.is_blocked(others), other.is_blocked(others)) {
            (true, false) => {
                return Ordering::Less;
            }
            (false, true) => {
                return Ordering::Greater;
            }
            _ => (),
        }

        // Due date
        let due = self.due_date();
        let other_due = other.due_date();
        match (due, other_due) {
            (Some(d), Some(od)) if d != od => {
                return od.cmp(&d);
            }
            (Some(d), None) if d <= today => {
                return Ordering::Greater;
            }
            (None, Some(od)) if od <= today => {
                return Ordering::Less;
            }
            _ => (),
        }

        // Explicit priority, no priority is less urgent than 'D' priority
        match (self.priority, other.priority) {
            (Some(p), Some(op)) if p != op => {
                return op.cmp(&p);
            }
            (Some(p), None) if p < 'D' => {
                return Ordering::Greater;
            }
            (None, Some(op)) if op < 'D' => {
                return Ordering::Less;
            }
            _ => (),
        }

        // Having due date is more important than not having one, if not before threshold
        match (due, other_due) {
            (Some(_), None) if self.is_pending(&today) => {
                return Ordering::Greater;
            }
            (None, Some(_)) if other.is_pending(&today) => {
                return Ordering::Less;
            }
            _ => (),
        }

        // Created date
        if let (Some(created), Some(other_created)) = (self.created_date(), other.created_date()) {
            return other_created.cmp(&created);
        }

        Ordering::Equal
    }

    pub fn to_string(&self, style_ctx: Option<&StyleContext>) -> String {
        let mut segments = Vec::new();

        let (base_style, overdue, single_global_style) = if let Some(style_ctx) = style_ctx {
            (
                console::Style::new().for_stdout(),
                self.due_date()
                    .map(|d| d <= *style_ctx.today)
                    .unwrap_or(false),
                !self.is_ready(style_ctx.today, style_ctx.other_tasks),
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
                    .apply_to(format!("{}:{}", attribute_key, attribute_value))
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
                line = base_style.clone().italic().apply_to(line).to_string();
            } else if !self.is_pending(style_ctx.today) {
                line = base_style.clone().dim().apply_to(line).to_string();
            }
        }
        line
    }
}

lazy_static::lazy_static! {
    static ref TASK_REGEX: Regex = RegexBuilder::new(r"
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
").ignore_whitespace(true).build().unwrap();

    static ref ATTRIBUTE_REGEX: Regex = Regex::new(r" ?(\w+:[^\s]+)").unwrap();

    static ref TAG_REGEX: Regex = Regex::new(r" ?([+#@][^\s]+)").unwrap();
}

/// Parse task from line
/// Last time I checked, existing parsers were not a good fit:
/// - todotxt (https://crates.io/crates/todotxt) is extremely buggy (not usable even for trivial stuff)
/// - todo_lib (https://crates.io/crates/todo_lib) does not separate task text from the rest
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
            .to_string();

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
                attributes.push((key.to_string(), val.to_string()));
            }
            text.replace_range(attribute_match.range(), "");
            text = text.trim().to_string();
        }

        let mut tags = Vec::new();
        while let Some(tag_match) = TAG_REGEX.find_iter(&text).next() {
            let tag_str = tag_match.as_str().trim_start();
            let kind = tag_str[0..1].parse().unwrap();
            tags.push(Tag {
                kind,
                value: tag_str[1..tag_str.len()].to_string(),
            });
            text.replace_range(tag_match.range(), "");
            text = text.trim().to_string();
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
            text: "task text".to_string(),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "task text");
    }

    #[test]
    fn test_display_prio() {
        let task = Task {
            text: "task text".to_string(),
            priority: Some('C'),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "(C) task text");
    }

    #[test]
    fn test_display_created_date() {
        let task = Task {
            text: "task text".to_string(),
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
            text: "task text".to_string(),
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
            text: "task text".to_string(),
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
            text: "task text".to_string(),
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
            text: "task text".to_string(),

            attributes: vec![
                ("attr1".to_string(), "v1".to_string()),
                ("attr2".to_string(), "v2".to_string()),
                ("attr3".to_string(), "v3".to_string()),
                ("attr4".to_string(), "v4".to_string()),
                ("rec".to_string(), "+3d".to_string()),
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
            text: "task text".to_string(),
            tags: vec![
                Tag {
                    kind: TagKind::Plus,
                    value: "tag1".to_string(),
                },
                Tag {
                    kind: TagKind::Arobase,
                    value: "tag2".to_string(),
                },
                Tag {
                    kind: TagKind::Hash,
                    value: "tag3".to_string(),
                },
                Tag {
                    kind: TagKind::Plus,
                    value: "tag4".to_string(),
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
                text: "task text".to_string(),
                ..Task::default()
            }
        );
    }

    #[test]
    fn test_parse_prio() {
        assert_eq!(
            "(C) task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_string(),
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
                text: "task text".to_string(),
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
                text: "task text".to_string(),
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
                text: "task text".to_string(),
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
                text: "task text".to_string(),
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
                text: "task text".to_string(),
                attributes: vec![
                    ("attr1".to_string(), "v1".to_string()),
                    ("attr2".to_string(), "v2".to_string()),
                    ("attr3".to_string(), "v3".to_string()),
                    ("attr4".to_string(), "v4".to_string()),
                    ("rec".to_string(), "+3d".to_string())
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
                text: "task text".to_string(),
                tags: vec![
                    Tag {
                        kind: TagKind::Plus,
                        value: "tag1".to_string()
                    },
                    Tag {
                        kind: TagKind::Arobase,
                        value: "tag2".to_string()
                    },
                    Tag {
                        kind: TagKind::Hash,
                        value: "tag3".to_string()
                    },
                    Tag {
                        kind: TagKind::Plus,
                        value: "tag4".to_string()
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
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            ..Task::default()
        };
        assert_eq!(task.recur(&today), None);

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![("rec".to_string(), "+1w".to_string())],
            ..Task::default()
        };
        assert_eq!(task.recur(&today), None);

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("due".to_string(), "2023-09-10".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(&today),
            Some(Task {
                text: "task text".to_string(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("due".to_string(), "2023-09-17".to_string()),
                    ("rec".to_string(), "+1w".to_string()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-08".to_string()),
                ("due".to_string(), "2023-09-10".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(&today),
            Some(Task {
                text: "task text".to_string(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_string(), "2023-09-15".to_string()),
                    ("due".to_string(), "2023-09-17".to_string()),
                    ("rec".to_string(), "+1w".to_string()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-08".to_string()),
                ("due".to_string(), "2023-09-10".to_string()),
                ("rec".to_string(), "1w".to_string()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(&today),
            Some(Task {
                text: "task text".to_string(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_string(), "2023-09-14".to_string()),
                    ("due".to_string(), "2023-09-16".to_string()),
                    ("rec".to_string(), "1w".to_string()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-08".to_string()),
                ("rec".to_string(), "1w".to_string()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(&today),
            Some(Task {
                text: "task text".to_string(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_string(), "2023-09-16".to_string()),
                    ("rec".to_string(), "1w".to_string()),
                ],
                ..Task::default()
            })
        );

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-08".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        };
        assert_eq!(
            task.recur(&today),
            Some(Task {
                text: "task text".to_string(),
                status: CreationCompletion::Pending {
                    created: Some(today)
                },
                attributes: vec![
                    ("t".to_string(), "2023-09-15".to_string()),
                    ("rec".to_string(), "+1w".to_string()),
                ],
                ..Task::default()
            })
        );
    }

    #[test]
    fn test_is_same_recurring() {
        assert!(Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-15".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text".to_string(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![
                ("t".to_string(), "2023-09-16".to_string()),
                ("rec".to_string(), "1w".to_string()),
            ],
            ..Task::default()
        }));

        assert!(!Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-15".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text".to_string(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![("t".to_string(), "2023-09-15".to_string()),],
            ..Task::default()
        }));

        assert!(!Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
                ("t".to_string(), "2023-09-15".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        }
        .is_same_recurring(&Task {
            text: "task text!".to_string(),
            status: CreationCompletion::Completed {
                completed: today(),
                created: None
            },
            attributes: vec![
                ("t".to_string(), "2023-09-15".to_string()),
                ("rec".to_string(), "+1w".to_string()),
            ],
            ..Task::default()
        }));
    }
}
