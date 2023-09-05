//! Todo.txt task

use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;

use chrono::Duration;
use regex::{Regex, RegexBuilder};

type Date = chrono::naive::NaiveDate;

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
    Due,
    Done,
}

#[derive(Debug, Eq, PartialEq)]
struct Recurrence {
    delta: Duration,
    reference: RecurrenceReference,
}

const DATE_FORMAT: &str = "%Y-%m-%d";

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
            "+" => RecurrenceReference::Done,
            "" => RecurrenceReference::Due,
            _ => unreachable!(),
        };
        Ok(Self { delta, reference })
    }
}

impl Task {
    fn threshold_date(&self) -> Option<Date> {
        self.attributes
            .iter()
            .find(|a| a.0 == "t")
            .and_then(|a| Date::parse_from_str(&a.1, DATE_FORMAT).ok())
    }

    fn due_date(&self) -> Option<Date> {
        self.attributes
            .iter()
            .find(|a| a.0 == "due")
            .and_then(|a| Date::parse_from_str(&a.1, DATE_FORMAT).ok())
    }

    fn created_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Pending { created } => created,
            CreationCompletion::Completed { created, .. } => created,
        }
    }

    fn recurrence(&self) -> Option<Recurrence> {
        self.attributes
            .iter()
            .find(|a| a.0 == "rec")
            .and_then(|v| v.1.parse().ok())
    }

    fn set_done(&mut self, today: &Date) {
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

    fn recur(&self, today: &Date) -> Option<Self> {
        let due = match self.due_date() {
            Some(d) => d,
            None => {
                return None;
            }
        };
        self.recurrence().map(|r| {
            // Update status
            let status = CreationCompletion::Pending {
                created: Some(*today),
            };

            // Update attributes
            let mut attributes = self.attributes.clone();
            let ref_ = match r.reference {
                RecurrenceReference::Due => due,
                RecurrenceReference::Done => *today,
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

            Self {
                status,
                attributes,
                index: None,
                ..self.clone()
            }
        })
    }
}

impl fmt::Display for Task {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments = Vec::new();

        let today = today();

        let base_style = if cfg!(test) {
            dialoguer::console::Style::new().force_styling(false)
        } else {
            dialoguer::console::Style::new().for_stdout()
        };

        let before_threshold = self.threshold_date().map(|d| d > today).unwrap_or(false);
        let overdue = self.due_date().map(|d| d <= today).unwrap_or(false);

        match self.status {
            CreationCompletion::Pending { created } => {
                if let Some(priority) = self.priority {
                    let priority_style = match priority {
                        'A' if !before_threshold => base_style.clone().red(),
                        'B' if !before_threshold => base_style.clone().color256(9),
                        'C' if !before_threshold => base_style.clone().yellow(),
                        _ => base_style.clone(),
                    };
                    segments.push(priority_style.apply_to(format!("({priority})")).to_string());
                }
                if let Some(created) = created {
                    let created_style = match today.signed_duration_since(created).num_days() {
                        d if (0..=7).contains(&d) && !before_threshold => base_style.clone().dim(),
                        d if (8..=30).contains(&d) && !before_threshold => base_style.clone(),
                        _ if !before_threshold => base_style.clone().bold(),
                        _ => base_style.clone(),
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

        let tag_style = if !before_threshold {
            base_style.clone().cyan()
        } else {
            base_style.clone()
        };
        for tag in &self.tags {
            segments.push(
                tag_style
                    .apply_to(format!("{}{}", tag.kind.as_ref(), tag.value))
                    .to_string(),
            );
        }

        segments.push(self.text.clone());

        for (attribute_key, attribute_value) in &self.attributes {
            let attribute_style = match attribute_key.as_str() {
                "due" if overdue && !before_threshold => base_style.clone().magenta(),
                _ if !before_threshold => base_style.clone().green(),
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
        if matches!(self.status, CreationCompletion::Completed { .. }) {
            line = base_style
                .clone()
                .strikethrough()
                .apply_to(line)
                .to_string();
        } else if before_threshold {
            line = base_style.clone().dim().apply_to(line).to_string();
        }

        write!(f, "{}", line)
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

fn today() -> Date {
    chrono::Local::now().date_naive()
}

impl Ord for Task {
    fn cmp(&self, other: &Self) -> Ordering {
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

        // Due date
        let due = self.due_date();
        let other_due = other.due_date();
        match (due, other_due) {
            (Some(d), Some(od)) => {
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

        // Created date
        if let (Some(created), Some(other_created)) = (self.created_date(), other.created_date()) {
            return other_created.cmp(&created);
        }

        Ordering::Equal
    }
}

impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_empty() {
        let task = Task::default();
        assert_eq!(format!("{task}"), "");
    }

    #[test]
    fn test_display_simple() {
        let task = Task {
            text: "task text".to_string(),
            ..Task::default()
        };
        assert_eq!(format!("{task}"), "task text");
    }

    #[test]
    fn test_display_prio() {
        let task = Task {
            text: "task text".to_string(),
            priority: Some('C'),
            ..Task::default()
        };
        assert_eq!(format!("{task}"), "(C) task text");
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
        assert_eq!(format!("{task}"), "2023-08-20 task text");
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
        assert_eq!(format!("{task}"), "x 2023-08-21 task text");
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
        assert_eq!(format!("{task}"), "x 2023-08-21 2023-08-20 task text");
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
        assert_eq!(format!("{task}"), "x 2023-08-21 task text pri:D");
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
            format!("{task}"),
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
        assert_eq!(format!("{task}"), "+tag1 @tag2 #tag3 +tag4 task text");
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
            "3d".parse::<Recurrence>().unwrap(),
            Recurrence {
                delta: Duration::days(3),
                reference: RecurrenceReference::Due,
            }
        );
        assert_eq!(
            "+10w".parse::<Recurrence>().unwrap(),
            Recurrence {
                delta: Duration::weeks(10),
                reference: RecurrenceReference::Done,
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
            attributes: vec![("rec".to_string(), "1w".to_string())],
            ..Task::default()
        };
        assert_eq!(task.recur(&today), None);

        let task = Task {
            text: "task text".to_string(),
            status: CreationCompletion::Pending { created: None },
            attributes: vec![
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
                    ("due".to_string(), "2023-09-17".to_string()),
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
                    ("t".to_string(), "2023-09-15".to_string()),
                    ("due".to_string(), "2023-09-17".to_string()),
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
                    ("t".to_string(), "2023-09-14".to_string()),
                    ("due".to_string(), "2023-09-16".to_string()),
                    ("rec".to_string(), "+1w".to_string()),
                ],
                ..Task::default()
            })
        );
    }
}
