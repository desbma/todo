//! Todo.txt task

use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;

use regex::{Regex, RegexBuilder};

type Date = chrono::naive::NaiveDate;

#[derive(Debug, Eq, PartialEq, strum::EnumString, strum::AsRefStr)]
pub enum TagKind {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "#")]
    Hash,
    #[strum(serialize = "@")]
    Arobase,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Tag {
    kind: TagKind,
    value: String,
}

#[derive(Debug, Eq, PartialEq)]
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
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Task {
    pub priority: Option<char>,
    pub status: CreationCompletion,
    pub tags: Vec<Tag>,
    pub attributes: Vec<(String, String)>,
    pub text: String,
}

impl Task {
    fn threshold_date(&self) -> Option<Date> {
        self.attributes
            .iter()
            .find(|a| a.0 == "t")
            .and_then(|a| Date::parse_from_str(&a.1, "%Y-%m-%d").ok())
    }

    fn due_date(&self) -> Option<Date> {
        self.attributes
            .iter()
            .find(|a| a.0 == "due")
            .and_then(|a| Date::parse_from_str(&a.1, "%Y-%m-%d").ok())
    }

    fn created_date(&self) -> Option<Date> {
        match self.status {
            CreationCompletion::Pending { created } => created,
            CreationCompletion::Completed { created, .. } => created,
        }
    }
}

impl fmt::Display for Task {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments = Vec::new();

        match self.status {
            CreationCompletion::Pending { created } => {
                if let Some(priority) = self.priority {
                    segments.push(format!("({priority})"));
                }
                if let Some(created) = created {
                    segments.push(format!("{created}"));
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
            segments.push(format!("{}{}", tag.kind.as_ref(), tag.value));
        }

        segments.push(self.text.clone());

        for (attribute_key, attribute_value) in &self.attributes {
            segments.push(format!("{}:{}", attribute_key, attribute_value));
        }

        if matches!(self.status, CreationCompletion::Completed { .. }) {
            if let Some(priority) = self.priority {
                segments.push(format!("pri:{priority}"));
            }
        }

        write!(f, "{}", segments.join(" "))
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
}
