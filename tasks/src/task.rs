//! Todo.txt task

use std::{
    cmp::Ordering,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher as _},
    str::FromStr,
};

use chrono::Duration;
use nom::{
    IResult, Parser as _,
    branch::alt,
    bytes::complete::{take, take_while1},
    character::complete::{char, digit1, satisfy, space1},
    combinator::{map, map_opt, map_res, opt, peek, value, verify},
    sequence::{delimited, preceded, terminated},
};

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

impl TagKind {
    fn from_char(c: char) -> Option<Self> {
        let mut buf = [0_u8; 4];
        let s = c.encode_utf8(&mut buf);
        s.parse().ok()
    }
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

#[derive(Debug, Clone, Eq, PartialEq)]
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

/// Parse a recurrence string like `+3d`, `10w`, `1m`, `2y`
fn parse_recurrence(input: &str) -> IResult<&str, Recurrence> {
    let reference = alt((
        value(RecurrenceReference::Task, char('+')),
        value(RecurrenceReference::Completed, peek(digit1)),
    ));
    let val = map_res(digit1, |s: &str| s.parse::<i64>());
    let days_per_unit = alt((
        value(1_i64, char('d')),
        value(7_i64, char('w')),
        value(30_i64, char('m')),
        value(365_i64, char('y')),
    ));
    map(
        (reference, val, days_per_unit),
        |(reference, val, days_per_unit)| Recurrence {
            delta: Duration::days(val * days_per_unit),
            reference,
        },
    )
    .parse(input)
}

pub struct StyleContext<'a> {
    pub today: &'a Date,
    pub other_tasks: &'a [Task],
}

impl Task {
    /// Is task not completed and after threshold?
    #[must_use]
    pub fn is_pending(&self, today: Date) -> bool {
        match self.status {
            CreationCompletion::Pending { .. } => self.threshold_date().is_none_or(|t| today >= t),
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
            .and_then(|v| parse_recurrence(&v.1).ok().map(|(_, r)| r))
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
        #[expect(clippy::return_and_then)]
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
    #[expect(clippy::too_many_lines)]
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

        // Due date, if not before threshold
        match (due, other_due) {
            (Some(d), Some(od))
                if (d != od) && self.is_pending(today) && other.is_pending(today) =>
            {
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

fn parse_date_token(input: &str) -> IResult<&str, Date> {
    // Recognize exactly 10 chars matching digit patterns
    let date_str = verify(take(10_usize), |s: &str| {
        s.len() == 10
            && s.as_bytes().iter().enumerate().all(|(i, b)| match i {
                4 | 7 => *b == b'-',
                _ => b.is_ascii_digit(),
            })
    });
    map_opt(date_str, |s: &str| {
        Date::parse_from_str(s, DATE_FORMAT).ok()
    })
    .parse(input)
}

fn parse_priority(input: &str) -> IResult<&str, char> {
    delimited(char('('), satisfy(|c| c.is_ascii_uppercase()), char(')')).parse(input)
}

fn parse_completed_prefix(input: &str) -> IResult<&str, (Date, Option<Date>)> {
    let completed = preceded(
        terminated(char('x'), space1),
        terminated(parse_date_token, space1),
    );
    let created = opt(terminated(parse_date_token, space1));
    (completed, created).parse(input)
}

fn parse_pending_prefix(input: &str) -> IResult<&str, (Option<char>, Option<Date>)> {
    let priority = opt(terminated(parse_priority, space1));
    let created = opt(terminated(parse_date_token, space1));
    (priority, created).parse(input)
}

/// Tokens that can appear in the body of a task line
enum BodyToken<'a> {
    Attribute(&'a str, &'a str),
    Tag(TagKind, &'a str),
    Plain(&'a str),
}

/// Parse a single non-whitespace word
fn parse_word(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| !c.is_ascii_whitespace()).parse(input)
}

fn parse_attribute(input: &str) -> IResult<&str, (&str, &str)> {
    let atom = verify(parse_word, |s: &str| {
        // Must contain ':' that is not at position 0 and not at the end
        s.split_once(':').is_some_and(|(key, val)| {
            !key.is_empty()
                && !val.is_empty()
                && key.chars().all(|c| c.is_alphanumeric() || c == '_')
        })
    });
    map(atom, |s: &str| {
        let (key, val) = s.split_once(':').unwrap_or_default();
        (key, val)
    })
    .parse(input)
}

fn parse_tag(input: &str) -> IResult<&str, (TagKind, &str)> {
    // TagKind derives strum::EnumString with serialize attributes matching +, @, #
    let prefix = map_opt(
        satisfy(|c| matches!(c, '+' | '@' | '#')),
        TagKind::from_char,
    );
    let val = take_while1(|c: char| !c.is_ascii_whitespace());
    (prefix, val).parse(input)
}

fn parse_body_token(input: &str) -> IResult<&str, BodyToken<'_>> {
    alt((
        map(parse_attribute, |(k, v)| BodyToken::Attribute(k, v)),
        map(parse_tag, |(kind, val)| BodyToken::Tag(kind, val)),
        map(parse_word, BodyToken::Plain),
    ))
    .parse(input)
}

fn parse_prefix(input: &str) -> IResult<&str, (Option<char>, CreationCompletion)> {
    alt((
        map(parse_completed_prefix, |(completed, created)| {
            (None, CreationCompletion::Completed { created, completed })
        }),
        map(
            verify(parse_pending_prefix, |(pri, created)| {
                pri.is_some() || created.is_some()
            }),
            |(pri, created)| (pri, CreationCompletion::Pending { created }),
        ),
    ))
    .parse(input)
}

struct ParsedBody<'a> {
    tags: Vec<Tag>,
    attributes: Vec<(String, String)>,
    text_parts: Vec<&'a str>,
    pri_attr: Option<char>,
}

fn parse_body(input: &str) -> IResult<&str, ParsedBody<'_>> {
    let token = preceded(opt(space1), parse_body_token);
    let (rest, tokens) = nom::multi::many0(token).parse(input)?;

    let mut body = ParsedBody {
        tags: Vec::new(),
        attributes: Vec::new(),
        text_parts: Vec::new(),
        pri_attr: None,
    };
    for t in tokens {
        match t {
            BodyToken::Attribute("pri", val) if body.pri_attr.is_none() => {
                body.pri_attr = val.chars().next();
            }
            BodyToken::Attribute(key, val) => {
                body.attributes.push((key.to_owned(), val.to_owned()));
            }
            BodyToken::Tag(kind, val) => {
                body.tags.push(Tag {
                    kind,
                    value: val.to_owned(),
                });
            }
            BodyToken::Plain(word) => {
                body.text_parts.push(word);
            }
        }
    }

    Ok((rest, body))
}

fn parse_task(input: &str) -> IResult<&str, Task> {
    let (rest, (prefix_priority, status)) = opt(parse_prefix)
        .map(|o| o.unwrap_or((None, CreationCompletion::Pending { created: None })))
        .parse(input)?;

    let (rest, body) = parse_body(rest)?;
    let priority = prefix_priority.or(body.pri_attr);
    let text = body.text_parts.join(" ");

    Ok((
        rest,
        Task {
            priority,
            status,
            tags: body.tags,
            attributes: body.attributes,
            text,
            index: None,
        },
    ))
}

/// Parse task from line
/// Last time I checked, existing parsers were not a good fit:
/// - `todotxt` (<https://crates.io/crates/todotxt>) is extremely buggy (not usable even for trivial stuff)
/// - `todo_lib` (<https://crates.io/crates/todo_lib>) does not separate task text from the rest
///
/// So roll our own using nom combinators, and unit tests to cover most cases
impl FromStr for Task {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, task) = parse_task(s).map_err(|e| anyhow::anyhow!("Failed to parse task: {e}"))?;
        Ok(task)
    }
}

#[cfg(test)]
#[expect(clippy::too_many_lines, clippy::shadow_unrelated)]
mod tests {
    use std::{env, path::Path};

    use itertools::Itertools as _;

    use super::*;
    use crate::TodoFile;

    #[test]
    fn display_empty() {
        let task = Task::default();
        assert_eq!(task.to_string(None), "");
    }

    #[test]
    fn display_simple() {
        let task = Task {
            text: "task text".to_owned(),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "task text");
    }

    #[test]
    fn display_prio() {
        let task = Task {
            text: "task text".to_owned(),
            priority: Some('C'),
            ..Task::default()
        };
        assert_eq!(task.to_string(None), "(C) task text");
    }

    #[test]
    fn display_created_date() {
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
    fn display_completed() {
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
    fn display_completed_created_date() {
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
    fn display_completed_priority() {
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
    fn display_attributes() {
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
    fn display_tags() {
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
    fn parse_empty() {
        assert_eq!("".parse::<Task>().unwrap(), Task::default());
    }

    #[test]
    fn parse_simple() {
        assert_eq!(
            "task text".parse::<Task>().unwrap(),
            Task {
                text: "task text".to_owned(),
                ..Task::default()
            }
        );
    }

    #[test]
    fn parse_prio() {
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
    fn parse_created_date() {
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
    fn parse_completed() {
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
    fn parse_completed_created_date() {
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
    fn parse_completed_priority() {
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
    fn parse_attributes() {
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
    fn parse_tags() {
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
    fn parse_recurrence_basic() {
        assert_eq!(
            parse_recurrence("+3d").unwrap().1,
            Recurrence {
                delta: Duration::days(3),
                reference: RecurrenceReference::Task,
            }
        );
        assert_eq!(
            parse_recurrence("10w").unwrap().1,
            Recurrence {
                delta: Duration::weeks(10),
                reference: RecurrenceReference::Completed,
            }
        );
    }

    #[test]
    fn recur() {
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
    fn is_same_recurring() {
        assert!(
            Task {
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
            })
        );

        assert!(
            Task {
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
            })
        );

        assert!(
            !Task {
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
            })
        );

        assert!(
            !Task {
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
            })
        );
    }

    #[test]
    fn autogenerate_id() {
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

    #[test]
    fn cmp() {
        let _ = simple_logger::SimpleLogger::new().env().init();

        let todotxt_var = env::var_os("TODO_FILE");
        let Some(todotxt_path) = todotxt_var.as_ref().map(Path::new) else {
            return;
        };
        let done_var = env::var_os("DONE_FILE");
        let Some(done_path) = done_var.as_ref().map(Path::new) else {
            return;
        };
        let tasks = TodoFile::new(todotxt_path, done_path)
            .unwrap()
            .load_tasks()
            .unwrap();
        for ab in tasks.iter().permutations(2) {
            let a = ab[0];
            let b = ab[1];
            match a.cmp(b, &tasks) {
                Ordering::Less => {
                    assert_eq!(b.cmp(a, &tasks), Ordering::Greater);
                }
                Ordering::Equal => assert_eq!(b.cmp(a, &tasks), Ordering::Equal),
                Ordering::Greater => assert_eq!(b.cmp(a, &tasks), Ordering::Less),
            }
        }
        for abc in tasks.iter().permutations(3) {
            let a = abc[0];
            let b = abc[1];
            let c = abc[2];
            log::trace!("a = {a:?}");
            log::trace!("b = {b:?}");
            log::trace!("c = {c:?}");
            #[expect(clippy::match_same_arms)]
            match (a.cmp(b, &tasks), b.cmp(c, &tasks)) {
                // a < b, b < c => a < c
                (Ordering::Less, Ordering::Less) => assert_eq!(a.cmp(c, &tasks), Ordering::Less),
                // a < b, b = c => a < c
                (Ordering::Less, Ordering::Equal) => assert_eq!(a.cmp(c, &tasks), Ordering::Less),
                // a < b, b > c => ?
                (Ordering::Less, Ordering::Greater) => {}
                // a = b, b < c => a < c
                (Ordering::Equal, Ordering::Less) => assert_eq!(a.cmp(c, &tasks), Ordering::Less),
                // a = b, b = c => a = c
                (Ordering::Equal, Ordering::Equal) => assert_eq!(a.cmp(c, &tasks), Ordering::Equal),
                // a = b, b > c => a > c
                (Ordering::Equal, Ordering::Greater) => {
                    assert_eq!(a.cmp(c, &tasks), Ordering::Greater);
                }
                // a > b, b < c => ?
                (Ordering::Greater, Ordering::Less) => {}
                // a > b, b = c => a > c
                (Ordering::Greater, Ordering::Equal) => {
                    assert_eq!(a.cmp(c, &tasks), Ordering::Greater);
                }
                // a > b, b > c => a > c
                (Ordering::Greater, Ordering::Greater) => {
                    assert_eq!(a.cmp(c, &tasks), Ordering::Greater);
                }
            }
        }
    }

    // --- Parser helper tests ---

    #[test]
    fn date_token_valid() {
        let (rest, d) = parse_date_token("2023-08-20 foo").unwrap();
        assert_eq!(d, Date::from_ymd_opt(2023, 8, 20).unwrap());
        assert_eq!(rest, " foo");
    }

    #[test]
    fn date_token_invalid_month() {
        assert!(parse_date_token("2023-13-01").is_err());
    }

    #[test]
    fn date_token_malformed_separators() {
        assert!(parse_date_token("2023/08/20").is_err());
    }

    #[test]
    fn priority_token_valid() {
        let (rest, p) = parse_priority("(A) foo").unwrap();
        assert_eq!(p, 'A');
        assert_eq!(rest, " foo");
    }

    #[test]
    fn priority_token_lowercase_rejection() {
        assert!(parse_priority("(a)").is_err());
    }

    #[test]
    fn priority_token_malformed() {
        assert!(parse_priority("A)").is_err());
        assert!(parse_priority("(A").is_err());
    }

    #[test]
    fn completed_prefix_with_created() {
        let (rest, (completed, created)) =
            parse_completed_prefix("x 2023-08-21 2023-08-20 task").unwrap();
        assert_eq!(completed, Date::from_ymd_opt(2023, 8, 21).unwrap());
        assert_eq!(created, Some(Date::from_ymd_opt(2023, 8, 20).unwrap()));
        assert_eq!(rest, "task");
    }

    #[test]
    fn completed_prefix_without_created() {
        let (rest, (completed, created)) = parse_completed_prefix("x 2023-08-21 task").unwrap();
        assert_eq!(completed, Date::from_ymd_opt(2023, 8, 21).unwrap());
        assert_eq!(created, None);
        assert_eq!(rest, "task");
    }

    #[test]
    fn pending_prefix_priority_only() {
        let (rest, (pri, created)) = parse_pending_prefix("(B) task").unwrap();
        assert_eq!(pri, Some('B'));
        assert_eq!(created, None);
        assert_eq!(rest, "task");
    }

    #[test]
    fn pending_prefix_created_only() {
        let (rest, (pri, created)) = parse_pending_prefix("2023-08-20 task").unwrap();
        assert_eq!(pri, None);
        assert_eq!(created, Some(Date::from_ymd_opt(2023, 8, 20).unwrap()));
        assert_eq!(rest, "task");
    }

    #[test]
    fn pending_prefix_both() {
        let (rest, (pri, created)) = parse_pending_prefix("(A) 2023-08-20 task").unwrap();
        assert_eq!(pri, Some('A'));
        assert_eq!(created, Some(Date::from_ymd_opt(2023, 8, 20).unwrap()));
        assert_eq!(rest, "task");
    }

    #[test]
    fn pending_prefix_empty() {
        let (rest, (pri, created)) = parse_pending_prefix("task").unwrap();
        assert_eq!(pri, None);
        assert_eq!(created, None);
        assert_eq!(rest, "task");
    }

    #[test]
    fn attribute_standard() {
        let (rest, (k, v)) = parse_attribute("due:2023-09-10 foo").unwrap();
        assert_eq!(k, "due");
        assert_eq!(v, "2023-09-10");
        assert_eq!(rest, " foo");
    }

    #[test]
    fn attribute_numeric_underscore_key() {
        let (rest, (k, v)) = parse_attribute("key_2:val").unwrap();
        assert_eq!(k, "key_2");
        assert_eq!(v, "val");
        assert_eq!(rest, "");
    }

    #[test]
    fn attribute_missing_value_rejection() {
        assert!(parse_attribute("key:").is_err());
    }

    #[test]
    fn tag_plus() {
        let (rest, (kind, val)) = parse_tag("+project foo").unwrap();
        assert_eq!(kind, TagKind::Plus);
        assert_eq!(val, "project");
        assert_eq!(rest, " foo");
    }

    #[test]
    fn tag_hash() {
        let (rest, (kind, val)) = parse_tag("#label").unwrap();
        assert_eq!(kind, TagKind::Hash);
        assert_eq!(val, "label");
        assert_eq!(rest, "");
    }

    #[test]
    fn tag_arobase() {
        let (rest, (kind, val)) = parse_tag("@context").unwrap();
        assert_eq!(kind, TagKind::Arobase);
        assert_eq!(val, "context");
        assert_eq!(rest, "");
    }

    #[test]
    fn tag_prefix_only_rejection() {
        assert!(parse_tag("+ ").is_err());
    }

    #[test]
    fn plain_word_normal() {
        let (rest, w) = parse_word("hello world").unwrap();
        assert_eq!(w, "hello");
        assert_eq!(rest, " world");
    }

    #[test]
    fn plain_word_punctuation() {
        let (rest, w) = parse_word("hello! world").unwrap();
        assert_eq!(w, "hello!");
        assert_eq!(rest, " world");
    }

    #[test]
    fn recurrence_all_units() {
        assert_eq!(
            parse_recurrence("+3d").unwrap().1,
            Recurrence {
                delta: Duration::days(3),
                reference: RecurrenceReference::Task,
            }
        );
        assert_eq!(
            parse_recurrence("10w").unwrap().1,
            Recurrence {
                delta: Duration::weeks(10),
                reference: RecurrenceReference::Completed,
            }
        );
        assert_eq!(
            parse_recurrence("2m").unwrap().1,
            Recurrence {
                delta: Duration::days(60),
                reference: RecurrenceReference::Completed,
            }
        );
        assert_eq!(
            parse_recurrence("+1y").unwrap().1,
            Recurrence {
                delta: Duration::days(365),
                reference: RecurrenceReference::Task,
            }
        );
    }

    #[test]
    fn recurrence_invalid() {
        assert!(parse_recurrence("abc").is_err());
        assert!(parse_recurrence("3x").is_err());
        assert!(parse_recurrence("d").is_err());
    }

    // --- Integration / parity tests ---

    #[test]
    fn parse_prefix_interleaved_tags_attrs_text() {
        let task = "(A) 2023-08-20 +proj task text due:2023-09-10 @ctx more #lbl"
            .parse::<Task>()
            .unwrap();
        assert_eq!(task.priority, Some('A'));
        assert_eq!(
            task.status,
            CreationCompletion::Pending {
                created: Some(Date::from_ymd_opt(2023, 8, 20).unwrap())
            }
        );
        assert_eq!(task.text, "task text more");
        assert_eq!(task.tags.len(), 3);
        assert_eq!(task.attributes.len(), 1);
        assert_eq!(
            task.attributes[0],
            ("due".to_owned(), "2023-09-10".to_owned())
        );
    }

    #[test]
    fn parse_repeated_pri_attribute() {
        let task = "x 2023-08-21 task pri:A pri:B".parse::<Task>().unwrap();
        assert_eq!(task.priority, Some('A'));
        assert_eq!(task.attributes.len(), 1);
        assert_eq!(task.attributes[0], ("pri".to_owned(), "B".to_owned()));
    }

    #[test]
    fn parse_extra_spaces() {
        let task = "  task   text  ".parse::<Task>().unwrap();
        assert_eq!(task.text, "task text");
    }

    #[test]
    fn roundtrip_pending() {
        let input = "(B) 2023-08-20 +proj @ctx task text due:2023-09-10 rec:+1w";
        let task = input.parse::<Task>().unwrap();
        let output = task.to_string(None);
        let reparsed = output.parse::<Task>().unwrap();
        assert_eq!(task, reparsed);
    }

    #[test]
    fn roundtrip_completed() {
        let input = "x 2023-08-21 2023-08-20 +proj task text due:2023-09-10 pri:C";
        let task = input.parse::<Task>().unwrap();
        let output = task.to_string(None);
        let reparsed = output.parse::<Task>().unwrap();
        assert_eq!(task, reparsed);
    }

    #[test]
    fn tags_attrs_from_middle() {
        let task = "word1 +tag1 word2 key:val word3 @ctx word4"
            .parse::<Task>()
            .unwrap();
        assert_eq!(task.text, "word1 word2 word3 word4");
        assert_eq!(task.tags.len(), 2);
        assert_eq!(task.attributes.len(), 1);
    }
}
