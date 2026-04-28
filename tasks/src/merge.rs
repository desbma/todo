//! Conflict file resolution via semantic merge

use std::{collections::HashSet, fs, path::Path};

use crate::{
    TodoFile,
    task::{CreationCompletion, Identifier, MergeKey, Task},
};

/// Describes which side a task came from during matching
#[derive(Debug)]
enum Matched<'a> {
    Both(&'a Task, &'a Task),
    LocalOnly(&'a Task),
    ConflictOnly(&'a Task),
}

/// Match tasks between two lists using `MergeKey`
/// Returns `None` if the merge cannot be performed safely
fn match_tasks<'a>(local: &'a [Task], conflict: &'a [Task]) -> Option<Vec<Matched<'a>>> {
    let mut result = Vec::new();
    let mut local_matched: HashSet<usize> = HashSet::new();
    let mut conflict_matched: HashSet<usize> = HashSet::new();

    // Bail out if a recurring task lacks occurrence-level identity
    for t in local.iter().chain(conflict.iter()) {
        if t.merge_key().is_none() {
            log::warn!(
                "Recurring task without due/threshold anchor, cannot determine occurrence identity: {:?}",
                t.text
            );
            return None;
        }
    }

    for (li, lt) in local.iter().enumerate() {
        let local_key = lt.merge_key()?;
        let candidates: Vec<_> = conflict
            .iter()
            .enumerate()
            .filter(|(ci, _)| !conflict_matched.contains(ci))
            .filter(|(_, ct)| ct.merge_key().as_ref() == Some(&local_key))
            .collect();
        if candidates.len() == 1 {
            let &(ci, ct) = candidates.first().unwrap();
            result.push(Matched::Both(lt, ct));
            local_matched.insert(li);
            conflict_matched.insert(ci);
        } else if candidates.len() > 1 {
            match local_key {
                MergeKey::NonRecurring(Identifier::Id(id)) => {
                    log::warn!("Ambiguous id:{id} in conflict file, aborting merge");
                    return None;
                }
                MergeKey::Recurring { identifier, .. } => {
                    log::warn!("Ambiguous recurring occurrence for {identifier:?}, aborting merge");
                    return None;
                }
                MergeKey::NonRecurring(Identifier::Text(_)) => {
                    log::info!(
                        "Multiple fingerprint matches for {:?}, keeping all as separate",
                        lt.text
                    );
                }
            }
        }
    }

    for (li, lt) in local.iter().enumerate() {
        if !local_matched.contains(&li) {
            result.push(Matched::LocalOnly(lt));
        }
    }
    for (ci, ct) in conflict.iter().enumerate() {
        if !conflict_matched.contains(&ci) {
            result.push(Matched::ConflictOnly(ct));
        }
    }

    Some(result)
}

pub fn resolve_conflict(
    local_todo_path: &Path,
    local_done_path: &Path,
    todo_conflict_path: Option<&Path>,
    done_conflict_path: Option<&Path>,
) -> anyhow::Result<usize> {
    let todo_file = TodoFile::new(local_todo_path, local_done_path)?;
    let (local_todo, local_done) = todo_file.load_all_tasks_unindexed()?;
    let conflict_todo = todo_conflict_path
        .map(TodoFile::load_tasks_unindexed)
        .transpose()?
        .unwrap_or_default();
    let conflict_done = done_conflict_path
        .map(TodoFile::load_tasks_unindexed)
        .transpose()?
        .unwrap_or_default();

    let local_all: Vec<_> = local_todo
        .iter()
        .chain(local_done.iter())
        .cloned()
        .collect();
    let conflict_all: Vec<_> = conflict_todo
        .iter()
        .chain(conflict_done.iter())
        .cloned()
        .collect();

    let Some(matches) = match_tasks(&local_all, &conflict_all) else {
        anyhow::bail!("Cannot safely resolve conflict: ambiguous task matches detected");
    };

    let mut merged_count = 0_usize;
    let mut merged_todo: Vec<Task> = Vec::new();
    let mut merged_done: Vec<Task> = Vec::new();

    for m in &matches {
        let task = match m {
            Matched::Both(local, conflict) => {
                let merged = local.merge(conflict);
                if **local != merged {
                    merged_count += 1;
                }
                merged
            }
            Matched::LocalOnly(t) => (*t).clone(),
            Matched::ConflictOnly(t) => {
                merged_count += 1;
                (*t).clone()
            }
        };
        match task.status {
            CreationCompletion::Completed { .. } => merged_done.push(task),
            CreationCompletion::Pending { .. } => merged_todo.push(task),
        }
    }

    todo_file.save_all_tasks(&merged_todo, &merged_done)?;

    // Remove conflict files only after both destination files have been written
    for path in [todo_conflict_path, done_conflict_path]
        .into_iter()
        .flatten()
    {
        fs::remove_file(path)?;
    }

    log::info!("Resolved conflict: {merged_count} task(s) merged");
    Ok(merged_count)
}

#[cfg(test)]
mod tests {
    use std::{fs, io::Write as _, path::Path};

    use tempfile::NamedTempFile;

    use super::*;

    fn write_file(lines: &[&str]) -> NamedTempFile {
        let mut f = NamedTempFile::new().unwrap();
        for line in lines {
            writeln!(f, "{line}").unwrap();
        }
        f
    }

    fn load_tasks_from_path(path: &Path) -> anyhow::Result<Vec<Task>> {
        TodoFile::load_tasks_unindexed(path)
    }

    #[test]
    fn identical_files_no_changes() {
        let todo = write_file(&["(A) task one", "(B) task two"]);
        let done = write_file(&[]);
        let conflict = write_file(&["(A) task one", "(B) task two"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 0);
        assert!(!conflict.path().exists());
    }

    #[test]
    fn resolve_conflict_creates_todo_backup() {
        let todo = write_file(&["task one"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one", "task two"]);

        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();

        let backup_path = todo.path().with_file_name(format!(
            "{}.bak.1",
            todo.path().file_name().unwrap().to_string_lossy(),
        ));
        assert!(backup_path.exists());
        assert_eq!(fs::read_to_string(backup_path).unwrap(), "task one\n");
    }

    #[test]
    fn conflict_adds_new_task() {
        let todo = write_file(&["task one"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one", "task two"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks.len(), 2);
        assert!(tasks.iter().any(|t| t.text == "task one"));
        assert!(tasks.iter().any(|t| t.text == "task two"));
    }

    #[test]
    fn local_adds_new_task() {
        let todo = write_file(&["task one", "task two"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 0);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks.len(), 2);
    }

    #[test]
    fn both_add_different_tasks() {
        let todo = write_file(&["common", "local new"]);
        let done = write_file(&[]);
        let conflict = write_file(&["common", "conflict new"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks.len(), 3);
    }

    #[test]
    fn conflict_marks_done() {
        let todo = write_file(&["task to complete"]);
        let done = write_file(&[]);
        let conflict = write_file(&["x 2024-06-15 task to complete"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        // Task should be in done, not in todo
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert!(todo_tasks.is_empty());
        assert_eq!(done_tasks.len(), 1);
        assert!(matches!(
            done_tasks[0].status,
            CreationCompletion::Completed { .. }
        ));
    }

    #[test]
    fn local_marks_done_conflict_modifies() {
        // Local has the task completed, conflict has it pending with extra attributes
        let todo = write_file(&[]);
        let done = write_file(&["x 2024-06-15 task one"]);
        let conflict = write_file(&["task one due:2024-07-01"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        // Completed wins
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(done_tasks.len(), 1);
        assert!(matches!(
            done_tasks[0].status,
            CreationCompletion::Completed { .. }
        ));
        // But extra attributes from conflict should be kept
        assert!(done_tasks[0].attributes.iter().any(|(k, _)| k == "due"));
    }

    #[test]
    fn match_by_explicit_id() {
        let todo = write_file(&["task local text id:abc"]);
        let done = write_file(&[]);
        let conflict = write_file(&["x 2024-06-15 task local text id:abc"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(done_tasks.len(), 1);
        assert!(
            done_tasks[0]
                .attributes
                .iter()
                .any(|(k, v)| k == "id" && v == "abc")
        );
    }

    #[test]
    fn priority_merge_keeps_higher() {
        let todo = write_file(&["(C) task one"]);
        let done = write_file(&[]);
        let conflict = write_file(&["(A) task one"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks[0].priority, Some('A'));
    }

    #[test]
    fn tags_union() {
        let todo = write_file(&["+project1 task one"]);
        let done = write_file(&[]);
        let conflict = write_file(&["+project1 +project2 task one"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks[0].tags.len(), 2);
    }

    #[test]
    fn attributes_union() {
        let todo = write_file(&["task one due:2024-07-01"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one due:2024-07-01 t:2024-06-25"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert!(tasks[0].attributes.iter().any(|(k, _)| k == "due"));
        assert!(tasks[0].attributes.iter().any(|(k, _)| k == "t"));
    }

    #[test]
    fn conflict_file_removed_on_success() {
        let todo = write_file(&["task one"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one"]);
        let conflict_path = conflict.path().to_owned();
        resolve_conflict(todo.path(), done.path(), Some(&conflict_path), None).unwrap();
        assert!(!conflict_path.exists());
    }

    #[test]
    fn empty_files() {
        let todo = write_file(&[]);
        let done = write_file(&[]);
        let conflict = write_file(&[]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 0);
    }

    #[test]
    fn conflict_only_new_tasks_all_added() {
        let todo = write_file(&[]);
        let done = write_file(&[]);
        let conflict = write_file(&["new task a", "new task b"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 2);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(tasks.len(), 2);
    }

    #[test]
    fn done_task_in_local_done_matched_with_conflict() {
        // Task was archived to done locally, but conflict file still has it pending
        let todo = write_file(&["other task"]);
        let done = write_file(&["x 2024-06-15 2024-06-10 archived task"]);
        let conflict = write_file(&["other task", "archived task"]);
        let _count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert!(done_tasks.iter().any(|t| t.text == "archived task"));
        assert!(
            done_tasks
                .iter()
                .find(|t| t.text == "archived task")
                .unwrap()
                .completed_date()
                .is_some()
        );
    }

    #[test]
    fn attribute_conflict_keeps_later_value() {
        let todo = write_file(&["task one due:2024-06-15"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task one due:2024-07-01"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert_eq!(count, 1);
        let tasks = load_tasks_from_path(todo.path()).unwrap();
        let due = tasks[0]
            .attributes
            .iter()
            .find(|(k, _)| k == "due")
            .unwrap();
        // "2024-07-01" > "2024-06-15" lexicographically, so conflict wins
        assert_eq!(due.1, "2024-07-01");
    }

    #[test]
    fn both_completed_keeps_latest_date() {
        let todo = write_file(&[]);
        let done = write_file(&["x 2024-06-15 task one"]);
        let conflict = write_file(&["x 2024-06-18 task one"]);
        let count =
            resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        assert!(count >= 1);
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        let task = done_tasks.iter().find(|t| t.text == "task one").unwrap();
        assert_eq!(
            task.completed_date(),
            Some(chrono::NaiveDate::from_ymd_opt(2024, 6, 18).unwrap())
        );
    }

    #[test]
    fn recurring_completed_and_next_pending_kept_separate() {
        // Local already completed one occurrence and recreated the next one
        // Conflict still has the older pending occurrence
        let todo = write_file(&["task rec:+1d due:2024-06-16"]);
        let done = write_file(&["x 2024-06-15 task rec:+1d due:2024-06-15"]);
        let conflict = write_file(&["task rec:+1d due:2024-06-15"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
        assert_eq!(done_tasks.len(), 1);
        assert_eq!(
            todo_tasks[0].due_date(),
            Some(chrono::NaiveDate::from_ymd_opt(2024, 6, 16).unwrap())
        );
        assert_eq!(
            done_tasks[0].due_date(),
            Some(chrono::NaiveDate::from_ymd_opt(2024, 6, 15).unwrap())
        );
    }

    #[test]
    fn recurring_same_occurrence_edited_merges() {
        let todo = write_file(&["(C) task rec:+1d due:2024-06-15"]);
        let done = write_file(&[]);
        let conflict = write_file(&["(A) task rec:+1d due:2024-06-15 +tag1"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
        assert_eq!(todo_tasks[0].priority, Some('A'));
        assert_eq!(todo_tasks[0].tags.len(), 1);
    }

    #[test]
    fn recurring_two_completed_different_anchors_kept_separate() {
        let todo = write_file(&[]);
        let done = write_file(&["x 2024-06-15 task rec:+1d due:2024-06-15"]);
        let conflict = write_file(&["x 2024-06-16 task rec:+1d due:2024-06-16"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(done_tasks.len(), 2);
    }

    #[test]
    fn recurring_two_pending_different_anchors_kept_separate() {
        let todo = write_file(&["task rec:+1d due:2024-06-15"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task rec:+1d due:2024-06-16"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 2);
    }

    #[test]
    fn recurring_with_no_anchor_aborts() {
        let todo = write_file(&["task rec:+1d"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task rec:+1d"]);
        let conflict_path = conflict.path().to_owned();
        let res = resolve_conflict(todo.path(), done.path(), Some(&conflict_path), None);
        assert!(res.is_err());
        assert!(conflict_path.exists());
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
    }

    #[test]
    fn recurring_same_id_different_anchors_kept_separate() {
        // Reused series-level id must not merge different occurrences
        let todo = write_file(&["task rec:+1d due:2024-06-15 id:abc"]);
        let done = write_file(&[]);
        let conflict = write_file(&["task rec:+1d due:2024-06-16 id:abc"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 2);
    }

    #[test]
    fn recurring_threshold_only_anchor_merges_same_occurrence() {
        let todo = write_file(&["(C) task rec:+1d t:2024-06-10"]);
        let done = write_file(&[]);
        let conflict = write_file(&["(A) task rec:+1d t:2024-06-10"]);
        resolve_conflict(todo.path(), done.path(), Some(conflict.path()), None).unwrap();
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
        assert_eq!(todo_tasks[0].priority, Some('A'));
    }

    #[test]
    fn done_only_conflict_archives_local_pending() {
        // Only done.txt has a conflict; conflict's done file shows the local pending task
        // was archived as completed on the other device
        let todo = write_file(&["task one", "task two"]);
        let done = write_file(&[]);
        let done_conflict = write_file(&["x 2024-06-15 task one"]);
        let count =
            resolve_conflict(todo.path(), done.path(), None, Some(done_conflict.path())).unwrap();
        assert_eq!(count, 1);
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
        assert_eq!(todo_tasks[0].text, "task two");
        assert_eq!(done_tasks.len(), 1);
        assert_eq!(done_tasks[0].text, "task one");
        assert!(!done_conflict.path().exists());
    }

    #[test]
    fn done_only_conflict_keeps_local_done_task() {
        let todo = write_file(&["pending one"]);
        let done = write_file(&["x 2024-06-15 archived"]);
        let done_conflict = write_file(&["x 2024-06-15 archived"]);
        let count =
            resolve_conflict(todo.path(), done.path(), None, Some(done_conflict.path())).unwrap();
        assert_eq!(count, 0);
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
        assert_eq!(done_tasks.len(), 1);
    }

    #[test]
    fn dual_conflict_resolved_atomically() {
        let todo = write_file(&["task A", "task B"]);
        let done = write_file(&["x 2024-06-10 old archived"]);
        let todo_conflict = write_file(&["task A", "task C"]);
        let done_conflict = write_file(&["x 2024-06-10 old archived", "x 2024-06-15 task B"]);
        let count = resolve_conflict(
            todo.path(),
            done.path(),
            Some(todo_conflict.path()),
            Some(done_conflict.path()),
        )
        .unwrap();
        assert!(count >= 2);
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(todo_tasks.len(), 2);
        assert!(todo_tasks.iter().any(|t| t.text == "task A"));
        assert!(todo_tasks.iter().any(|t| t.text == "task C"));
        assert_eq!(done_tasks.len(), 2);
        assert!(done_tasks.iter().any(|t| t.text == "task B"));
        assert!(done_tasks.iter().any(|t| t.text == "old archived"));
        assert!(!todo_conflict.path().exists());
        assert!(!done_conflict.path().exists());
    }

    #[test]
    fn dual_conflict_order_independent() {
        let todo_a = write_file(&["task one"]);
        let done_a = write_file(&["x 2024-06-15 task two"]);
        let todo_conflict_a = write_file(&["task one", "task two"]);
        let done_conflict_a = write_file(&[]);
        resolve_conflict(
            todo_a.path(),
            done_a.path(),
            Some(todo_conflict_a.path()),
            Some(done_conflict_a.path()),
        )
        .unwrap();
        let final_todo_a = load_tasks_from_path(todo_a.path()).unwrap();
        let final_done_a = load_tasks_from_path(done_a.path()).unwrap();

        let todo_b = write_file(&["task one", "task two"]);
        let done_b = write_file(&[]);
        let todo_conflict_b = write_file(&["task one"]);
        let done_conflict_b = write_file(&["x 2024-06-15 task two"]);
        resolve_conflict(
            todo_b.path(),
            done_b.path(),
            Some(todo_conflict_b.path()),
            Some(done_conflict_b.path()),
        )
        .unwrap();
        let final_todo_b = load_tasks_from_path(todo_b.path()).unwrap();
        let final_done_b = load_tasks_from_path(done_b.path()).unwrap();

        assert_eq!(final_todo_a.len(), final_todo_b.len());
        assert_eq!(final_done_a.len(), final_done_b.len());
        assert!(final_todo_a.iter().any(|t| t.text == "task one"));
        assert!(final_todo_b.iter().any(|t| t.text == "task one"));
        assert!(final_done_a.iter().any(|t| t.text == "task two"));
        assert!(final_done_b.iter().any(|t| t.text == "task two"));
    }

    #[test]
    fn dual_conflict_ambiguity_aborts_without_writes() {
        let todo = write_file(&["task one id:abc"]);
        let done = write_file(&[]);
        let todo_conflict = write_file(&["task one id:abc", "task other id:abc"]);
        let done_conflict = write_file(&[]);
        let res = resolve_conflict(
            todo.path(),
            done.path(),
            Some(todo_conflict.path()),
            Some(done_conflict.path()),
        );
        assert!(res.is_err());
        assert!(todo_conflict.path().exists());
        assert!(done_conflict.path().exists());
        let todo_tasks = load_tasks_from_path(todo.path()).unwrap();
        assert_eq!(todo_tasks.len(), 1);
    }

    #[test]
    fn dual_conflict_completed_metadata_preserved() {
        let todo = write_file(&[]);
        let done = write_file(&["x 2024-06-15 task one"]);
        let todo_conflict = write_file(&[]);
        let done_conflict = write_file(&["x 2024-06-18 task one due:2024-07-01"]);
        resolve_conflict(
            todo.path(),
            done.path(),
            Some(todo_conflict.path()),
            Some(done_conflict.path()),
        )
        .unwrap();
        let done_tasks = load_tasks_from_path(done.path()).unwrap();
        assert_eq!(done_tasks.len(), 1);
        assert_eq!(
            done_tasks[0].completed_date(),
            Some(chrono::NaiveDate::from_ymd_opt(2024, 6, 18).unwrap())
        );
        assert!(done_tasks[0].attributes.iter().any(|(k, _)| k == "due"));
    }
}
