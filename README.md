# Todo

[![Build status](https://github.com/desbma/todo/actions/workflows/ci.yml/badge.svg)](https://github.com/desbma/todo/actions)
[![License](https://img.shields.io/github/license/desbma/todo.svg?style=flat)](https://github.com/desbma/todo/blob/master/LICENSE)

[Todo.txt](http://todotxt.org/) terminal frontend

## Features (work in progress)

- [x] Colored output, respecting native terminal colors
- [x] Better default sorting (deprioritizes tasks before threshold date, etc.)
- [x] FZF like menu with auto reload
- [ ] Recurrence, propagated to both `t` and `due` attributes (at 'do' time, and after it if task was marked as done from another device)
- [ ] Auto archival of done tasks, with subfiles
- [ ] Dependencies
- [ ] Completely independant of `todo.sh`

## Rationale

I have been a heavy [Taskwarrior](https://taskwarrior.org/) user for several years, but have been dissatisfied mostly with the inability to reliably share task handling between devices.

### Thoughts on Taskwarrior

Strengths:

- the data model does no force a way to manage tasks
- ability to customize the formula that computes the priority values, and consequently the sorting of tasks

Weaknesses:

- recurrence is not usable with more than one device without risking data loss or duplicate task creation
- recurrence model is limited: next task is always created regardless if the previous one is still pending
- no usable Android app
- the command line tool `task`, has many bugs (breaking unicode strings, inconsistent option handling or formatting...)
- the sync daemon is of proof of concept quality, and is terribly lacking both in its design and implementation

### Todo.txt

Strengths:

- has a very simple and flexible file format, which allows editing tasks by hand
- consequently, syncing the file across devices is trivial (ie. with Syncthing) and merging modifications from devices also is
- has several usable Android apps

Weaknesses:

- no support for recurrence, waiting tasks, or dependencies out of the box, although this is for the most part commonly done with addons and custom attributes
- none of the Android apps fully handle both `rec` and `t`
- no smart task sorting the way taskwarrior does

### Goals

The goals of this project is to improve the weaknesses of Todo.txt for my use cases, while keeping the flexibility, easy of sync, and existing ecosystem compatibility of the original text format.

## Installation

You need a Rust build environment for example from [rustup](https://rustup.rs/).

```
cargo build --release
install -Dm 755 -t /usr/local/bin target/release/todo
```

## License

[GPLv3](https://www.gnu.org/licenses/gpl-3.0-standalone.html)
