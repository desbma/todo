# AGENTS.md

## Build & Test

- Build: `cargo build`
- Check/Lint: `cargo clippy --all-targets --all-features` (pedantic + restriction lints enabled)
- Format: `cargo +nightly fmt --check -- --config imports_granularity=Crate --config group_imports=StdExternalCrate`
- Test: `cargo test --all-features`
- Single test: `cargo test <test_name>`

## Architecture

- Rust workspace (edition 2024, MSRV 1.87) with two crates:
  - **`tasks`** — library crate: `Task` model with todo.txt parsing via `nom` (`task.rs`),
    `TodoFile` for file I/O, file watching via `notify`, undo history, auto-archiving,
    and zstd-compressed backups (`file.rs`)
  - **`todo`** — binary crate (depends on `tasks`): clap-based CLI with subcommands
    (`add`, `auto`, `list`, `menu`, `next`, `notify-overdue`, `pending-count`, `report`, `undo`)
    defined in `cl.rs`, dispatched from `main.rs`
- The `menu` subcommand launches an interactive TUI (ratatui + crossterm) in `menu/`:
  - `state.rs` — state machine (`App`, `Mode::Normal`/`ActionPopup`, `TaskAction`)
  - `update.rs` — pure state transitions from key events, file-change notifications, and ticks
  - `render.rs` — draws search bar, task list with priority/tag/date coloring, action popup, and toast
  - `app.rs` — event loop wiring terminal events, file watcher channels, and task I/O
- Supports multiple todo.txt files: all commands accept optional file paths (defaults to `$TODO_FILE`)
- Uses `anyhow` for error handling throughout; no `unwrap`/`expect` outside tests

## Code Style

- Clippy pedantic + many restriction lints enabled (see `Cargo.toml` `[workspace.lints.clippy]`)
- Imports:
  - Group std imports first, then external crates, then local modules
  - Never use fully-qualified paths (e.g., `std::path::Path` or `crate::ui::foo()`) in code; always import namespaces via `use` statements and refer to symbols by their short name
  - Import deep `std` namespaces aggressively (e.g., `use std::path::PathBuf;`, `use std::collections::HashMap;`), except for namespaces like `io` or `fs` whose symbols have very common names that may collide — import those at the module level instead (e.g., `use std::fs;`)
  - For third-party crates, prefer importing at the crate or module level (e.g., `use anyhow::Context as _;`, `use clap::Parser;`) rather than deeply importing individual symbols, to keep the origin of symbols clear when reading code — only import deeper when needed to avoid very long fully-qualified namespaces
- When formatting paths in error messages or logs, always use debug formatting (`{:?}`) rather than `.display()` to preserve non-UTF-8 safety and show quoting
- Prefer `log` macros for logging; no `dbg!` or `todo!`
- Prefer `default-features = false` for dependencies
- Comments (including doc comments):
  - Keep comments concise: prefer a short summary over restating implementation details, only mention exceptional cases when they affect behavior, and are not already conveyed by the types used, function signature, or code just below
  - Omit trailing periods in single-sentence comments
- In tests:
  - Use `use super::*;` to import from the parent module
  - Prefer `unwrap()` over `expect()` for conciseness
  - Do not add custom messages to `assert!`/`assert_eq!`/`assert_ne!` — the test name is sufficient
  - Prefer full type comparisons with `assert_eq!` over selectively checking nested attributes or unpacking; tag types with `#[cfg_attr(test, derive(Eq, PartialEq))]` if needed
  - Do not add section-separator comments (e.g., `// --- Some Section ---`) in test modules — test names are descriptive enough
- When moving or refactoring code, never remove comment lines — preserve all comments and move them along with the code they document
