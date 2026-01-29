## 2026-01-29 - Enable ANSI Colors
**Learning:** CLI tools often default to disabling colors to be safe, but this hurts UX significantly. `clap` handles color auto-detection robustly. For `tracing-subscriber`, explicitly checking `IsTerminal` AND `NO_COLOR` is critical to respect user preferences and avoid breaking scripts.
**Action:** Always check `IsTerminal` and `NO_COLOR` when configuring logging libraries that don't do it automatically.
