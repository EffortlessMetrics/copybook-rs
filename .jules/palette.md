## 2026-01-24 - [CLI Color Accessibility]
**Learning:** CLI tools often disable colors by default or hardcode them, missing a huge UX win. `clap`'s `ColorChoice::Auto` + `NO_COLOR` support is the gold standard for accessible, beautiful terminal interfaces.
**Action:** Always check `clap` configuration and `tracing` setup in CLI tools to ensure they respect TTY and `NO_COLOR` for optimal default experience.
