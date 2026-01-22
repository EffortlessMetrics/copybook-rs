## 2026-02-14 - CLI Color Auto-Detection
**Learning:** Hardcoding `ColorChoice::Never` in `clap` CLI tools suppresses all visual hierarchy, making help text and errors harder to parse. Enabling `ColorChoice::Auto` is a low-effort, high-impact win for usability, but must be paired with `NO_COLOR` respect for accessibility/automation compliance.
**Action:** Always use `ColorChoice::Auto` for CLIs and ensure logging libraries (like `tracing`) also respect TTY status and `NO_COLOR` env vars.
