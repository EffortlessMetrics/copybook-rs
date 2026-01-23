# Palette's Journal 🎨

This journal records critical UX and accessibility learnings for the `copybook-rs` project.

## 2025-02-18 - CLI Color Policy
**Learning:** The CLI was explicitly configured with `ColorChoice::Never` and `tracing` forced ansi to `false`, making the tool feel "dead" and unmodern.
**Action:** Default to `ColorChoice::Auto` and respect `NO_COLOR` standard for all future CLI tools in this workspace.
