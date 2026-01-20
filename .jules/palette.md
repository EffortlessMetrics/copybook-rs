## 2026-01-20 - CLI Color Defaults and Testing
**Learning:** Enabling `ColorChoice::Auto` in `clap` and removing explicit `ansi(false)` in `tracing` improves UX but breaks integration tests that assert on stderr output (due to ANSI codes).
**Action:** When enabling colors in CLI, always ensure test helpers explicitly set `NO_COLOR=1` environment variable to keep tests deterministic and text-based assertions valid.
