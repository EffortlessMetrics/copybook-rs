## 2026-02-14 - CLI Color Testing
**Learning:** `clap`'s `ColorChoice::Auto` is great for UX but breaks `assert_cmd` integration tests that rely on string matching.
**Action:** Always inject `NO_COLOR=1` env var in test helpers when enabling auto-colors in CLI.
