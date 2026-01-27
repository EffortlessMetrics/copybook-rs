## 2026-01-27 - Safe Color Implementation in CLI
**Learning:** Enabling `ColorChoice::Auto` in `clap` is a quick UX win, but it MUST be paired with strict environment controls in integration tests. Specifically, `assert_cmd` tests running in non-TTY environments can be unpredictable with colors unless `NO_COLOR=1` is explicitly set in the test harness.
**Action:** Always wrap `cargo_bin_cmd!` in a helper that sets `.env("NO_COLOR", "1")` when working on CLI tools that support colored output.
