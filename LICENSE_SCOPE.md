# License Scope

As of 2026-02-19, the entire repository and its git history are
published under **AGPL-3.0-or-later** by the copyright holder,
Steven Zimmerman / EffortlessMetrics.

No third-party code is vendored. All dependencies are sourced from
crates.io and are listed with their licenses in THIRD_PARTY_NOTICES.md.

## SPDX Header Exclusions

The following directories intentionally **omit** per-file SPDX license
headers to preserve tool-parser compatibility (YAML front matter, etc.):

- `.claude/`
- `.kiro/`
- `.jules/` (if added in the future)

These directories are covered by the repository-level AGPL-3.0-or-later
license. Do not inject SPDX headers into these paths.
