# copybook-cli-io

Single-responsibility CLI I/O helpers for `copybook-rs`.

This microcrate centralizes:

- atomic file writes via temp-file + rename
- text input loading from a path or standard input (`-`)
