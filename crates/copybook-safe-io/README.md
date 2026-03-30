# copybook-safe-io

Single-responsibility microcrate for panic-safe stdio writes and panic payload classification.

This crate centralizes:
- consumer-closed detection for stdout/stderr writes (`BrokenPipe`, `WriteZero`, Windows pipe closures)
- panic payload extraction into displayable text
- panic classification for stdio pipeline shutdowns
