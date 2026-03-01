# copybook-error-reporter

Structured error reporting and handling for copybook-rs processing flows.

## Overview

Provides configurable error accumulation with severity classification, strict/lenient handling
policies, and summary statistics. Used by the codec and CLI layers to track and report errors
across large batch processing runs without losing context on individual failures.

## Usage

```rust
use copybook_error_reporter::{ErrorReporter, ErrorMode};

// Create a lenient reporter that continues on errors
let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

// Report errors during processing; reporter accumulates them
// reporter.report_error(some_error);

let summary = reporter.summary();
println!("Processed with {} errors", summary.total_errors);
```

## Public API

- `ErrorReporter` — Accumulates errors with configurable stop/continue policy
- `ErrorMode` — `Strict` (fail-fast) or `Lenient` (continue processing)
- `ErrorSeverity` — `Info`, `Warning`, `Error`, `Fatal`
- `ErrorReport` — Single error with severity, timestamp, and metadata
- `ErrorSummary` — Aggregate statistics across a processing run

## License

AGPL-3.0-or-later
