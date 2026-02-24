# copybook-error-reporter

This crate provides structured error reporting for copybook-rs processing flows.

- Error severity classification (`Info`, `Warning`, `Error`, `Fatal`)
- Configurable strict/lenient handling policy
- Error statistics and summary reporting
- Corruption warning tracking

## Public API

- `ErrorMode`
- `ErrorSeverity`
- `ErrorReport`
- `ErrorSummary`
- `ErrorReporter`
