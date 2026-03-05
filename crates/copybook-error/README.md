# copybook-error

Error types and taxonomy for copybook-rs.

This crate provides a comprehensive error taxonomy with stable error codes
for all failure modes in the copybook processing system.

## Public API

- `Error` - Main error type with code, message, and optional context
- `ErrorCode` - Stable error codes for programmatic error handling
- `ErrorContext` - Detailed error context (record index, field path, byte offset, line number)
- `Result<T>` - Result type alias
- `error!` macro - Convenience macro for creating errors

## Error Code Taxonomy

The error codes follow the pattern `CBK[Category][Number]_[Description]`:
- `CBKP*` - Parse errors during copybook analysis
- `CBKS*` - Schema validation and ODO processing
- `CBKR*` - Record format and RDW processing
- `CBKC*` - Character conversion and encoding
- `CBKD*` - Data decoding and field validation
- `CBKE*` - Encoding and JSON serialization
- `CBKF*` - File format and structure validation
- `CBKI*` - Iterator and infrastructure state validation
- `CBKA*` - Performance and compliance audit operations
- `CBKW*` - Arrow and Parquet conversion errors
