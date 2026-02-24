# copybook-utils

Panic-safe utility functions and extension traits for copybook-rs.

This crate provides utilities to eliminate panic conditions in the copybook-rs
codebase, replacing unwrap() calls with structured error handling.

## Public API

- `OptionExt<T>` - Extension trait for Option with panic-safe unwrapping
- `VecExt<T>` - Extension trait for Vec with panic-safe access operations
- `SliceExt<T>` - Extension trait for slices with panic-safe indexing
- `safe_ops` - Module containing various safe operation functions
- `Result<T>` - Result type alias using copybook-error's Error

## Features

- **Panic Prevention**: All utilities return structured errors instead of panicking
- **Performance Optimized**: Hot paths are aggressively inlined for minimal overhead
- **Error Context**: Detailed error messages with context for debugging
