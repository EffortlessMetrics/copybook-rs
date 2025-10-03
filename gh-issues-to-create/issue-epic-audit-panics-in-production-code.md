
# [Epic]: Audit and Refactor Panics in Production Code

**Issue Description**

A search of the codebase revealed over 200 instances of `.unwrap()`, `.expect()`, or `panic!()`. While many of these are acceptable within test code, a significant number are present in production library code (`copybook-core` and `copybook-codec`).

These panics represent a major robustness issue, as they can crash the entire application when encountering unexpected data or states. For a library, this is a critical flaw. The project already has a focus on "panic elimination" (as seen in file names and safe utility functions), and this effort should be continued to completion.

**High-Priority Examples:**

1.  **Potential Panic in Record Iterator (`copybook-codec/src/iterator.rs:139`)**:
    The `read_record_fixed` method unconditionally unwraps `self.schema.lrecl_fixed`. If a schema without a fixed record length is used with this iterator, it will panic. This is a critical bug in a core library component.

    ```rust
    // copybook-codec/src/iterator.rs
    fn read_record_fixed(&mut self) -> Result<Option<Vec<u8>>> {
        let lrecl = self.schema.lrecl_fixed.unwrap() as usize; // PANIC RISK
        // ...
    }
    ```

2.  **Panic on Buffer Write (`copybook-codec/src/json.rs:1029`)**:
    The `write_numeric` method uses `.unwrap()` when writing to a string buffer. While a write to a `String` is not expected to fail, using `unwrap` is not robust. The `fmt::Write` trait returns a `Result` for a reason, and it should be handled properly.

    ```rust
    // copybook-codec/src/json.rs
    fn write_numeric<T: fmt::Display>(
        &mut self,
        // ...
    ) -> Result<()> {
        // ...
        write!(self.json_buffer, "{}", num).unwrap(); // PANIC RISK
        Ok(())
    }
    ```

**Proposed Action**

A systematic audit and refactoring effort should be undertaken to eliminate all panics from production code.

**Workflow:**

1.  **Identify Panics**: Locate all instances of `.unwrap()`, `.expect()`, and `panic!()` in the `src` directories of all `copybook-*` crates, excluding `copybook-bench` and `#[cfg(test)]` modules.

2.  **Prioritize Library Code**: Begin with the core libraries, `copybook-core` and `copybook-codec`, as panics here have the widest impact.

3.  **Refactor**: Replace each panic with robust error handling.
    -   Use the `?` operator to propagate errors up the call stack.
    -   Use `if let` or `match` to handle `Option` and `Result` types gracefully.
    -   Return a `Result` with a specific, descriptive `Error` variant instead of panicking.

**Example Refactoring (for `read_record_fixed`):**

```rust
// copybook-codec/src/iterator.rs

    fn read_record_fixed(&mut self) -> Result<Option<Vec<u8>>> {
        // Use `ok_or` or a match to handle the Option gracefully
        let lrecl = self.schema.lrecl_fixed.ok_or_else(|| Error::new(
            ErrorCode::CBKI001_INVALID_STATE, // Example error code
            "read_record_fixed called on a schema with no fixed record length"
        ))? as usize;

        // ... (rest of the function)
    }
```

Completing this epic will significantly improve the robustness and production-readiness of the entire `copybook-rs` suite.
