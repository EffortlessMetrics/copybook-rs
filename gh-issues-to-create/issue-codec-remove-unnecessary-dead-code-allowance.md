# [Task]: Remove Unnecessary `dead_code` Allowance from `likely` Function

**Issue Description**

The function `likely` in `copybook-codec/src/numeric.rs` is annotated with `#[allow(dead_code)]`, but it is actively used in the hot path of the `decode_packed_decimal` function. This makes the `allow` attribute unnecessary and harmful, as it suppresses legitimate compiler warnings.

Keeping this unnecessary allowance poses a maintenance risk: if the `decode_packed_decimal` function is refactored in the future to no longer use `likely`, the compiler will be silenced and will not report that `likely` has become dead code. This leads to code rot.

**File and Location:**

`copybook-codec/src/numeric.rs:14`

**Code Context:**

*The function definition with the unnecessary allowance:*
```rust
// copybook-codec/src/numeric.rs

// CRITICAL PERFORMANCE OPTIMIZATION: Inline hints for hot paths
#[allow(dead_code)] // This is unnecessary
#[allow(clippy::inline_always)]
#[inline(always)]
pub(crate) fn likely(b: bool) -> bool {
    // ...
}
```

*The usage of the function in `decode_packed_decimal`:*
```rust
// copybook-codec/src/numeric.rs

#[inline(always)]
pub fn decode_packed_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    // ...
    // PERFORMANCE CRITICAL: Single branch validation optimized for happy path
    if likely(data.len() == expected_bytes && !data.is_empty() && digits <= 18) {
        // ULTRA-FAST PATH: Most common enterprise cases with minimal validation
        return decode_packed_decimal_fast_path(data, digits, scale, signed);
    }
    // ...
}
```

**Proposed Fix**

The fix is simple and clean: remove the `#[allow(dead_code)]` attribute from the `likely` function. The code is correct and the function is in use, so the compiler will not generate any warnings.

This change improves code hygiene and ensures that the compiler can perform its job correctly in the future.

```rust
// copybook-codec/src/numeric.rs

// CRITICAL PERFORMANCE OPTIMIZATION: Inline hints for hot paths
// No longer has #[allow(dead_code)]
#[allow(clippy::inline_always)]
#[inline(always)]
pub(crate) fn likely(b: bool) -> bool {
    // CRITICAL PERFORMANCE OPTIMIZATION: Manual branch prediction optimization
    // The true case is expected to be taken most of the time (likely path)
    // Mark the false case as cold to optimize for the common true case
    if b {
        true
    } else {
        cold_branch_hint();
        false
    }
}
```