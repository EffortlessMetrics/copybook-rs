# ODO Array Validation Investigation

## Current Behavior
Current ODO array processing is not enforcing expected validation constraints:

### Test Scenarios
1. **Clipped Record**: Record with ODO count of 5, but only data for 1 element
   - Expected: Error `CBKS301_ODO_CLIPPED`
   - Actual: Successfully decoded with partial data

2. **Too Short Record**: Record with only count byte
   - Expected: Error `CBKD301_RECORD_TOO_SHORT`
   - Actual: Partially decoded

3. **ODO Count Raised**: Record with count exceeding maximum
   - Expected: Error `CBKS302_ODO_RAISED`
   - Actual: Successfully decoded

## Recommendations
1. Review ODO array validation logic in `decode_record` function
2. Implement strict validation checks for:
   - Minimum required record length
   - ODO count within specified range
   - Complete data for declared ODO array
3. Add robust error handling with precise error codes
4. Consider configurable validation strictness (strict vs lenient modes)

## Potential Implementation
```rust
// Pseudocode for stricter ODO validation
fn validate_odo_array(schema: &Schema, record: &[u8]) -> Result<(), Error> {
    let odo_count = parse_odo_count(record);
    let min_record_length = calculate_min_record_length(schema, odo_count);

    if record.len() < min_record_length {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            "Insufficient data for ODO array"
        ));
    }

    if odo_count > schema.max_odo_count {
        return Err(Error::new(
            ErrorCode::CBKS302_ODO_RAISED,
            "ODO count exceeds maximum"
        ));
    }

    // Additional validation...
}
```

## Impact
Critical for maintaining data integrity in COBOL record processing, especially for mainframe migrations and legacy system integrations.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
