---
title: Add Comma Token Support for Complex VALUE Clauses in copybook-core Parser
labels: ["enhancement", "todo"]
assignees: []
---

## Issue Description

In `copybook-core/src/parser.rs` at line 982, there is a `TODO` comment: `// TODO: Add comma token support if needed for complex VALUE clauses`. This indicates that the current copybook parser might not fully handle `VALUE` clauses that contain comma tokens. If such clauses are present in COBOL copybooks, the parser could fail to correctly interpret them, leading to incomplete or incorrect copybook definitions.

## Location

*   `copybook-core/src/parser.rs:982`

## Proposed Fix

1.  **Research:** Investigate the COBOL language specification to confirm whether comma tokens are valid within `VALUE` clauses and, if so, their precise syntax and semantics.
2.  **Implement Lexer/Parser Changes:** Based on the research, modify the lexer to correctly identify comma tokens within `VALUE` clauses (if they are part of the literal value) or as separators (if they serve a structural purpose). Update the parser logic in `copybook-core/src/parser.rs` to correctly process these tokens and integrate them into the abstract syntax tree (AST) representation of the copybook.
3.  **Add Tests:** Create new unit and integration tests to cover various scenarios of `VALUE` clauses with and without comma tokens to ensure the parser behaves as expected.

## Example (Illustrative - conceptual change in parser logic)

```rust
// Before (simplified context around the TODO):
// fn parse_value_clause(...) -> Result<Value, ParseError> {
//     // ... existing logic ...
//     match token {
//         Token::Literal(s) => {
//             // Current handling of literal values
//             // TODO: Add comma token support if needed for complex VALUE clauses
//             Ok(Value::Literal(s.to_string()))
//         },
//         // ... other token types ...
//     }
// }

// After (conceptual, assuming a VALUE "1,234" needs to be parsed as a single string or number):
// fn parse_value_clause(...) -> Result<Value, ParseError> {
//     // ... existing logic ...
//     match token {
//         Token::Literal(s) => {
//             // Example: If commas are part of the literal and should be preserved or removed
//             let processed_s = s.replace(",", ""); // Or handle based on COBOL spec
//             Ok(Value::Literal(processed_s))
//         },
//         // ... other token types ...
//     }
// }

// Or, if commas are structural separators within a complex VALUE clause:
// fn parse_complex_value_clause(...) -> Result<ComplexValue, ParseError> {
//     let mut parts = Vec::new();
//     parts.push(parse_single_value()?);
//     while self.peek_token() == Some(Token::Comma) {
//         self.consume_token(Token::Comma)?;
//         parts.push(parse_single_value()?);
//     }
//     Ok(ComplexValue { parts })
// }
```

This enhancement would improve the robustness and compatibility of the `copybook-core` parser with a wider range of COBOL copybook definitions.
