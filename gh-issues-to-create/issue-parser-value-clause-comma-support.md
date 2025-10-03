# [Task]: Add Comma Support to Level-88 VALUE Clauses in Parser

**Issue Description**

The COBOL copybook parser currently does not support comma-separated values within a `VALUE` clause for a level-88 condition name field. The current implementation only handles space-separated literals. While this covers many cases, some COBOL dialects or coding styles may use commas, leading to parsing failures for valid copybooks.

This was marked by a `TODO` in the parser:

```rust
// For now, we'll just parse space-separated values without comma requirement
// TODO: Add comma token support if needed for complex VALUE clauses
```

To properly support this, both the lexer and the parser need to be updated.

**Files and Locations:**

- `copybook-core/src/lexer.rs` (to add the `Comma` token)
- `copybook-core/src/parser.rs` (to handle the new token in the `VALUE` clause logic)

**Proposed Fix**

This fix involves two steps: updating the lexer to recognize commas, and then updating the parser to handle them in level-88 `VALUE` clauses.

### Step 1: Add `Comma` Token to Lexer

In `copybook-core/src/lexer.rs`, the `Token` enum needs a new variant for commas.

```rust
// copybook-core/src/lexer.rs

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
    // ... existing tokens

    // Punctuation
    #[token(".")]
    Period,

    #[token(",")] // Add this line
    Comma,         // Add this line

    #[token("(")]
    LeftParen,

    // ... existing tokens
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ... existing match arms
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::Period => write!(f, "."),
            Token::Comma => write!(f, ","), // Add this line
            Token::LeftParen => write!(f, "("),
            // ... existing match arms
        }
    }
}
```

### Step 2: Update Parser to Handle `Comma`

In `copybook-core/src/parser.rs`, the `parse_level88_value_clause` function should be updated to optionally consume a `Token::Comma` between values.

```rust
// copybook-core/src/parser.rs

    /// Parse Level-88 VALUE clause
    fn parse_level88_value_clause(&mut self, field: &mut Field) -> Result<()> {
        let mut values = Vec::new();

        loop {
            // ... (existing code to parse a value)

            // Optionally consume a comma after a value
            self.consume(&Token::Comma);

            // If the next token is not a literal that can start a new value, break
            if !matches!(self.current_token(), Some(TokenPos { token: Token::StringLiteral(_) | Token::Number(_) | Token::Identifier(_), .. })) {
                break;
            }
        }

        if values.is_empty() {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Level-88 VALUE clause requires at least one value",
            ));
        }

        field.kind = FieldKind::Condition { values };

        Ok(())
    }
```

### Step 3: Add a New Test Case

To ensure the fix is working correctly and to prevent future regressions, a new test should be added to the `tests` module in `copybook-core/src/parser.rs`.

```rust
// copybook-core/src/parser.rs (in `mod tests`)

    #[test]
    fn test_level88_with_comma_separated_values() {
        let input = r#"
    01  STATUS-FIELD    PIC X(1).
        88  IS-VALID    VALUE "A", "B", "C".
"#;
        let schema = parse(input).unwrap();

        assert_eq!(schema.fields.len(), 1);
        let parent_field = &schema.fields[0];
        assert_eq!(parent_field.children.len(), 1);

        let level_88_field = &parent_field.children[0];
        assert_eq!(level_88_field.name, "IS-VALID");
        assert_eq!(level_88_field.level, 88);

        if let FieldKind::Condition { values } = &level_88_field.kind {
            assert_eq!(values.len(), 3);
            assert_eq!(values[0], "A");
            assert_eq!(values[1], "B");
            assert_eq!(values[2], "C");
        } else {
            panic!("Expected FieldKind::Condition, found {:?}", level_88_field.kind);
        }
    }
```