//! COBOL copybook parser
//!
//! This module implements the parsing logic for COBOL copybooks,
//! including lexical analysis and AST construction.

use crate::error::ErrorCode;
use crate::lexer::{CobolFormat, Lexer, Token, TokenPos};
use crate::pic::PicClause;
use crate::schema::{Field, FieldKind, Occurs, Schema};
use crate::{Error, Result};

/// Parse a COBOL copybook text into a schema
/// 
/// # Errors
/// 
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse(text: &str) -> Result<Schema> {
    if text.trim().is_empty() {
        return Err(Error::new(ErrorCode::CBKP001_SYNTAX, "Empty copybook text"));
    }

    let mut lexer = Lexer::new(text);
    let tokens = lexer.tokenize();
    
    let mut parser = Parser::new(tokens, lexer.format());
    parser.parse_schema()
}

/// Parser state for COBOL copybook parsing
struct Parser {
    tokens: Vec<TokenPos>,
    current: usize,
    format: CobolFormat,
}

impl Parser {
    fn new(tokens: Vec<TokenPos>, format: CobolFormat) -> Self {
        Self {
            tokens,
            current: 0,
            format,
        }
    }

    /// Parse the complete schema
    fn parse_schema(&mut self) -> Result<Schema> {
        let mut fields = Vec::new();
        
        // Skip any leading comments or empty lines
        self.skip_comments_and_newlines();
        
        while !self.is_at_end() {
            if let Some(field) = self.parse_field()? {
                fields.push(field);
            }
            self.skip_comments_and_newlines();
        }

        if fields.is_empty() {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "No valid field definitions found",
            ));
        }

        Ok(Schema::from_fields(fields))
    }

    /// Parse a single field definition
    fn parse_field(&mut self) -> Result<Option<Field>> {
        // Look for level number
        let level = match self.current_token() {
            Some(TokenPos { token: Token::Level(n), .. }) => {
                let level = *n;
                self.advance();
                level
            }
            Some(TokenPos { token: Token::Level66, .. }) => {
                // Skip 66-level (rename) entries
                self.skip_to_period();
                return Ok(None);
            }
            Some(TokenPos { token: Token::Level77, .. }) => {
                let level = 77;
                self.advance();
                level
            }
            Some(TokenPos { token: Token::Level88, .. }) => {
                // Skip 88-level (condition) entries
                self.skip_to_period();
                return Ok(None);
            }
            _ => return Ok(None),
        };

        // Get field name
        let name = match self.current_token() {
            Some(TokenPos { token: Token::Identifier(name), .. }) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    format!("Expected field name after level {}", level),
                ));
            }
        };

        // Parse field clauses
        let mut field = Field::new(level, name);
        
        while !self.check(&Token::Period) && !self.is_at_end() {
            self.parse_field_clause(&mut field)?;
        }

        // Expect period to end field definition
        if !self.consume(&Token::Period) {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Expected period after field definition for {}", field.name),
            ));
        }

        Ok(Some(field))
    }

    /// Parse a field clause (PIC, USAGE, REDEFINES, etc.)
    fn parse_field_clause(&mut self, field: &mut Field) -> Result<()> {
        match self.current_token() {
            Some(TokenPos { token: Token::Pic, .. }) => {
                self.advance();
                self.parse_pic_clause(field)?;
            }
            Some(TokenPos { token: Token::Usage, .. }) => {
                self.advance();
                self.parse_usage_clause(field)?;
            }
            Some(TokenPos { token: Token::Redefines, .. }) => {
                self.advance();
                self.parse_redefines_clause(field)?;
            }
            Some(TokenPos { token: Token::Occurs, .. }) => {
                self.advance();
                self.parse_occurs_clause(field)?;
            }
            Some(TokenPos { token: Token::Synchronized, .. }) => {
                self.advance();
                field.synchronized = true;
            }
            Some(TokenPos { token: Token::Value, .. }) => {
                // Skip VALUE clauses (metadata only)
                self.skip_value_clause()?;
            }
            Some(TokenPos { token: Token::Blank, .. }) => {
                self.advance();
                self.parse_blank_when_zero_clause(field)?;
            }
            Some(TokenPos { token: Token::Sign, .. }) => {
                // SIGN clauses are treated as edited PIC
                return Err(Error::new(
                    ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                    "SIGN clauses are not supported (treated as edited PIC)",
                ));
            }
            _ => {
                // Unknown clause - advance and continue
                self.advance();
            }
        }
        Ok(())
    }

    /// Parse PIC clause
    fn parse_pic_clause(&mut self, field: &mut Field) -> Result<()> {
        let pic_str = match self.current_token() {
            Some(TokenPos { token: Token::PicClause(pic), .. }) => {
                let pic = pic.clone();
                self.advance();
                pic
            }
            Some(TokenPos { token: Token::EditedPic(pic), .. }) => {
                return Err(Error::new(
                    ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                    format!("Edited PIC not supported: {}", pic),
                ));
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected PIC clause after PIC keyword",
                ));
            }
        };

        let pic = PicClause::parse(&pic_str)?;
        
        field.kind = match pic.kind {
            crate::pic::PicKind::Alphanumeric => FieldKind::Alphanum { len: pic.digits as u32 },
            crate::pic::PicKind::NumericDisplay => FieldKind::ZonedDecimal {
                digits: pic.digits,
                scale: pic.scale,
                signed: pic.signed,
            },
            crate::pic::PicKind::Edited => {
                return Err(Error::new(
                    ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                    "Edited PIC should have been caught earlier",
                ));
            }
        };

        Ok(())
    }

    /// Parse USAGE clause
    fn parse_usage_clause(&mut self, field: &mut Field) -> Result<()> {
        match self.current_token() {
            Some(TokenPos { token: Token::Display, .. }) => {
                self.advance();
                // USAGE DISPLAY is the default, no change needed
            }
            Some(TokenPos { token: Token::Comp, .. }) => {
                self.advance();
                self.convert_to_binary_field(field)?;
            }
            Some(TokenPos { token: Token::Comp3, .. }) => {
                self.advance();
                self.convert_to_packed_field(field)?;
            }
            Some(TokenPos { token: Token::Binary, .. }) => {
                self.advance();
                self.convert_to_binary_field(field)?;
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected USAGE type after USAGE keyword",
                ));
            }
        }
        Ok(())
    }

    /// Parse REDEFINES clause
    fn parse_redefines_clause(&mut self, field: &mut Field) -> Result<()> {
        let target = match self.current_token() {
            Some(TokenPos { token: Token::Identifier(name), .. }) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected field name after REDEFINES",
                ));
            }
        };

        field.redefines_of = Some(target);
        Ok(())
    }

    /// Parse OCCURS clause
    fn parse_occurs_clause(&mut self, field: &mut Field) -> Result<()> {
        let count = match self.current_token() {
            Some(TokenPos { token: Token::Number(n), .. }) => {
                let count = *n;
                self.advance();
                count
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected number after OCCURS",
                ));
            }
        };

        // Check for DEPENDING ON
        if self.check(&Token::Depending) {
            self.advance(); // consume DEPENDING
            if !self.consume(&Token::On) {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected ON after DEPENDING",
                ));
            }

            let counter_field = match self.current_token() {
                Some(TokenPos { token: Token::Identifier(name), .. }) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        "Expected field name after DEPENDING ON",
                    ));
                }
            };

            field.occurs = Some(Occurs::ODO {
                min: 0, // Will be validated later
                max: count,
                counter_path: counter_field,
            });
        } else {
            field.occurs = Some(Occurs::Fixed { count });
        }

        // Skip optional TIMES keyword
        if self.check(&Token::Times) {
            self.advance();
        }

        Ok(())
    }

    /// Parse BLANK WHEN ZERO clause
    fn parse_blank_when_zero_clause(&mut self, field: &mut Field) -> Result<()> {
        if !self.consume(&Token::When) {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Expected WHEN after BLANK",
            ));
        }

        if !self.consume(&Token::Zero) {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Expected ZERO after BLANK WHEN",
            ));
        }

        field.blank_when_zero = true;
        Ok(())
    }

    /// Skip VALUE clause (not needed for layout)
    fn skip_value_clause(&mut self) -> Result<()> {
        self.advance(); // consume VALUE
        
        // Skip until we find a keyword or period
        while !self.is_at_end() && !self.check(&Token::Period) {
            if self.is_keyword() {
                break;
            }
            self.advance();
        }
        
        Ok(())
    }

    /// Convert numeric field to binary
    fn convert_to_binary_field(&mut self, field: &mut Field) -> Result<()> {
        match &field.kind {
            FieldKind::ZonedDecimal { digits, signed, .. } => {
                let bits = match digits {
                    1..=4 => 16,
                    5..=9 => 32,
                    10..=18 => 64,
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Binary field with {} digits not supported", digits),
                        ));
                    }
                };
                
                field.kind = FieldKind::BinaryInt { bits, signed: *signed };
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "USAGE COMP/BINARY can only be applied to numeric fields",
                ));
            }
        }
        Ok(())
    }

    /// Convert numeric field to packed decimal
    fn convert_to_packed_field(&mut self, field: &mut Field) -> Result<()> {
        match &field.kind {
            FieldKind::ZonedDecimal { digits, scale, signed } => {
                field.kind = FieldKind::PackedDecimal {
                    digits: *digits,
                    scale: *scale,
                    signed: *signed,
                };
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "USAGE COMP-3 can only be applied to numeric fields",
                ));
            }
        }
        Ok(())
    }

    /// Skip to the next period
    fn skip_to_period(&mut self) {
        while !self.is_at_end() && !self.check(&Token::Period) {
            self.advance();
        }
        if self.check(&Token::Period) {
            self.advance();
        }
    }

    /// Skip comments and newlines
    fn skip_comments_and_newlines(&mut self) {
        while let Some(token) = self.current_token() {
            match &token.token {
                Token::InlineComment(_) | Token::Newline => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Check if current token is a keyword
    fn is_keyword(&self) -> bool {
        matches!(
            self.current_token().map(|t| &t.token),
            Some(Token::Pic) | Some(Token::Usage) | Some(Token::Redefines) |
            Some(Token::Occurs) | Some(Token::Synchronized) | Some(Token::Value) |
            Some(Token::Blank) | Some(Token::Sign)
        )
    }

    /// Get current token
    fn current_token(&self) -> Option<&TokenPos> {
        self.tokens.get(self.current)
    }

    /// Advance to next token
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    /// Check if we're at the end
    fn is_at_end(&self) -> bool {
        matches!(
            self.current_token().map(|t| &t.token),
            Some(Token::Eof) | None
        )
    }

    /// Check if current token matches the given token
    fn check(&self, token: &Token) -> bool {
        if let Some(current) = self.current_token() {
            std::mem::discriminant(&current.token) == std::mem::discriminant(token)
        } else {
            false
        }
    }

    /// Consume token if it matches, return true if consumed
    fn consume(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_field_parsing() {
        let input = "01 CUSTOMER-ID PIC X(10).";
        let schema = parse(input).unwrap();
        
        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert_eq!(field.name, "CUSTOMER-ID");
        assert_eq!(field.level, 1);
        assert!(matches!(field.kind, FieldKind::Alphanum { len: 10 }));
    }

    #[test]
    fn test_numeric_field_parsing() {
        let input = "01 AMOUNT PIC S9(7)V99.";
        
        // Debug: test tokenization
        let mut lexer = crate::lexer::Lexer::new(input);
        let tokens = lexer.tokenize();
        for (i, token) in tokens.iter().enumerate() {
            println!("Token {}: {:?}", i, token.token);
        }
        
        // Debug: test PIC parsing directly
        let pic_result = crate::pic::PicClause::parse("S9(7)V99");
        println!("PIC parse result: {:?}", pic_result);
        
        let schema = parse(input).unwrap();
        
        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert_eq!(field.name, "AMOUNT");
        
        // Debug print the actual field kind
        println!("Field kind: {:?}", field.kind);
        
        assert!(matches!(
            field.kind,
            FieldKind::ZonedDecimal { digits: 9, scale: 2, signed: true }
        ));
    }

    #[test]
    fn test_binary_field_parsing() {
        let input = "01 COUNT PIC 9(5) USAGE COMP.";
        let schema = parse(input).unwrap();
        
        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert!(matches!(
            field.kind,
            FieldKind::BinaryInt { bits: 32, signed: false }
        ));
    }

    #[test]
    fn test_occurs_parsing() {
        let input = "01 ARRAY-FIELD PIC X(10) OCCURS 5 TIMES.";
        let schema = parse(input).unwrap();
        
        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert!(matches!(field.occurs, Some(Occurs::Fixed { count: 5 })));
    }

    #[test]
    fn test_redefines_parsing() {
        let input = r#"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES FIELD-A PIC 9(10).
"#;
        let schema = parse(input).unwrap();
        
        assert_eq!(schema.fields.len(), 2);
        let field_b = &schema.fields[1];
        assert_eq!(field_b.redefines_of, Some("FIELD-A".to_string()));
    }

    #[test]
    fn test_edited_pic_rejection() {
        let input = "01 AMOUNT PIC ZZ,ZZZ.99.";
        let result = parse(input);
        
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err().code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC));
    }

    #[test]
    fn test_sign_clause_rejection() {
        let input = "01 AMOUNT PIC S9(5) SIGN LEADING.";
        let result = parse(input);
        
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err().code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC));
    }
}