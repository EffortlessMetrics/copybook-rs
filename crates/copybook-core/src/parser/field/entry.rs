use crate::error::{ErrorCode, ErrorContext};
use crate::lexer::{Token, TokenPos};
use crate::schema::{Field, FieldKind};
use crate::{Error, Result};

use super::super::{Parser, try_extract_data_name};

impl Parser {
    /// Parse a single field definition.
    pub(in crate::parser) fn parse_field(&mut self) -> Result<Option<Field>> {
        let Some(level) = self.parse_field_level()? else {
            return Ok(None);
        };

        let name = self.parse_field_name(level)?;
        let mut field = Field::new(level, name);

        self.parse_field_clauses_until_period(&mut field)?;
        self.consume_field_period(&field)?;
        validate_level_66_has_renames(&field)?;

        Ok(Some(field))
    }

    fn parse_field_level(&mut self) -> Result<Option<u8>> {
        let level = match self.current_token() {
            Some(TokenPos {
                token: Token::Level(n),
                ..
            }) => {
                let level = *n;
                self.advance();
                level
            }
            Some(TokenPos {
                token: Token::Level66,
                ..
            }) => {
                self.advance();
                66
            }
            Some(TokenPos {
                token: Token::Level77,
                ..
            }) => {
                self.advance();
                77
            }
            Some(TokenPos {
                token: Token::Level88,
                ..
            }) => {
                self.advance();
                88
            }
            Some(TokenPos {
                token: Token::Number(n),
                line,
                ..
            }) => return self.parse_numeric_level_token(*n, *line),
            _ => {
                // If we encounter an unrecognized token, advance to avoid infinite loop.
                self.advance();
                return Ok(None);
            }
        };

        Ok(Some(level))
    }

    fn parse_numeric_level_token(&mut self, number: u32, line: usize) -> Result<Option<u8>> {
        if number == 0 || (50..=99).contains(&number) {
            return Err(invalid_level_error(number, line));
        }

        if (1..=49).contains(&number) {
            self.advance();
            return Ok(Some(number as u8));
        }

        // Large numbers are likely sequence numbers or other contexts.
        self.advance();
        Ok(None)
    }

    fn parse_field_name(&mut self, level: u8) -> Result<String> {
        match self.current_token().and_then(try_extract_data_name) {
            Some(name) => {
                self.advance();
                Ok(normalize_filler_name(name, self.options.emit_filler))
            }
            None => Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Expected field name after level {level}"),
            )),
        }
    }

    fn parse_field_clauses_until_period(&mut self, field: &mut Field) -> Result<()> {
        while !self.check(&Token::Period) && !self.is_at_end() {
            self.parse_field_clause(field)?;
        }

        Ok(())
    }

    fn consume_field_period(&mut self, field: &Field) -> Result<()> {
        if self.consume(&Token::Period) {
            return Ok(());
        }

        Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Expected period after field definition for {}", field.name),
        ))
    }
}

fn normalize_filler_name(name: String, emit_filler: bool) -> String {
    if name.eq_ignore_ascii_case("FILLER") && !emit_filler {
        return "FILLER".to_string();
    }

    name
}

fn invalid_level_error(number: u32, line: usize) -> Error {
    let safe_line_number =
        copybook_overflow::safe_usize_to_u32(line, "error context line number").ok();

    Error::new(
        ErrorCode::CBKP001_SYNTAX,
        format!("Invalid level number '{number}'"),
    )
    .with_context(ErrorContext {
        record_index: None,
        field_path: None,
        byte_offset: None,
        line_number: safe_line_number,
        details: None,
    })
}

fn validate_level_66_has_renames(field: &Field) -> Result<()> {
    if field.level == 66 && !matches!(field.kind, FieldKind::Renames { .. }) {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Level-66 field '{}' must have RENAMES clause", field.name),
        ));
    }

    Ok(())
}
