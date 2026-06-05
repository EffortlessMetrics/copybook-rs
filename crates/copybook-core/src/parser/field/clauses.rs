use crate::error::ErrorCode;
use crate::feature_flags::{Feature, FeatureFlags};
use crate::lexer::Token;
use crate::schema::{Field, FieldKind};
use crate::{Error, Result};

use super::super::Parser;

impl Parser {
    /// Parse a field clause (PIC, USAGE, REDEFINES, etc.).
    pub(in crate::parser) fn parse_field_clause(&mut self, field: &mut Field) -> Result<()> {
        let Some(token) = self
            .current_token()
            .map(|token_pos| token_pos.token.clone())
        else {
            return Ok(());
        };

        match token {
            Token::Pic => self.parse_pic_field_clause(field),
            Token::Usage => self.parse_usage_field_clause(field),
            Token::Redefines => self.parse_redefines_field_clause(field),
            Token::Renames => self.parse_renames_field_clause(field),
            Token::Occurs => self.parse_occurs_field_clause(field),
            Token::Synchronized => self.parse_synchronized_field_clause(field),
            Token::Value => self.parse_value_field_clause(field),
            Token::Blank => self.parse_blank_field_clause(field),
            Token::Sign => self.parse_sign_field_clause(field),
            Token::Comp => self.parse_comp_field_clause(field),
            Token::Comp3 => self.parse_comp3_field_clause(field),
            Token::Comp1 => self.parse_comp1_field_clause(field),
            Token::Comp2 => self.parse_comp2_field_clause(field),
            Token::Binary => self.parse_binary_field_clause(field),
            _ => {
                // Unknown clause - advance and continue.
                self.advance();
                Ok(())
            }
        }
    }

    fn parse_pic_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.parse_pic_clause(field)
    }

    fn parse_usage_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.parse_usage_clause(field)
    }

    fn parse_redefines_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.parse_redefines_clause(field)
    }

    fn parse_renames_field_clause(&mut self, field: &mut Field) -> Result<()> {
        if field.level == 66 {
            return self.parse_renames(field);
        }

        Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!(
                "RENAMES clause can only be used with level-66, not level {}",
                field.level
            ),
        ))
    }

    fn parse_occurs_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.parse_occurs_clause(field)
    }

    fn parse_synchronized_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        field.synchronized = true;
        Ok(())
    }

    fn parse_value_field_clause(&mut self, field: &mut Field) -> Result<()> {
        if field.level == 88 {
            self.advance();
            return self.parse_level88_value_clause(field);
        }

        // Skip VALUE clauses for non-88 fields (metadata only).
        self.skip_value_clause()
    }

    fn parse_blank_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.parse_blank_when_zero_clause(field)
    }

    fn parse_sign_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        if FeatureFlags::global().is_enabled(Feature::SignSeparate) {
            return self.parse_sign_clause(field);
        }

        Err(Error::new(
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
            format!(
                "SIGN clause on field '{}' is not supported (enable with --enable-features sign_separate)",
                field.name
            ),
        ))
    }

    fn parse_comp_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.convert_to_binary_field(field)
    }

    fn parse_comp3_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.convert_to_packed_field(field)
    }

    fn parse_comp1_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.require_feature_enabled(Feature::Comp1, &field.name, "comp_1", "COMP-1")?;
        field.kind = FieldKind::FloatSingle;
        Ok(())
    }

    fn parse_comp2_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.require_feature_enabled(Feature::Comp2, &field.name, "comp_2", "COMP-2")?;
        field.kind = FieldKind::FloatDouble;
        Ok(())
    }

    fn parse_binary_field_clause(&mut self, field: &mut Field) -> Result<()> {
        self.advance();
        self.convert_to_binary_field(field)
    }
}
