//! COBOL copybook parser
//!
//! This module implements the parsing logic for COBOL copybooks,
//! including lexical analysis and AST construction.

use crate::error;
use crate::error::{ErrorCode, ErrorContext};
use crate::lexer::{Lexer, Token, TokenPos};
use crate::pic::PicClause;
use crate::schema::{Field, FieldKind, Occurs, Schema};
use crate::utils::VecExt;
use crate::{Error, Result};

/// Parse a COBOL copybook text into a schema
///
/// # Errors
/// Returns an error if the copybook contains syntax errors or unsupported features.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse(text: &str) -> Result<Schema> {
    parse_with_options(text, &ParseOptions::default())
}

/// Parse a COBOL copybook text into a schema with specific options
///
/// # Errors
/// Returns an error if the copybook contains syntax errors or unsupported features.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_with_options(text: &str, options: &ParseOptions) -> Result<Schema> {
    if text.trim().is_empty() {
        return Err(error!(ErrorCode::CBKP001_SYNTAX, "Empty copybook text"));
    }

    let tokens = Lexer::new_with_options(text, options).tokenize();
    let mut parser = Parser::with_options(tokens, options.clone());
    parser.parse_schema()
}

/// Options for parsing behavior
#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct ParseOptions {
    /// Whether to emit FILLER fields in output
    pub emit_filler: bool,
    /// Codepage for fingerprint calculation
    pub codepage: String,
    /// Whether to allow inline comments (*>)
    pub allow_inline_comments: bool,
    /// Whether to run in strict mode (more error intolerance)
    pub strict: bool,
    /// Whether to enforce strict comment parsing rules
    pub strict_comments: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            emit_filler: false,
            codepage: "cp037".to_string(),
            allow_inline_comments: true,
            strict: false,
            strict_comments: false,
        }
    }
}

/// Parser state for COBOL copybook parsing
struct Parser {
    tokens: Vec<TokenPos>,
    current: usize,
    options: ParseOptions,
}

impl Parser {
    fn with_options(tokens: Vec<TokenPos>, options: ParseOptions) -> Self {
        Self {
            tokens,
            current: 0,
            options,
        }
    }

    /// Parse the complete schema
    fn parse_schema(&mut self) -> Result<Schema> {
        // Skip any leading comments or empty lines
        self.skip_comments_and_newlines()?;

        // Parse all field definitions into a flat list first
        let mut flat_fields = Vec::new();
        while !self.is_at_end() {
            if let Some(field) = self.parse_field()? {
                flat_fields.push(field);
            }
            self.skip_comments_and_newlines()?;
        }

        if flat_fields.is_empty() {
            return Err(error!(
                ErrorCode::CBKP001_SYNTAX,
                "No valid field definitions found"
            ));
        }

        // Build hierarchical structure from flat fields
        let hierarchical_fields = self.build_hierarchy(flat_fields)?;

        // Validate the structure (REDEFINES targets, ODO constraints, etc.)
        self.validate_structure(&hierarchical_fields)?;

        // Create schema with fingerprint
        let mut schema = Schema::from_fields(hierarchical_fields);

        // Resolve field layouts and compute offsets
        crate::layout::resolve_layout(&mut schema)?;

        self.calculate_schema_fingerprint(&mut schema);

        Ok(schema)
    }

    /// Build hierarchical structure from flat field list
    fn build_hierarchy(&mut self, mut flat_fields: Vec<Field>) -> Result<Vec<Field>> {
        if flat_fields.is_empty() {
            return Ok(Vec::new());
        }

        // Handle duplicate names and FILLER fields
        self.process_field_names(&mut flat_fields);

        // Build hierarchical structure using a stack-based approach
        let mut stack: Vec<Field> = Vec::new();
        let mut result: Vec<Field> = Vec::new();

        for mut field in flat_fields {
            // Set initial path
            field.path = field.name.clone();

            // Special handling for level-88 (condition values) and level-66 (RENAMES)
            if field.level == 88 {
                // Level-88 is a child of the immediately preceding field
                if let Some(parent) = stack.last_mut() {
                    field.path = format!("{}.{}", parent.path, field.name);
                    parent.children.push(field);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        "Level-88 condition must follow a data field".to_string(),
                    ));
                }
                continue;
            }

            // Level-66 (RENAMES) is a non-storage sibling under the same parent group
            let is_renames = field.level == 66;

            // Special handling for RENAMES: close all scopes back to level-01
            if is_renames {
                // Level-66 should trigger closing of all open scopes (groups and leaf fields)
                // back to the level-01 record, just like encountering a new level-01 sibling would.
                // We pop fields and attach each to its parent as we go (like normal field processing),
                // stopping when we reach the level-01 record.
                while let Some(top) = stack.last() {
                    // Stop at level-01 (always keep it on stack)
                    if top.level == 1 {
                        break;
                    }

                    // Pop this field and attach it to its parent
                    let mut completed_field = stack.pop_or_cbkp_error(
                        ErrorCode::CBKP001_SYNTAX,
                        "Parser stack underflow while attaching RENAMES",
                    )?;

                    // Mark as group if it has children
                    if !completed_field.children.is_empty() {
                        completed_field.kind = FieldKind::Group;
                    }

                    // Attach to parent (like normal field processing at lines 220-223)
                    if let Some(parent) = stack.last_mut() {
                        completed_field.path = format!("{}.{}", parent.path, completed_field.name);
                        parent.children.push(completed_field);
                    } else {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            "Level-66 RENAMES must be within a record group".to_string(),
                        ));
                    }
                }

                // Now attach the level-66 field itself as a sibling
                let parent = stack.last_mut().ok_or_else(|| {
                    Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        "Level-66 RENAMES must be within a record group".to_string(),
                    )
                })?;

                field.path = format!("{}.{}", parent.path, field.name);
                parent.children.push(field);
                continue;
            }

            // Pop fields from stack that are at same or higher level (normal fields)
            while let Some(top) = stack.last() {
                if top.level >= field.level {
                    let mut completed_field = stack.pop_or_cbkp_error(
                        ErrorCode::CBKP001_SYNTAX,
                        "Parser stack underflow: expected field to pop but stack was empty",
                    )?;

                    // If this field has children, make it a group
                    if !completed_field.children.is_empty() {
                        completed_field.kind = FieldKind::Group;
                    }

                    // Add to parent or result
                    if let Some(parent) = stack.last_mut() {
                        // Update path to include parent
                        completed_field.path = format!("{}.{}", parent.path, completed_field.name);
                        parent.children.push(completed_field);
                    } else {
                        result.push(completed_field);
                    }
                } else {
                    break;
                }
            }

            // Update path if we have a parent
            if let Some(parent) = stack.last() {
                field.path = format!("{}.{}", parent.path, field.name);
            }

            stack.push(field);
        }

        // Pop remaining fields from stack
        while let Some(mut field) = stack.pop() {
            // If this field has children, make it a group
            if !field.children.is_empty() {
                field.kind = FieldKind::Group;
            }

            // Level-77 is an independent item, always goes to result (not under a parent)
            if field.level == 77 {
                result.push(field);
                continue;
            }

            // Add to parent or result
            if let Some(parent) = stack.last_mut() {
                parent.children.push(field);
            } else {
                result.push(field);
            }
        }

        Ok(result)
    }

    /// Process field names for duplicates and FILLER handling
    fn process_field_names(&mut self, fields: &mut [Field]) {
        // First pass: handle FILLER fields
        for field in fields.iter_mut() {
            if field.name.to_uppercase() == "FILLER" {
                if self.options.emit_filler {
                    // Replace FILLER with _filler_<offset> (offset will be calculated later in layout resolution)
                    // For now, use a placeholder that will be updated
                    field.name = format!("_filler_{}", 0);
                } else {
                    // Keep FILLER name for now, will be filtered out in layout resolution
                    field.name = "FILLER".to_string();
                }
            }
        }

        // Second pass: handle duplicate names at each level
        self.process_duplicates_at_level(fields, 0);
    }

    /// Process duplicate names recursively by level
    fn process_duplicates_at_level(&mut self, fields: &mut [Field], parent_level: u8) {
        let mut name_counts: std::collections::HashMap<String, u32> =
            std::collections::HashMap::new();
        let mut siblings = Vec::new();

        // Find all siblings at the next level
        for (i, field) in fields.iter().enumerate() {
            if field.level == parent_level + 1 || (parent_level == 0 && field.level <= 49) {
                siblings.push(i);
            }
        }

        // Process duplicates among siblings
        for &i in &siblings {
            let field_name = fields[i].name.clone();

            // Skip FILLER fields that won't be emitted
            if field_name == "FILLER" && !self.options.emit_filler {
                continue;
            }

            let count = name_counts.entry(field_name.clone()).or_insert(0);
            *count += 1;

            if *count > 1 {
                fields[i].name = format!("{}__dup{}", field_name, count);
            }
        }
    }

    /// Validate the parsed structure
    fn validate_structure(&self, fields: &[Field]) -> Result<()> {
        // Validate REDEFINES targets
        self.validate_redefines(fields)?;

        // Validate ODO constraints
        self.validate_odo_constraints(fields)?;

        Ok(())
    }

    /// Validate REDEFINES relationships
    fn validate_redefines(&self, fields: &[Field]) -> Result<()> {
        let all_fields = Self::collect_all_fields(fields);

        for field in &all_fields {
            if let Some(ref target) = field.redefines_of {
                // Find the target field
                let target_found = all_fields
                    .iter()
                    .any(|f| f.name == *target || f.path == *target);

                if !target_found {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!(
                            "REDEFINES target '{}' not found for field '{}'",
                            target, field.name
                        ),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Validate ODO constraints using hierarchical structure
    fn validate_odo_constraints(&self, fields: &[Field]) -> Result<()> {
        // Collect all fields for counter lookups
        let all_fields = Self::collect_all_fields(fields);

        // Validate each field group hierarchically
        for field in fields {
            self.validate_odo_in_group(field, &all_fields, false)?;
        }

        Ok(())
    }

    /// Check if a field is inside a REDEFINES region by walking the path
    fn is_inside_redefines(&self, field_path: &str, all_fields: &[&Field]) -> bool {
        // Check all ancestor paths to see if any have redefines_of
        for ancestor in all_fields {
            if field_path.starts_with(&format!("{}.", ancestor.path))
                && ancestor.redefines_of.is_some()
            {
                return true;
            }
        }
        false
    }

    /// Recursively validate ODO constraints within a field group
    ///
    /// # Arguments
    /// * `field` - The field to validate
    /// * `all_fields` - All fields for counter lookups
    /// * `inside_occurs` - Whether we're already inside an OCCURS/ODO array
    fn validate_odo_in_group(
        &self,
        field: &Field,
        all_fields: &[&Field],
        inside_occurs: bool,
    ) -> Result<()> {
        // Check if this field is an ODO array
        if let Some(Occurs::ODO { counter_path, .. }) = &field.occurs {
            // O5: Check for nested ODO (ODO inside OCCURS/ODO)
            if inside_occurs {
                return Err(Error::new(
                    ErrorCode::CBKP022_NESTED_ODO,
                    format!(
                        "Nested ODO not supported: field '{}' has OCCURS DEPENDING ON inside another OCCURS/ODO array",
                        field.path
                    ),
                ));
            }

            // O6: Check for ODO inside REDEFINES region
            if self.is_inside_redefines(&field.path, all_fields) || field.redefines_of.is_some() {
                return Err(Error::new(
                    ErrorCode::CBKP023_ODO_REDEFINES,
                    format!(
                        "ODO over REDEFINES not supported: field '{}' has OCCURS DEPENDING ON inside a REDEFINES region",
                        field.path
                    ),
                ));
            }
            // Find the counter field
            let counter_field = all_fields
                .iter()
                .find(|f| f.name == *counter_path || f.path == *counter_path);

            if counter_field.is_none() {
                return Err(Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!(
                        "ODO counter field '{}' not found for array '{}'",
                        counter_path, field.name
                    ),
                ));
            }

            // Validate that counter is not inside REDEFINES or ODO region
            if let Some(counter) = counter_field {
                if counter.redefines_of.is_some() {
                    return Err(Error::new(
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                        format!(
                            "ODO counter '{}' cannot be inside a REDEFINES region",
                            counter_path
                        ),
                    ));
                }

                if counter.occurs.is_some() {
                    return Err(Error::new(
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                        format!(
                            "ODO counter '{}' cannot be inside an ODO region",
                            counter_path
                        ),
                    ));
                }
            }
        }

        // Determine if we're now inside an OCCURS/ODO region
        let child_inside_occurs = inside_occurs || field.occurs.is_some();

        // Recursively validate children and check ODO tail constraints
        for (i, child) in field.children.iter().enumerate() {
            // Check if child is ODO and enforce tail position rule
            if child
                .occurs
                .as_ref()
                .is_some_and(|o| matches!(o, Occurs::ODO { .. }))
                && !self.is_odo_at_tail_sibling_based(child, &field.children, i)
            {
                return Err(Error::new(
                    ErrorCode::CBKP021_ODO_NOT_TAIL,
                    format!(
                        "ODO array '{}' must be last storage field under '{}'",
                        child.path, field.path
                    ),
                ));
            }

            // Recursively validate this child's subtree, passing down OCCURS context
            self.validate_odo_in_group(child, all_fields, child_inside_occurs)?;
        }

        Ok(())
    }

    /// Check if ODO array is the last storage sibling (structural, sibling-based logic)
    fn is_odo_at_tail_sibling_based(
        &self,
        _odo_field: &Field,
        siblings: &[Field],
        odo_index: usize,
    ) -> bool {
        // Check if there are any storage fields after this ODO field among siblings
        !siblings
            .iter()
            .skip(odo_index + 1)
            .any(|sibling| self.is_storage_field(sibling))
        // Return true if no storage siblings found after ODO
    }

    /// Check if a field is a storage field (excludes level 88 and non-storage field types)
    fn is_storage_field(&self, field: &Field) -> bool {
        // Exclude level 88 (condition names)
        if field.level == 88 {
            return false;
        }

        // Check if field kind would have storage (independent of calculated length)
        match &field.kind {
            FieldKind::Group => {
                // Groups have storage if they have storage children or aren't just containers
                // For validation purposes, consider groups as having potential storage
                true
            }
            FieldKind::Alphanum { .. }
            | FieldKind::ZonedDecimal { .. }
            | FieldKind::BinaryInt { .. }
            | FieldKind::PackedDecimal { .. } => true,
            FieldKind::Condition { .. } => false, // Level-88 fields don't have storage
            FieldKind::Renames { .. } => false,   // Level-66 fields don't have storage
        }
    }

    /// Collect all fields in a flat list
    fn collect_all_fields(fields: &[Field]) -> Vec<&Field> {
        let mut result = Vec::new();
        for field in fields {
            result.push(field);
            let children = Self::collect_all_fields(&field.children);
            result.extend(children);
        }
        result
    }

    /// Calculate schema fingerprint using SHA-256 including parse options
    fn calculate_schema_fingerprint(&self, schema: &mut Schema) {
        use sha2::{Digest, Sha256};

        // Use schema's canonical JSON representation
        let canonical_json = schema.create_canonical_json();

        // Create hasher and add canonical JSON
        let mut hasher = Sha256::new();
        hasher.update(canonical_json.as_bytes());

        // Add parse-specific options that affect fingerprint
        hasher.update(self.options.codepage.as_bytes());
        hasher.update([if self.options.emit_filler { 1 } else { 0 }]);

        // Compute final hash
        let result = hasher.finalize();
        schema.fingerprint = format!("{:x}", result);
    }

    /// Parse a single field definition
    fn parse_field(&mut self) -> Result<Option<Field>> {
        // Look for level number
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
                // Parse 66-level (RENAMES) entries
                let level = 66;
                self.advance();
                level
            }
            Some(TokenPos {
                token: Token::Level77,
                ..
            }) => {
                let level = 77;
                self.advance();
                level
            }
            Some(TokenPos {
                token: Token::Level88,
                ..
            }) => {
                let level = 88;
                self.advance();
                level
            }
            Some(TokenPos {
                token: Token::Number(n),
                line,
                ..
            }) => {
                // Handle invalid level numbers more carefully
                // 0 and numbers > 49 are invalid as COBOL level numbers
                // But only report as error if they appear in level number position
                if (*n == 0) || (*n >= 50 && *n <= 99) {
                    let line_number = *line;
                    // Convert line number safely, omitting from context if conversion fails
                    // to avoid silently corrupting error information with u32::MAX
                    let safe_line_number = crate::utils::safe_ops::safe_usize_to_u32(
                        line_number,
                        "error context line number",
                    )
                    .ok();

                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid level number '{}'", n),
                    )
                    .with_context(ErrorContext {
                        record_index: None,
                        field_path: None,
                        byte_offset: None,
                        line_number: safe_line_number,
                        details: None,
                    }));
                } else if *n >= 1 && *n <= 49 {
                    // Valid single-digit level numbers (1-49) without leading zeros
                    // Convert to proper level number
                    let level = *n as u8;
                    self.advance();
                    level
                } else {
                    // Large numbers are likely sequence numbers or other contexts
                    self.advance();
                    return Ok(None);
                }
            }
            _ => {
                // If we encounter an unrecognized token, advance to avoid infinite loop
                self.advance();
                return Ok(None);
            }
        };

        // Get field name
        let mut name = match self.current_token() {
            Some(TokenPos {
                token: Token::Identifier(name),
                ..
            }) => {
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

        // Handle FILLER fields
        if name.to_uppercase() == "FILLER" && !self.options.emit_filler {
            // For now, keep FILLER name - it will be processed later
            name = "FILLER".to_string();
        }

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

        // Validate that level-66 fields have RENAMES clause
        if level == 66 && !matches!(field.kind, FieldKind::Renames { .. }) {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Level-66 field '{}' must have RENAMES clause", field.name),
            ));
        }

        Ok(Some(field))
    }

    /// Parse a field clause (PIC, USAGE, REDEFINES, etc.)
    fn parse_field_clause(&mut self, field: &mut Field) -> Result<()> {
        match self.current_token() {
            Some(TokenPos {
                token: Token::Pic, ..
            }) => {
                self.advance();
                self.parse_pic_clause(field)?;
            }
            Some(TokenPos {
                token: Token::Usage,
                ..
            }) => {
                self.advance();
                self.parse_usage_clause(field)?;
            }
            Some(TokenPos {
                token: Token::Redefines,
                ..
            }) => {
                self.advance();
                self.parse_redefines_clause(field)?;
            }
            Some(TokenPos {
                token: Token::Renames,
                ..
            }) => {
                // RENAMES clause (level-66 only)
                if field.level == 66 {
                    self.parse_renames(field)?;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!(
                            "RENAMES clause can only be used with level-66, not level {}",
                            field.level
                        ),
                    ));
                }
            }
            Some(TokenPos {
                token: Token::Occurs,
                ..
            }) => {
                self.advance();
                self.parse_occurs_clause(field)?;
            }
            Some(TokenPos {
                token: Token::Synchronized,
                ..
            }) => {
                self.advance();
                field.synchronized = true;
            }
            Some(TokenPos {
                token: Token::Value,
                ..
            }) => {
                if field.level == 88 {
                    self.advance();
                    self.parse_level88_value_clause(field)?;
                } else {
                    // Skip VALUE clauses for non-88 fields (metadata only)
                    self.skip_value_clause()?;
                }
            }
            Some(TokenPos {
                token: Token::Blank,
                ..
            }) => {
                self.advance();
                self.parse_blank_when_zero_clause(field)?;
            }
            Some(TokenPos {
                token: Token::Sign, ..
            }) => {
                // SIGN clauses are treated as edited PIC
                return Err(Error::new(
                    ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                    "SIGN clauses are not supported (treated as edited PIC)",
                ));
            }
            Some(TokenPos {
                token: Token::Comp, ..
            }) => {
                self.advance();
                self.convert_to_binary_field(field)?;
            }
            Some(TokenPos {
                token: Token::Comp3,
                ..
            }) => {
                self.advance();
                self.convert_to_packed_field(field)?;
            }
            Some(TokenPos {
                token: Token::Binary,
                ..
            }) => {
                self.advance();
                self.convert_to_binary_field(field)?;
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
        // Collect PIC clause tokens - might be split across multiple tokens
        let mut pic_parts = Vec::new();

        // First token should be a PIC clause or identifier
        match self.current_token() {
            Some(TokenPos {
                token: Token::PicClause(pic),
                ..
            }) => {
                pic_parts.push(pic.clone());
                self.advance();
            }
            Some(TokenPos {
                token: Token::EditedPic(pic),
                ..
            }) => {
                return Err(Error::new(
                    ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                    format!("Edited PIC not supported: {}", pic),
                ));
            }
            Some(TokenPos {
                token: Token::Identifier(id),
                ..
            }) => {
                // This might be part of a PIC clause like "S9(7)" followed by "V99"
                pic_parts.push(id.clone());
                self.advance();
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected PIC clause after PIC keyword",
                ));
            }
        }

        // Check if next token is also part of PIC (like V99 after S9(7))
        while let Some(token) = self.current_token() {
            match &token.token {
                Token::Identifier(id) if id.starts_with('V') || id.starts_with('v') => {
                    pic_parts.push(id.clone());
                    self.advance();
                }
                _ => break,
            }
        }

        let pic_str = pic_parts.join("");
        let pic = PicClause::parse(&pic_str)?;

        field.kind = match pic.kind {
            crate::pic::PicKind::Alphanumeric => FieldKind::Alphanum {
                len: pic.digits as u32,
            },
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
            Some(TokenPos {
                token: Token::Display,
                ..
            }) => {
                self.advance();
                // USAGE DISPLAY is the default, no change needed
            }
            Some(TokenPos {
                token: Token::Comp, ..
            }) => {
                self.advance();
                self.convert_to_binary_field(field)?;
            }
            Some(TokenPos {
                token: Token::Comp3,
                ..
            }) => {
                self.advance();
                self.convert_to_packed_field(field)?;
            }
            Some(TokenPos {
                token: Token::Binary,
                ..
            }) => {
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
            Some(TokenPos {
                token: Token::Identifier(name),
                ..
            }) => {
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
        let min = match self.current_token() {
            Some(TokenPos {
                token: Token::Number(n),
                ..
            }) => {
                let min = *n;
                self.advance();
                min
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected number after OCCURS",
                ));
            }
        };

        // Check for TO keyword (range syntax)
        let max = if self.check(&Token::To) {
            self.advance(); // consume TO
            match self.current_token() {
                Some(TokenPos {
                    token: Token::Number(n),
                    ..
                }) => {
                    let max = *n;
                    self.advance();
                    max
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        "Expected number after TO in OCCURS clause",
                    ));
                }
            }
        } else {
            min // If no TO, then min == max (fixed count)
        };

        // Skip optional TIMES keyword
        if self.check(&Token::Times) {
            self.advance();
        }

        // Look for DEPENDING ON (might not be immediately next)
        let depending_pos = self.find_depending_in_clause();

        if let Some(depending_idx) = depending_pos {
            // Found DEPENDING, advance to it
            self.current = depending_idx;
            self.advance(); // consume DEPENDING

            if !self.consume(&Token::On) {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected ON after DEPENDING",
                ));
            }

            let counter_field = match self.current_token() {
                Some(TokenPos {
                    token: Token::Identifier(name),
                    ..
                }) => {
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
                min,
                max,
                counter_path: counter_field,
            });
        } else {
            // No DEPENDING ON found
            if min != max {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Range syntax (min TO max) requires DEPENDING ON clause",
                ));
            }
            field.occurs = Some(Occurs::Fixed { count: min });
        }

        Ok(())
    }

    /// Find DEPENDING token in the current clause (before the next field starts)
    fn find_depending_in_clause(&self) -> Option<usize> {
        for i in self.current..self.tokens.len() {
            match &self.tokens[i].token {
                Token::Depending => return Some(i),
                // Stop looking when we hit the start of a new field
                Token::Level(_) | Token::Level66 | Token::Level77 | Token::Level88 => break,
                _ => {}
            }
        }
        None
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

    /// Parse Level-88 VALUE clause
    fn parse_level88_value_clause(&mut self, field: &mut Field) -> Result<()> {
        let mut values = Vec::new();

        // Parse VALUE clauses - can be literals, ranges, or multiple values
        loop {
            match self.current_token() {
                Some(TokenPos {
                    token: Token::StringLiteral(s),
                    ..
                }) => {
                    values.push(s.clone());
                    self.advance();
                }
                Some(TokenPos {
                    token: Token::Number(n),
                    ..
                }) => {
                    values.push(n.to_string());
                    self.advance();
                }
                Some(TokenPos {
                    token: Token::Identifier(id),
                    ..
                }) if id.to_uppercase() == "ZEROS" || id.to_uppercase() == "ZEROES" => {
                    values.push("ZEROS".to_string());
                    self.advance();
                }
                Some(TokenPos {
                    token: Token::Identifier(id),
                    ..
                }) if id.to_uppercase() == "SPACES" => {
                    values.push("SPACES".to_string());
                    self.advance();
                }
                _ => break,
            }

            // Check for THROUGH/THRU ranges or additional values
            let range_keyword = match self.current_token() {
                Some(TokenPos {
                    token: Token::Through,
                    ..
                }) => {
                    self.advance();
                    "THROUGH"
                }
                Some(TokenPos {
                    token: Token::Thru, ..
                }) => {
                    self.advance();
                    "THRU"
                }
                _ => {
                    // No range keyword, continue to next value/comma
                    if let Some(TokenPos {
                        token: Token::Comma,
                        ..
                    }) = self.current_token()
                    {
                        self.advance();
                    }
                    continue;
                }
            };

            // Parse the range end value
            match self.current_token() {
                Some(TokenPos {
                    token: Token::StringLiteral(s),
                    ..
                }) => {
                    // Replace last value with range notation
                    if let Some(last) = values.last_mut() {
                        *last = format!("{} {} {}", last, range_keyword, s);
                    }
                    self.advance();
                }
                Some(TokenPos {
                    token: Token::Number(n),
                    ..
                }) => {
                    if let Some(last) = values.last_mut() {
                        *last = format!("{} {} {}", last, range_keyword, n);
                    }
                    self.advance();
                }
                _ => break,
            }

            // Optionally consume a comma before the next value
            self.consume(&Token::Comma);
        }

        if values.is_empty() {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Level-88 VALUE clause requires at least one value",
            ));
        }

        // Set field kind to Condition
        field.kind = FieldKind::Condition { values };

        Ok(())
    }

    /// Parse a qualified name (QNAME): IDENT (OF IDENT)*
    fn parse_qualified_name(&mut self) -> Result<String> {
        let mut parts = Vec::new();

        // Parse first identifier
        match self.current_token() {
            Some(TokenPos {
                token: Token::Identifier(name),
                ..
            }) => {
                parts.push(name.clone());
                self.advance();
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected identifier in qualified name",
                ));
            }
        }

        // Parse optional "OF IDENT" sequences
        loop {
            match self.current_token() {
                Some(TokenPos {
                    token: Token::Identifier(name),
                    ..
                }) if name.eq_ignore_ascii_case("OF") => {
                    self.advance(); // consume OF
                    match self.current_token() {
                        Some(TokenPos {
                            token: Token::Identifier(next_name),
                            ..
                        }) => {
                            parts.push("OF".to_string());
                            parts.push(next_name.clone());
                            self.advance();
                        }
                        _ => {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                "Expected identifier after OF in qualified name",
                            ));
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(parts.join(" "))
    }

    /// Parse Level-66 RENAMES clause
    ///
    /// Syntax: 66 NAME RENAMES from-field THROUGH|THRU thru-field.
    /// Field names can be qualified: IDENT (OF IDENT)*
    fn parse_renames(&mut self, field: &mut Field) -> Result<()> {
        // Expect RENAMES keyword
        match self.current_token() {
            Some(TokenPos {
                token: Token::Renames,
                ..
            }) => {
                self.advance();
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected RENAMES keyword after level-66 field name",
                ));
            }
        }

        // Parse from-field qualified name
        let from_field = self.parse_qualified_name()?;

        // Expect THROUGH or THRU keyword
        match self.current_token() {
            Some(TokenPos {
                token: Token::Through | Token::Thru,
                ..
            }) => {
                self.advance();
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    "Expected THROUGH or THRU keyword after from-field in RENAMES",
                ));
            }
        }

        // Parse thru-field qualified name
        let thru_field = self.parse_qualified_name()?;

        // Set field kind to Renames
        field.kind = FieldKind::Renames {
            from_field,
            thru_field,
        };

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

                field.kind = FieldKind::BinaryInt {
                    bits,
                    signed: *signed,
                };
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
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
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

    /// Skip comments and newlines
    fn skip_comments_and_newlines(&mut self) -> Result<()> {
        while let Some(token) = self.current_token() {
            match &token.token {
                Token::InlineComment(_) => {
                    if self.options.strict_comments {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            "Inline comments (*>) are not allowed in strict mode",
                        ));
                    }
                    self.advance();
                }
                Token::Newline => {
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Check if current token is a keyword
    fn is_keyword(&self) -> bool {
        matches!(
            self.current_token().map(|t| &t.token),
            Some(
                Token::Pic
                    | Token::Usage
                    | Token::Redefines
                    | Token::Occurs
                    | Token::Synchronized
                    | Token::Value
                    | Token::Blank
                    | Token::Sign
            )
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
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
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
            FieldKind::ZonedDecimal {
                digits: 9,
                scale: 2,
                signed: true
            }
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
            FieldKind::BinaryInt {
                bits: 32,
                signed: false
            }
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
        let input = r"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES FIELD-A PIC 9(10).
";
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
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
        ));
    }

    #[test]
    fn test_sign_clause_rejection() {
        let input = "01 AMOUNT PIC S9(5) SIGN LEADING.";
        let result = parse(input);

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
        ));
    }

    #[test]
    fn test_schema_fingerprint() {
        let input = "01 CUSTOMER-ID PIC X(10).";
        let schema = parse(input).unwrap();

        // Should have a non-empty fingerprint
        assert!(!schema.fingerprint.is_empty());
        assert_ne!(schema.fingerprint, "placeholder");

        // Same input should produce same fingerprint
        let schema2 = parse(input).unwrap();
        assert_eq!(schema.fingerprint, schema2.fingerprint);
    }

    #[test]
    fn test_duplicate_name_handling() {
        let input = r"
01 RECORD-A.
   05 FIELD-NAME PIC X(10).
   05 FIELD-NAME PIC 9(5).
";
        let schema = parse(input).unwrap();

        // Should have one root field with hierarchical structure
        assert_eq!(schema.fields.len(), 1);
        let root = &schema.fields[0];
        assert_eq!(root.name, "RECORD-A");
        assert!(matches!(root.kind, FieldKind::Group));

        // Root should have 2 children with disambiguated names
        assert_eq!(root.children.len(), 2);
        assert_eq!(root.children[0].name, "FIELD-NAME");
        assert_eq!(root.children[1].name, "FIELD-NAME__dup2");
    }

    #[test]
    fn test_odo_validation() {
        let input = r"
01 COUNTER PIC 9(3).
01 ARRAY-FIELD PIC X(10) OCCURS 5 TIMES DEPENDING ON COUNTER.
";
        let result = parse(input);

        // Should succeed with valid ODO structure
        assert!(result.is_ok());
        let schema = result.unwrap();
        assert_eq!(schema.fields.len(), 2);

        // Check ODO field
        let odo_field = &schema.fields[1];
        if let Some(Occurs::ODO {
            max, counter_path, ..
        }) = &odo_field.occurs
        {
            assert_eq!(*max, 5);
            assert_eq!(counter_path, "COUNTER");
        } else {
            panic!("Expected ODO occurs, got {:?}", odo_field.occurs);
        }
    }

    #[test]
    fn test_redefines_validation() {
        let input = r"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES FIELD-A PIC 9(10).
";
        let result = parse(input);

        // Should succeed with valid REDEFINES
        assert!(result.is_ok());
        let schema = result.unwrap();
        assert_eq!(schema.fields.len(), 2);

        let field_b = &schema.fields[1];
        assert_eq!(field_b.redefines_of, Some("FIELD-A".to_string()));
    }

    #[test]
    fn test_hierarchical_structure() {
        let input = r"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID PIC X(10).
   05 CUSTOMER-NAME.
      10 FIRST-NAME PIC X(20).
      10 LAST-NAME PIC X(30).
   05 CUSTOMER-ADDRESS.
      10 STREET PIC X(38).
      10 CITY PIC X(30).
";
        let schema = parse(input).unwrap();

        // Should have one root field
        assert_eq!(schema.fields.len(), 1);
        let root = &schema.fields[0];
        assert_eq!(root.name, "CUSTOMER-RECORD");
        assert!(matches!(root.kind, FieldKind::Group));

        // Root should have 3 children
        assert_eq!(root.children.len(), 3);

        // Check first child
        let customer_id = &root.children[0];
        assert_eq!(customer_id.name, "CUSTOMER-ID");
        assert_eq!(customer_id.path, "CUSTOMER-RECORD.CUSTOMER-ID");
        assert!(matches!(customer_id.kind, FieldKind::Alphanum { len: 10 }));

        // Check second child (group)
        let customer_name = &root.children[1];
        assert_eq!(customer_name.name, "CUSTOMER-NAME");
        assert_eq!(customer_name.path, "CUSTOMER-RECORD.CUSTOMER-NAME");
        assert!(matches!(customer_name.kind, FieldKind::Group));
        assert_eq!(customer_name.children.len(), 2);

        // Check nested children
        let first_name = &customer_name.children[0];
        assert_eq!(first_name.name, "FIRST-NAME");
        assert_eq!(first_name.path, "CUSTOMER-RECORD.CUSTOMER-NAME.FIRST-NAME");
    }

    #[test]
    fn test_sha256_fingerprint() {
        let input = "01 CUSTOMER-ID PIC X(10).";
        let schema = parse(input).unwrap();

        // Should have a SHA-256 fingerprint (64 hex characters)
        assert_eq!(schema.fingerprint.len(), 64);
        assert!(schema.fingerprint.chars().all(|c| c.is_ascii_hexdigit()));

        // Same input should produce same fingerprint
        let schema2 = parse(input).unwrap();
        assert_eq!(schema.fingerprint, schema2.fingerprint);

        // Different input should produce different fingerprint
        let input2 = "01 CUSTOMER-ID PIC X(20).";
        let schema3 = parse(input2).unwrap();
        assert_ne!(schema.fingerprint, schema3.fingerprint);
    }

    #[test]
    fn test_invalid_redefines_target() {
        let input = r"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES NONEXISTENT PIC 9(10).
";
        let result = parse(input);

        // Should fail with invalid REDEFINES target
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_blank_when_zero() {
        let input = "01 AMOUNT PIC 9(5) BLANK WHEN ZERO.";
        let schema = parse(input).unwrap();

        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert!(field.blank_when_zero);
    }

    #[test]
    fn test_synchronized_field() {
        let input = "01 BINARY-FIELD PIC 9(5) USAGE COMP SYNCHRONIZED.";
        let schema = parse(input).unwrap();

        assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        assert!(field.synchronized);
        assert!(matches!(field.kind, FieldKind::BinaryInt { .. }));
    }
}
