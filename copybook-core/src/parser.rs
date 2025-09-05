//! COBOL copybook parser
//!
//! This module implements the parsing logic for COBOL copybooks,
//! including lexical analysis and AST construction.

use crate::error::ErrorCode;
use crate::lexer::{CobolFormat, Lexer, Token, TokenPos};
use crate::pic::PicClause;
use crate::schema::{Field, FieldKind, Occurs, Schema};
use crate::{Error, Result};
use serde_json::Value;

/// Parse a COBOL copybook text into a schema
///
/// # Errors
///
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse(text: &str) -> Result<Schema> {
    parse_with_options(text, &ParseOptions::default())
}

/// Parse a COBOL copybook text into a schema with specific options
///
/// # Errors
///
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse_with_options(text: &str, options: &ParseOptions) -> Result<Schema> {
    if text.trim().is_empty() {
        return Err(Error::new(ErrorCode::CBKP001_SYNTAX, "Empty copybook text"));
    }

    let mut lexer = Lexer::new(text);
    let tokens = lexer.tokenize();

    let mut parser = Parser::with_options(tokens, lexer.format(), options.clone());
    parser.parse_schema()
}

/// Options for parsing behavior
#[derive(Debug, Clone)]
pub struct ParseOptions {
    /// Whether to emit FILLER fields in output
    pub emit_filler: bool,
    /// Codepage for fingerprint calculation
    pub codepage: String,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            emit_filler: false,
            codepage: "cp037".to_string(),
        }
    }
}

/// Parser state for COBOL copybook parsing
struct Parser {
    tokens: Vec<TokenPos>,
    current: usize,
    _format: CobolFormat,
    options: ParseOptions,
    /// Track field names at each level for duplicate detection
    _name_counters: std::collections::HashMap<String, u32>,
}

impl Parser {
    #[allow(dead_code)]
    fn new(tokens: Vec<TokenPos>, format: CobolFormat) -> Self {
        Self {
            tokens,
            current: 0,
            _format: format,
            options: ParseOptions::default(),
            _name_counters: std::collections::HashMap::new(),
        }
    }

    fn with_options(tokens: Vec<TokenPos>, format: CobolFormat, options: ParseOptions) -> Self {
        Self {
            tokens,
            current: 0,
            _format: format,
            options,
            _name_counters: std::collections::HashMap::new(),
        }
    }

    /// Parse the complete schema
    fn parse_schema(&mut self) -> Result<Schema> {
        // Skip any leading comments or empty lines
        self.skip_comments_and_newlines();

        // Parse all field definitions into a flat list first
        let mut flat_fields = Vec::new();
        while !self.is_at_end() {
            if let Some(field) = self.parse_field()? {
                flat_fields.push(field);
            }
            self.skip_comments_and_newlines();
        }

        if flat_fields.is_empty() {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "No valid field definitions found",
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

            // Pop fields from stack that are at same or higher level
            while let Some(top) = stack.last() {
                if top.level >= field.level {
                    let mut completed_field = stack.pop().unwrap();

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

    /// Build hierarchical paths for all fields (simplified)
    #[allow(dead_code)]
    fn build_field_paths(&mut self, _fields: &mut [Field]) -> Result<()> {
        // Simplified for now - paths are set in build_hierarchy
        Ok(())
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
        let all_fields = self.collect_all_fields(fields);

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

    /// Validate ODO constraints
    fn validate_odo_constraints(&self, fields: &[Field]) -> Result<()> {
        let all_fields = self.collect_all_fields(fields);

        for field in &all_fields {
            if let Some(Occurs::ODO { counter_path, .. }) = &field.occurs {
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

                // Validate that ODO array is at tail position
                // This is a simplified check - full validation would require layout resolution
                if !self.is_odo_at_tail(field, &all_fields) {
                    return Err(Error::new(
                        ErrorCode::CBKP021_ODO_NOT_TAIL,
                        format!(
                            "ODO array '{}' must be at tail position of its containing group",
                            field.name
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
        }

        Ok(())
    }

    /// Check if ODO array is at tail position (simplified check)
    fn is_odo_at_tail(&self, _odo_field: &Field, _all_fields: &[&Field]) -> bool {
        // This is a simplified implementation
        // Full validation would require layout resolution to check byte positions
        true
    }

    /// Collect all fields in a flat list
    fn collect_all_fields<'a>(&self, fields: &'a [Field]) -> Vec<&'a Field> {
        let mut result = Vec::new();
        for field in fields {
            result.push(field);
            let children = self.collect_all_fields(&field.children);
            result.extend(children);
        }
        result
    }

    /// Calculate schema fingerprint using SHA-256
    fn calculate_schema_fingerprint(&self, schema: &mut Schema) {
        use sha2::{Digest, Sha256};

        // Create canonical JSON representation for fingerprinting
        let canonical_json = self.create_canonical_schema_json(schema);

        // Create hasher and add canonical JSON
        let mut hasher = Sha256::new();
        hasher.update(canonical_json.as_bytes());

        // Add codepage and options
        hasher.update(self.options.codepage.as_bytes());
        hasher.update(&[if self.options.emit_filler { 1 } else { 0 }]);

        // Compute final hash
        let result = hasher.finalize();
        schema.fingerprint = format!("{:x}", result);
    }

    /// Create canonical JSON representation of schema for fingerprinting
    fn create_canonical_schema_json(&self, schema: &Schema) -> String {
        use serde_json::{Map, Value};

        let mut schema_obj = Map::new();

        // Add fields in canonical order
        let fields_json: Vec<Value> = schema
            .fields
            .iter()
            .map(|f| self.field_to_canonical_json(f))
            .collect();
        schema_obj.insert("fields".to_string(), Value::Array(fields_json));

        // Add schema-level properties
        if let Some(lrecl) = schema.lrecl_fixed {
            schema_obj.insert("lrecl_fixed".to_string(), Value::Number(lrecl.into()));
        }

        if let Some(ref tail_odo) = schema.tail_odo {
            let mut tail_odo_obj = Map::new();
            tail_odo_obj.insert(
                "counter_path".to_string(),
                Value::String(tail_odo.counter_path.clone()),
            );
            tail_odo_obj.insert(
                "min_count".to_string(),
                Value::Number(tail_odo.min_count.into()),
            );
            tail_odo_obj.insert(
                "max_count".to_string(),
                Value::Number(tail_odo.max_count.into()),
            );
            tail_odo_obj.insert(
                "array_path".to_string(),
                Value::String(tail_odo.array_path.clone()),
            );
            schema_obj.insert("tail_odo".to_string(), Value::Object(tail_odo_obj));
        }

        // Convert to canonical JSON string (sorted keys)
        serde_json::to_string(&Value::Object(schema_obj)).unwrap_or_default()
    }

    /// Convert field to canonical JSON for fingerprinting
    fn field_to_canonical_json(&self, field: &Field) -> Value {
        use serde_json::{Map, Value};

        let mut field_obj = Map::new();

        // Add fields in canonical order
        field_obj.insert("path".to_string(), Value::String(field.path.clone()));
        field_obj.insert("name".to_string(), Value::String(field.name.clone()));
        field_obj.insert("level".to_string(), Value::Number(field.level.into()));

        // Add field kind
        let kind_str = match &field.kind {
            FieldKind::Alphanum { len } => format!("Alphanum({})", len),
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                format!("ZonedDecimal({},{},{})", digits, scale, signed)
            }
            FieldKind::BinaryInt { bits, signed } => {
                format!("BinaryInt({},{})", bits, signed)
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                format!("PackedDecimal({},{},{})", digits, scale, signed)
            }
            FieldKind::Group => "Group".to_string(),
        };
        field_obj.insert("kind".to_string(), Value::String(kind_str));

        // Add optional fields
        if let Some(ref redefines) = field.redefines_of {
            field_obj.insert("redefines_of".to_string(), Value::String(redefines.clone()));
        }

        if let Some(ref occurs) = field.occurs {
            let occurs_str = match occurs {
                Occurs::Fixed { count } => format!("Fixed({})", count),
                Occurs::ODO {
                    min,
                    max,
                    counter_path,
                } => {
                    format!("ODO({},{},{})", min, max, counter_path)
                }
            };
            field_obj.insert("occurs".to_string(), Value::String(occurs_str));
        }

        if field.synchronized {
            field_obj.insert("synchronized".to_string(), Value::Bool(true));
        }

        if field.blank_when_zero {
            field_obj.insert("blank_when_zero".to_string(), Value::Bool(true));
        }

        // Add children recursively
        if !field.children.is_empty() {
            let children_json: Vec<Value> = field
                .children
                .iter()
                .map(|c| self.field_to_canonical_json(c))
                .collect();
            field_obj.insert("children".to_string(), Value::Array(children_json));
        }

        Value::Object(field_obj)
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
                // Skip 66-level (rename) entries
                self.skip_to_period();
                return Ok(None);
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
                // Skip 88-level (condition) entries
                self.skip_to_period();
                return Ok(None);
            }
            _ => return Ok(None),
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
                // Skip VALUE clauses (metadata only)
                self.skip_value_clause()?;
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
        let count = match self.current_token() {
            Some(TokenPos {
                token: Token::Number(n),
                ..
            }) => {
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

        // Skip optional TIMES keyword first
        if self.check(&Token::Times) {
            self.advance();
        }

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
                min: 0, // Will be validated later
                max: count,
                counter_path: counter_field,
            });
        } else {
            field.occurs = Some(Occurs::Fixed { count });
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
        let input = r#"
01 RECORD-A.
   05 FIELD-NAME PIC X(10).
   05 FIELD-NAME PIC 9(5).
"#;
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
        let input = r#"
01 COUNTER PIC 9(3).
01 ARRAY-FIELD PIC X(10) OCCURS 5 TIMES DEPENDING ON COUNTER.
"#;
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
        let input = r#"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES FIELD-A PIC 9(10).
"#;
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
        let input = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID PIC X(10).
   05 CUSTOMER-NAME.
      10 FIRST-NAME PIC X(20).
      10 LAST-NAME PIC X(30).
   05 CUSTOMER-ADDRESS.
      10 STREET PIC X(38).
      10 CITY PIC X(30).
"#;
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
        let input = r#"
01 FIELD-A PIC X(10).
01 FIELD-B REDEFINES NONEXISTENT PIC 9(10).
"#;
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
