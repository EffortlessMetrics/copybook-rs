//! Schema types for COBOL copybook structures
//!
//! This module defines the core data structures that represent a parsed
//! COBOL copybook schema, including fields, types, and layout information.

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// A parsed COBOL copybook schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Schema {
    /// Root fields in the schema
    pub fields: Vec<Field>,
    /// Fixed record length (LRECL) if applicable
    pub lrecl_fixed: Option<u32>,
    /// Tail ODO information if present
    pub tail_odo: Option<TailODO>,
    /// Schema fingerprint for provenance tracking
    pub fingerprint: String,
}

/// Information about tail ODO (OCCURS DEPENDING ON) arrays
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TailODO {
    /// Path to the ODO counter field
    pub counter_path: String,
    /// Minimum array length
    pub min_count: u32,
    /// Maximum array length
    pub max_count: u32,
    /// Path to the ODO array field
    pub array_path: String,
}

/// A field in the copybook schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    /// Hierarchical path (e.g., "ROOT.CUSTOMER.ID")
    pub path: String,
    /// Field name (last component of path)
    pub name: String,
    /// Level number from copybook
    pub level: u8,
    /// Field type and characteristics
    pub kind: FieldKind,
    /// Byte offset within record
    pub offset: u32,
    /// Field length in bytes
    pub len: u32,
    /// Path of field this redefines (if any)
    pub redefines_of: Option<String>,
    /// Array information (if any)
    pub occurs: Option<Occurs>,
    /// Alignment padding bytes (if SYNCHRONIZED)
    pub sync_padding: Option<u16>,
    /// Whether field is SYNCHRONIZED
    pub synchronized: bool,
    /// Whether field has BLANK WHEN ZERO
    pub blank_when_zero: bool,
    /// Resolved RENAMES information (for level-66 fields only)
    pub resolved_renames: Option<ResolvedRenames>,
    /// Child fields (for groups)
    pub children: Vec<Field>,
}

/// Field type and characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FieldKind {
    /// Alphanumeric field (PIC X)
    Alphanum {
        /// Field length in characters
        len: u32,
    },
    /// Zoned decimal field (PIC 9, display)
    ZonedDecimal {
        /// Total number of digits
        digits: u16,
        /// Decimal places (can be negative for scaling)
        scale: i16,
        /// Whether field is signed
        signed: bool,
    },
    /// Binary integer field (COMP/BINARY)
    BinaryInt {
        /// Number of bits (16, 32, 64)
        bits: u16,
        /// Whether field is signed
        signed: bool,
    },
    /// Packed decimal field (COMP-3)
    PackedDecimal {
        /// Total number of digits
        digits: u16,
        /// Decimal places (can be negative for scaling)
        scale: i16,
        /// Whether field is signed
        signed: bool,
    },
    /// Group field (contains other fields)
    Group,
    /// Level-88 condition field (conditional values)
    Condition {
        /// Condition values (e.g., VALUE 'A', VALUE 1 THROUGH 5)
        values: Vec<String>,
    },
    /// Level-66 RENAMES field (field aliasing/regrouping)
    Renames {
        /// Starting field name in the range
        from_field: String,
        /// Ending field name in the range
        thru_field: String,
    },
    /// Edited numeric field (Phase E2: parse, represent, and decode)
    /// Examples: PIC ZZZ9, PIC $ZZ,ZZ9.99, PIC 9(7)V99CR
    EditedNumeric {
        /// Original PIC string (e.g., "ZZ,ZZZ.99")
        pic_string: String,
        /// Display width (computed from PIC)
        width: u16,
        /// Decimal places (scale) for numeric value
        scale: u16,
        /// Whether field has sign editing
        signed: bool,
    },
}

/// Resolved RENAMES (level-66) alias information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedRenames {
    /// Byte offset of the aliased range
    pub offset: u32,
    /// Total byte length of the aliased range
    pub length: u32,
    /// Paths of fields covered by this alias (in document order)
    pub members: Vec<String>,
}

/// Array occurrence information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Occurs {
    /// Fixed-size array
    Fixed {
        /// Number of elements
        count: u32,
    },
    /// Variable-size array (OCCURS DEPENDING ON)
    ODO {
        /// Minimum number of elements
        min: u32,
        /// Maximum number of elements
        max: u32,
        /// Path to counter field
        counter_path: String,
    },
}

impl Schema {
    /// Create a new empty schema
    #[must_use]
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            lrecl_fixed: None,
            tail_odo: None,
            fingerprint: String::new(),
        }
    }

    /// Create a schema from a list of fields
    #[must_use]
    pub fn from_fields(fields: Vec<Field>) -> Self {
        let mut schema = Self {
            fields,
            lrecl_fixed: None,
            tail_odo: None,
            fingerprint: String::new(),
        };
        schema.calculate_fingerprint();
        schema
    }

    /// Calculate the schema fingerprint using SHA-256
    pub fn calculate_fingerprint(&mut self) {
        use sha2::{Digest, Sha256};

        // Create canonical JSON representation
        let canonical_json = self.create_canonical_json();

        // Calculate SHA-256 hash
        let mut hasher = Sha256::new();
        hasher.update(canonical_json.as_bytes());

        let result = hasher.finalize();
        self.fingerprint = format!("{result:x}");
    }

    /// Create canonical JSON representation for fingerprinting
    pub fn create_canonical_json(&self) -> String {
        use serde_json::{Map, Value};

        let mut schema_obj = Map::new();

        // Add fields in canonical order
        let fields_json: Vec<Value> = self
            .fields
            .iter()
            .map(Self::field_to_canonical_json)
            .collect();
        schema_obj.insert("fields".to_string(), Value::Array(fields_json));

        // Add schema-level properties
        if let Some(lrecl) = self.lrecl_fixed {
            schema_obj.insert("lrecl_fixed".to_string(), Value::Number(lrecl.into()));
        }

        if let Some(ref tail_odo) = self.tail_odo {
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

        // Convert to canonical JSON string
        serde_json::to_string(&Value::Object(schema_obj)).unwrap_or_default()
    }

    /// Convert field to canonical JSON for fingerprinting
    fn field_to_canonical_json(field: &Field) -> Value {
        use serde_json::{Map, Value};

        let mut field_obj = Map::new();

        // Add fields in canonical order
        field_obj.insert("path".to_string(), Value::String(field.path.clone()));
        field_obj.insert("name".to_string(), Value::String(field.name.clone()));
        field_obj.insert("level".to_string(), Value::Number(field.level.into()));

        // Add field kind
        let kind_str = match &field.kind {
            FieldKind::Alphanum { len } => format!("Alphanum({len})"),
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
            } => {
                format!("ZonedDecimal({digits},{scale},{signed})")
            }
            FieldKind::BinaryInt { bits, signed } => {
                format!("BinaryInt({bits},{signed})")
            }
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                format!("PackedDecimal({digits},{scale},{signed})")
            }
            FieldKind::Group => "Group".to_string(),
            FieldKind::Condition { values } => format!("Condition({values:?})"),
            FieldKind::Renames {
                from_field,
                thru_field,
            } => format!("Renames({from_field},{thru_field})"),
            FieldKind::EditedNumeric {
                pic_string,
                width,
                scale,
                signed,
            } => {
                format!("EditedNumeric({pic_string},{width},scale={scale},signed={signed})")
            }
        };
        field_obj.insert("kind".to_string(), Value::String(kind_str));

        // Add optional fields
        if let Some(ref redefines) = field.redefines_of {
            field_obj.insert("redefines_of".to_string(), Value::String(redefines.clone()));
        }

        if let Some(ref occurs) = field.occurs {
            let occurs_str = match occurs {
                Occurs::Fixed { count } => format!("Fixed({count})"),
                Occurs::ODO {
                    min,
                    max,
                    counter_path,
                } => {
                    format!("ODO({min},{max},{counter_path})")
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
                .map(Self::field_to_canonical_json)
                .collect();
            field_obj.insert("children".to_string(), Value::Array(children_json));
        }

        Value::Object(field_obj)
    }

    /// Find a field by path
    #[must_use]
    pub fn find_field(&self, path: &str) -> Option<&Field> {
        Self::find_field_recursive(&self.fields, path)
    }

    fn find_field_recursive<'a>(fields: &'a [Field], path: &str) -> Option<&'a Field> {
        for field in fields {
            if field.path == path {
                return Some(field);
            }
            if let Some(found) = Self::find_field_recursive(&field.children, path) {
                return Some(found);
            }
        }
        None
    }

    /// Find a field by path or RENAMES alias name
    ///
    /// This method first tries to find a field by its path using standard lookup.
    /// If not found, it searches for a level-66 RENAMES field whose name matches
    /// the query and returns that alias field.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # use copybook_core::Schema;
    /// let schema: Schema = // ... parsed schema with RENAMES
    /// # Schema::new();
    ///
    /// // Direct field lookup
    /// if let Some(field) = schema.find_field_or_alias("CUSTOMER-INFO") {
    ///     println!("Found field: {}", field.name);
    /// }
    ///
    /// // Alias lookup - finds level-66 field
    /// if let Some(alias) = schema.find_field_or_alias("CUSTOMER-DETAILS") {
    ///     if alias.level == 66 {
    ///         println!("Found RENAMES alias: {}", alias.name);
    ///     }
    /// }
    /// ```
    #[must_use]
    pub fn find_field_or_alias(&self, name_or_path: &str) -> Option<&Field> {
        // First try direct field lookup
        if let Some(field) = self.find_field(name_or_path) {
            return Some(field);
        }

        // If not found, check if it's a RENAMES alias (level-66)
        // We need to match by field name (last path component), not full path
        let query_name = name_or_path.rsplit('.').next().unwrap_or(name_or_path);
        Self::find_alias_field_recursive(&self.fields, query_name)
    }

    /// Recursively search for a level-66 RENAMES field by name
    fn find_alias_field_recursive<'a>(fields: &'a [Field], alias_name: &str) -> Option<&'a Field> {
        for field in fields {
            // Check if this is a level-66 field with matching name
            if field.level == 66 && field.name.eq_ignore_ascii_case(alias_name) {
                return Some(field);
            }
            // Recurse into children
            if let Some(found) = Self::find_alias_field_recursive(&field.children, alias_name) {
                return Some(found);
            }
        }
        None
    }

    /// Resolve a RENAMES alias to its first target field
    ///
    /// If the query matches a level-66 RENAMES alias, this method returns the
    /// first storage-bearing field covered by that alias (from resolved_renames.members).
    /// Otherwise, it performs standard field lookup.
    ///
    /// This is useful for codec integration where you want to decode/encode data
    /// using an alias name but need the actual storage field.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # use copybook_core::Schema;
    /// let schema: Schema = // ... parsed schema with RENAMES
    /// # Schema::new();
    ///
    /// // Resolve alias to target field
    /// if let Some(target) = schema.resolve_alias_to_target("CUSTOMER-DETAILS") {
    ///     // target will be CUSTOMER-INFO (or its first member)
    ///     println!("Alias resolves to: {}", target.name);
    /// }
    /// ```
    #[must_use]
    pub fn resolve_alias_to_target(&self, name_or_path: &str) -> Option<&Field> {
        // First try to find it as an alias
        if let Some(alias_field) = self.find_field_or_alias(name_or_path) {
            // If it's a level-66 with resolved_renames, return the first member
            if alias_field.level == 66
                && let Some(ref resolved) = alias_field.resolved_renames
                && let Some(first_member_path) = resolved.members.first()
            {
                return self.find_field(first_member_path);
            }
            // Otherwise return the field itself
            return Some(alias_field);
        }
        None
    }

    /// Find all fields that redefine the field at the given path
    #[must_use]
    pub fn find_redefining_fields<'a>(&'a self, target_path: &str) -> Vec<&'a Field> {
        fn collect<'a>(fields: &'a [Field], target_path: &str, acc: &mut Vec<&'a Field>) {
            for f in fields {
                if let Some(ref redef) = f.redefines_of
                    && redef == target_path
                {
                    acc.push(f);
                }
                collect(&f.children, target_path, acc);
            }
        }

        let mut result = Vec::new();
        collect(&self.fields, target_path, &mut result);
        result
    }

    /// Get all fields in a flat list (pre-order traversal)
    #[must_use]
    pub fn all_fields(&self) -> Vec<&Field> {
        let mut result = Vec::new();
        Self::collect_fields_recursive(&self.fields, &mut result);
        result
    }

    fn collect_fields_recursive<'a>(fields: &'a [Field], result: &mut Vec<&'a Field>) {
        for field in fields {
            result.push(field);
            Self::collect_fields_recursive(&field.children, result);
        }
    }
}

impl Field {
    /// Create a new field with level and name
    #[must_use]
    pub fn new(level: u8, name: String) -> Self {
        Self {
            path: name.clone(),
            name,
            level,
            kind: FieldKind::Group, // Default to group, will be updated by parser
            offset: 0,
            len: 0,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: Vec::new(),
        }
    }

    /// Create a new field with all parameters
    #[must_use]
    pub fn with_kind(level: u8, name: String, kind: FieldKind) -> Self {
        Self {
            path: name.clone(),
            name,
            level,
            kind,
            offset: 0,
            len: 0,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: Vec::new(),
        }
    }

    /// Check if this is a group field
    #[must_use]
    pub fn is_group(&self) -> bool {
        matches!(self.kind, FieldKind::Group)
    }

    /// Check if this is a scalar (leaf) field
    #[must_use]
    pub fn is_scalar(&self) -> bool {
        !self.is_group()
    }

    /// Get the effective length including any arrays
    #[must_use]
    pub fn effective_length(&self) -> u32 {
        match &self.occurs {
            Some(Occurs::Fixed { count }) => self.len * count,
            Some(Occurs::ODO { max, .. }) => self.len * max,
            None => self.len,
        }
    }
}

impl Default for Schema {
    fn default() -> Self {
        Self::new()
    }
}
