//! Schema types for COBOL copybook structures
//!
//! This module defines the core data structures that represent a parsed
//! COBOL copybook schema, including fields, types, and layout information.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
    /// Child fields (for groups)
    pub children: Vec<Field>,
}

/// Field type and characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FieldKind {
    /// Alphanumeric field (PIC X)
    Alphanum { 
        /// Field length in characters
        len: u32 
    },
    /// Zoned decimal field (PIC 9, display)
    ZonedDecimal { 
        /// Total number of digits
        digits: u16, 
        /// Decimal places (can be negative for scaling)
        scale: i16, 
        /// Whether field is signed
        signed: bool 
    },
    /// Binary integer field (COMP/BINARY)
    BinaryInt { 
        /// Number of bits (16, 32, 64)
        bits: u16, 
        /// Whether field is signed
        signed: bool 
    },
    /// Packed decimal field (COMP-3)
    PackedDecimal { 
        /// Total number of digits
        digits: u16, 
        /// Decimal places (can be negative for scaling)
        scale: i16, 
        /// Whether field is signed
        signed: bool 
    },
    /// Group field (contains other fields)
    Group,
}

/// Array occurrence information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Occurs {
    /// Fixed-size array
    Fixed { 
        /// Number of elements
        count: u32 
    },
    /// Variable-size array (OCCURS DEPENDING ON)
    ODO { 
        /// Minimum number of elements
        min: u32, 
        /// Maximum number of elements
        max: u32, 
        /// Path to counter field
        counter_path: String 
    },
}

impl Schema {
    /// Create a new empty schema
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            lrecl_fixed: None,
            tail_odo: None,
            fingerprint: String::new(),
        }
    }

    /// Calculate the schema fingerprint
    pub fn calculate_fingerprint(&mut self) {
        // This will be implemented with SHA-256 over canonical JSON
        // For now, use a placeholder
        self.fingerprint = "placeholder".to_string();
    }

    /// Find a field by path
    pub fn find_field(&self, path: &str) -> Option<&Field> {
        self.find_field_recursive(&self.fields, path)
    }

    fn find_field_recursive(&self, fields: &[Field], path: &str) -> Option<&Field> {
        for field in fields {
            if field.path == path {
                return Some(field);
            }
            if let Some(found) = self.find_field_recursive(&field.children, path) {
                return Some(found);
            }
        }
        None
    }

    /// Get all fields in a flat list (pre-order traversal)
    pub fn all_fields(&self) -> Vec<&Field> {
        let mut result = Vec::new();
        self.collect_fields_recursive(&self.fields, &mut result);
        result
    }

    fn collect_fields_recursive<'a>(&self, fields: &'a [Field], result: &mut Vec<&'a Field>) {
        for field in fields {
            result.push(field);
            self.collect_fields_recursive(&field.children, result);
        }
    }
}

impl Field {
    /// Create a new field
    pub fn new(name: String, level: u8, kind: FieldKind) -> Self {
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
            children: Vec::new(),
        }
    }

    /// Check if this is a group field
    pub fn is_group(&self) -> bool {
        matches!(self.kind, FieldKind::Group)
    }

    /// Check if this is a scalar (leaf) field
    pub fn is_scalar(&self) -> bool {
        !self.is_group()
    }

    /// Get the effective length including any arrays
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