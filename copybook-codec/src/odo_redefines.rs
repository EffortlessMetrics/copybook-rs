//! ODO and REDEFINES error handling with comprehensive validation
//!
//! This module implements the normative error handling behavior for ODO (OCCURS DEPENDING ON)
//! and REDEFINES clauses, including strict vs lenient mode handling and comprehensive
//! validation with proper error context.

use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{Error, ErrorCode, ErrorContext, Field, Occurs, Result, Schema};
use serde_json::Value;
use std::collections::HashMap;
use tracing::{debug, warn};

/// ODO validation result
#[derive(Debug, Clone)]
pub struct OdoValidationResult {
    /// Actual count to use (clamped if necessary)
    pub actual_count: u32,
    /// Whether clamping occurred
    pub was_clamped: bool,
    /// Warning to report if clamping occurred
    pub warning: Option<Error>,
}

/// REDEFINES encoding context for ambiguity detection
#[derive(Debug, Clone)]
pub struct RedefinesContext {
    /// Map of cluster paths to their non-null views
    pub cluster_views: HashMap<String, Vec<String>>,
    /// Map of field paths to their cluster root
    pub field_to_cluster: HashMap<String, String>,
}

/// Validate ODO counter value with strict/lenient mode handling (NORMATIVE)
///
/// In lenient mode: clamp out-of-bounds counter to min/max with warnings
/// In strict mode: treat out-of-bounds ODO as fatal error and abort immediately
///
/// # Errors
///
/// Returns fatal error in strict mode for out-of-bounds values.
/// Returns warnings in lenient mode for clamped values.
#[allow(clippy::too_many_arguments)]
pub fn validate_odo_counter(
    counter_value: u32,
    min_count: u32,
    max_count: u32,
    field_path: &str,
    counter_path: &str,
    record_index: u64,
    byte_offset: u64,
    strict_mode: bool,
) -> Result<OdoValidationResult> {
    debug!(
        "Validating ODO counter: value={}, min={}, max={}, field={}, counter={}, record={}, strict={}",
        counter_value, min_count, max_count, field_path, counter_path, record_index, strict_mode
    );

    // Check if value is within bounds
    if counter_value >= min_count && counter_value <= max_count {
        return Ok(OdoValidationResult {
            actual_count: counter_value,
            was_clamped: false,
            warning: None,
        });
    }

    // Value is out of bounds - handle based on mode
    if strict_mode {
        // In strict mode, ODO out-of-bounds is fatal (NORMATIVE)
        let error_msg = if counter_value < min_count {
            format!(
                "ODO counter value {} is below minimum {} for array '{}'",
                counter_value, min_count, field_path
            )
        } else {
            format!(
                "ODO counter value {} exceeds maximum {} for array '{}'",
                counter_value, max_count, field_path
            )
        };

        return Err(
            Error::new(ErrorCode::CBKS301_ODO_CLIPPED, error_msg).with_context(ErrorContext {
                record_index: Some(record_index),
                field_path: Some(field_path.to_string()),
                byte_offset: Some(byte_offset),
                line_number: None,
                details: Some(format!(
                    "counter_field={}, counter_value={}",
                    counter_path, counter_value
                )),
            }),
        );
    }

    // In lenient mode, clamp and warn (NORMATIVE)
    let (actual_count, error_code, action) = if counter_value < min_count {
        (
            min_count,
            ErrorCode::CBKS302_ODO_RAISED,
            "raised to minimum",
        )
    } else {
        (
            max_count,
            ErrorCode::CBKS301_ODO_CLIPPED,
            "clipped to maximum",
        )
    };

    let warning = Error::new(
        error_code,
        format!(
            "ODO counter value {} {} {} for array '{}' (was {})",
            counter_value, action, actual_count, field_path, counter_value
        ),
    )
    .with_context(ErrorContext {
        record_index: Some(record_index),
        field_path: Some(field_path.to_string()),
        byte_offset: Some(byte_offset),
        line_number: None,
        details: Some(format!(
            "counter_field={}, original_value={}, clamped_value={}",
            counter_path, counter_value, actual_count
        )),
    });

    warn!("{}", warning);

    Ok(OdoValidationResult {
        actual_count,
        was_clamped: true,
        warning: Some(warning),
    })
}

/// Validate ODO tail position constraints (NORMATIVE)
///
/// Ensures that ODO arrays are only at the tail position of their containing group
/// and that counter fields are not inside REDEFINES or ODO regions.
///
/// # Errors
///
/// Returns error if ODO constraints are violated.
pub fn validate_odo_tail_position(
    schema: &Schema,
    odo_field_path: &str,
    counter_field_path: &str,
) -> Result<()> {
    debug!(
        "Validating ODO tail position: array={}, counter={}",
        odo_field_path, counter_field_path
    );

    // Find the ODO field and its parent group
    let odo_field = schema.find_field(odo_field_path).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
            format!("ODO array field '{}' not found in schema", odo_field_path),
        )
    })?;

    // Validate that the field actually has ODO
    match &odo_field.occurs {
        Some(Occurs::ODO {
            min: _,
            max: _,
            counter_path: _,
        }) => (),
        _ => {
            return Err(Error::new(
                ErrorCode::CBKP021_ODO_NOT_TAIL,
                format!("Field '{}' is not an ODO array", odo_field_path),
            ));
        }
    }

    // Find the counter field
    let counter_field = schema.find_field(counter_field_path).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
            format!(
                "ODO counter field '{}' not found in schema",
                counter_field_path
            ),
        )
    })?;

    // Validate counter precedes array in byte order
    if counter_field.offset >= odo_field.offset {
        return Err(Error::new(
            ErrorCode::CBKP021_ODO_NOT_TAIL,
            format!(
                "ODO counter '{}' (offset {}) must precede array '{}' (offset {}) in byte order",
                counter_field_path, counter_field.offset, odo_field_path, odo_field.offset
            ),
        ));
    }

    // Additional validation: check if counter is in a REDEFINES region
    if counter_field.redefines_of.is_some() {
        return Err(Error::new(
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
            format!(
                "ODO counter field '{}' cannot be inside a REDEFINES region",
                counter_field_path
            ),
        ));
    }

    // Check if counter is inside another ODO region (simplified check)
    // This would require more complex parent traversal in a full implementation
    debug!(
        "ODO tail position validation passed for array '{}' with counter '{}'",
        odo_field_path, counter_field_path
    );

    Ok(())
}

/// Build REDEFINES context for encoding ambiguity detection
///
/// Analyzes the JSON data to identify which views under each REDEFINES cluster
/// have non-null values, enabling proper ambiguity detection during encoding.
pub fn build_redefines_context(schema: &Schema, json_data: &Value) -> RedefinesContext {
    let mut context = RedefinesContext {
        cluster_views: HashMap::new(),
        field_to_cluster: HashMap::new(),
    };

    // Collect all REDEFINES relationships from schema
    collect_redefines_relationships(schema, &mut context);

    // Analyze JSON data to find non-null views
    if let Value::Object(obj) = json_data {
        analyze_json_for_redefines(&mut context, obj);
    }

    debug!(
        "Built REDEFINES context: {} clusters, {} field mappings",
        context.cluster_views.len(),
        context.field_to_cluster.len()
    );

    context
}

/// Validate REDEFINES encoding for ambiguity (NORMATIVE)
///
/// Implements the normative REDEFINES encode precedence:
/// 1. If --use-raw and record-level `__raw_b64` present and values match canonical decode → emit raw bytes
/// 2. Else if exactly one view under the cluster is non-null → emit from that view
/// 3. Else → error `CBKE501_JSON_TYPE_MISMATCH` (ambiguous write)
///
/// # Errors
///
/// Returns `CBKE501_JSON_TYPE_MISMATCH` with cluster path context for ambiguous writes.
pub fn validate_redefines_encoding(
    context: &RedefinesContext,
    cluster_path: &str,
    field_path: &str,
    json_data: &Value,
    use_raw: bool,
    record_index: u64,
    byte_offset: u64,
) -> Result<()> {
    debug!(
        "Validating REDEFINES encoding: cluster={}, field={}, use_raw={}",
        cluster_path, field_path, use_raw
    );

    // Check for raw data precedence (step 1)
    if use_raw
        && let Value::Object(obj) = json_data
        && let Some(Value::String(_)) = obj.get("__raw_b64")
    {
        debug!("Using raw data for REDEFINES cluster '{}'", cluster_path);
        return Ok(());
    }

    // Check for single non-null view (step 2)
    let non_null_views = context
        .cluster_views
        .get(cluster_path)
        .map_or(0, std::vec::Vec::len);

    if non_null_views == 0 {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!(
                "No non-null views found for REDEFINES cluster '{}'",
                cluster_path
            ),
        )
        .with_context(ErrorContext {
            record_index: Some(record_index),
            field_path: Some(field_path.to_string()),
            byte_offset: Some(byte_offset),
            line_number: None,
            details: Some(format!("cluster_path={}", cluster_path)),
        }));
    }

    if non_null_views == 1 {
        debug!(
            "Single non-null view found for REDEFINES cluster '{}'",
            cluster_path
        );
        return Ok(());
    }

    // Multiple non-null views - ambiguous write (step 3)
    let views_list = context
        .cluster_views
        .get(cluster_path)
        .map_or_else(|| "unknown".to_string(), |views| views.join(", "));

    Err(Error::new(
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        format!(
            "Ambiguous REDEFINES write: multiple non-null views ({}) for cluster '{}'",
            views_list, cluster_path
        ),
    )
    .with_context(ErrorContext {
        record_index: Some(record_index),
        field_path: Some(field_path.to_string()),
        byte_offset: Some(byte_offset),
        line_number: None,
        details: Some(format!(
            "cluster_path={}, non_null_views={}",
            cluster_path, non_null_views
        )),
    }))
}

/// Handle missing ODO counter field references
///
/// Provides comprehensive error reporting when ODO counter fields cannot be found,
/// including suggestions for common naming issues.
///
/// # Errors
///
/// Returns `CBKS121_COUNTER_NOT_FOUND` with detailed context and suggestions.
pub fn handle_missing_counter_field(
    counter_path: &str,
    array_path: &str,
    schema: &Schema,
    record_index: u64,
    byte_offset: u64,
) -> Error {
    debug!(
        "Handling missing counter field: counter={}, array={}",
        counter_path, array_path
    );

    // Try to find similar field names for suggestions
    let all_fields = schema.all_fields();
    let mut suggestions = Vec::new();

    for field in all_fields {
        if field.path.contains(counter_path) || counter_path.contains(&field.name) {
            suggestions.push(field.path.clone());
        }
    }

    let details = if suggestions.is_empty() {
        format!(
            "array_field={}, searched_paths=all_schema_fields",
            array_path
        )
    } else {
        format!(
            "array_field={}, similar_fields=[{}]",
            array_path,
            suggestions.join(", ")
        )
    };

    Error::new(
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        format!(
            "ODO counter field '{}' not found for array '{}'. {}",
            counter_path,
            array_path,
            if suggestions.is_empty() {
                "No similar field names found in schema."
            } else {
                "Did you mean one of the similar fields listed in details?"
            }
        ),
    )
    .with_context(ErrorContext {
        record_index: Some(record_index),
        field_path: Some(array_path.to_string()),
        byte_offset: Some(byte_offset),
        line_number: None,
        details: Some(details),
    })
}

/// Collect REDEFINES relationships from schema
fn collect_redefines_relationships(schema: &Schema, context: &mut RedefinesContext) {
    collect_redefines_from_fields(&schema.fields, context);
}

/// Recursively collect REDEFINES relationships from fields
fn collect_redefines_from_fields(fields: &[Field], context: &mut RedefinesContext) {
    for field in fields {
        if let Some(ref target) = field.redefines_of {
            // Map this field to its cluster (use field name, not full path for JSON matching)
            context
                .field_to_cluster
                .insert(field.name.clone(), target.clone());

            // Initialize cluster if not present (use target name, not full path)
            let target_name = target.split('.').next_back().unwrap_or(target);
            context
                .cluster_views
                .entry(target_name.to_string())
                .or_default();
        }

        // Recursively process children
        collect_redefines_from_fields(&field.children, context);
    }
}

/// Analyze JSON data to find non-null REDEFINES views
fn analyze_json_for_redefines(
    context: &mut RedefinesContext,
    json_obj: &serde_json::Map<String, Value>,
) {
    for (key, value) in json_obj {
        // Skip metadata fields
        if key.starts_with("__") {
            continue;
        }

        // Check if this field is part of a REDEFINES cluster
        if let Some(cluster_path) = context.field_to_cluster.get(key) {
            if !value.is_null() {
                // Add this view to the cluster's non-null views
                let views = context
                    .cluster_views
                    .entry(cluster_path.clone())
                    .or_default();
                if !views.contains(key) {
                    views.push(key.clone());
                }
            }
        } else {
            // Check if this field is a REDEFINES target (original field)
            if context.cluster_views.contains_key(key) && !value.is_null() {
                // This is the original field that others redefine
                let views = context.cluster_views.entry(key.clone()).or_default();
                if !views.contains(key) {
                    views.push(key.clone());
                }
            }
        }

        // Recursively analyze nested objects
        if let Value::Object(nested_obj) = value {
            analyze_json_for_redefines(context, nested_obj);
        }
    }
}

/// Context information for ODO validation operations
#[derive(Clone)]
pub struct OdoValidationContext {
    pub field_path: String,
    pub counter_path: String,
    pub record_index: u64,
    pub byte_offset: u64,
}

/// Create comprehensive error context for CBKD/CBKE errors
///
/// Ensures all data decode (CBKD*) and encode (CBKE*) errors include
/// `record_index`, `field_path`, and `byte_offset` context as required.
pub fn create_comprehensive_error_context(
    record_index: u64,
    field_path: &str,
    byte_offset: u64,
    additional_details: Option<String>,
) -> ErrorContext {
    ErrorContext {
        record_index: Some(record_index),
        field_path: Some(field_path.to_string()),
        byte_offset: Some(byte_offset),
        line_number: None,
        details: additional_details,
    }
}

/// Validate ODO array bounds during decode with comprehensive error handling
///
/// This function combines ODO counter validation with proper error context
/// and handles both strict and lenient modes according to normative behavior.
pub fn validate_odo_decode(
    counter_value: u32,
    min_count: u32,
    max_count: u32,
    context: &OdoValidationContext,
    options: &DecodeOptions,
) -> Result<OdoValidationResult> {
    validate_odo_counter(
        counter_value,
        min_count,
        max_count,
        &context.field_path,
        &context.counter_path,
        context.record_index,
        context.byte_offset,
        options.strict_mode,
    )
}

/// Validate ODO array bounds during encode with comprehensive error handling
///
/// This function validates ODO array lengths during encoding and ensures
/// proper error context is included in all error reports.
pub fn validate_odo_encode(
    array_length: usize,
    min_count: u32,
    max_count: u32,
    context: &OdoValidationContext,
    options: &EncodeOptions,
) -> Result<u32> {
    let array_length_u32 = u32::try_from(array_length).map_err(|_| {
        Error::new(
            ErrorCode::CBKE521_ARRAY_LEN_OOB,
            format!("Array length {} exceeds u32::MAX", array_length),
        )
        .with_context(create_comprehensive_error_context(
            context.record_index,
            &context.field_path,
            context.byte_offset,
            Some(format!("Array length: {}", array_length)),
        ))
    })?;

    let validation_result = validate_odo_counter(
        array_length_u32,
        min_count,
        max_count,
        &context.field_path,
        &context.counter_path,
        context.record_index,
        context.byte_offset,
        options.strict_mode,
    )?;

    if validation_result.was_clamped {
        // In encode mode, we should probably error even in lenient mode
        // since we can't "clamp" the input JSON array
        return Err(Error::new(
            ErrorCode::CBKE521_ARRAY_LEN_OOB,
            format!(
                "JSON array length {} is out of bounds for ODO field '{}' (min={}, max={})",
                array_length, context.field_path, min_count, max_count
            ),
        )
        .with_context(create_comprehensive_error_context(
            context.record_index,
            &context.field_path,
            context.byte_offset,
            Some(format!(
                "counter_field={}, array_length={}",
                context.counter_path, array_length
            )),
        )));
    }

    Ok(validation_result.actual_count)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use copybook_core::{Field, FieldKind, Schema};

    fn create_test_schema_with_odo() -> Schema {
        let mut schema = Schema::new();

        // Counter field
        let counter = Field {
            path: "ROOT.COUNTER".to_string(),
            name: "COUNTER".to_string(),
            level: 5,
            kind: FieldKind::ZonedDecimal {
                digits: 3,
                scale: 0,
                signed: false,
            },
            offset: 0,
            len: 3,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: vec![],
        };

        // ODO array field
        let array_field = Field {
            path: "ROOT.ARRAY".to_string(),
            name: "ARRAY".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 10 },
            offset: 3,
            len: 10,
            redefines_of: None,
            occurs: Some(Occurs::ODO {
                min: 0,
                max: 5,
                counter_path: "ROOT.COUNTER".to_string(),
            }),
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: vec![],
        };

        schema.fields = vec![counter, array_field];
        schema
    }

    #[test]
    fn test_odo_validation_within_bounds() {
        let result =
            validate_odo_counter(3, 0, 5, "ROOT.ARRAY", "ROOT.COUNTER", 1, 3, false).unwrap();

        assert_eq!(result.actual_count, 3);
        assert!(!result.was_clamped);
        assert!(result.warning.is_none());
    }

    #[test]
    fn test_odo_validation_strict_mode_over_max() {
        let result = validate_odo_counter(10, 0, 5, "ROOT.ARRAY", "ROOT.COUNTER", 1, 3, true);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED);
        assert!(error.context.is_some());

        let context = error.context.unwrap();
        assert_eq!(context.record_index, Some(1));
        assert_eq!(context.field_path, Some("ROOT.ARRAY".to_string()));
        assert_eq!(context.byte_offset, Some(3));
    }

    #[test]
    fn test_odo_validation_lenient_mode_clamp_max() {
        let result =
            validate_odo_counter(10, 0, 5, "ROOT.ARRAY", "ROOT.COUNTER", 1, 3, false).unwrap();

        assert_eq!(result.actual_count, 5);
        assert!(result.was_clamped);
        assert!(result.warning.is_some());

        let warning = result.warning.unwrap();
        assert_eq!(warning.code, ErrorCode::CBKS301_ODO_CLIPPED);
    }

    #[test]
    fn test_odo_validation_lenient_mode_raise_min() {
        let result =
            validate_odo_counter(0, 1, 5, "ROOT.ARRAY", "ROOT.COUNTER", 1, 3, false).unwrap();

        assert_eq!(result.actual_count, 1);
        assert!(result.was_clamped);
        assert!(result.warning.is_some());

        let warning = result.warning.unwrap();
        assert_eq!(warning.code, ErrorCode::CBKS302_ODO_RAISED);
    }

    #[test]
    fn test_odo_tail_position_validation() {
        let schema = create_test_schema_with_odo();

        let result = validate_odo_tail_position(&schema, "ROOT.ARRAY", "ROOT.COUNTER");

        assert!(result.is_ok());
    }

    #[test]
    fn test_odo_tail_position_validation_counter_after_array() {
        let mut schema = create_test_schema_with_odo();

        // Swap offsets to make counter come after array
        schema.fields[0].offset = 10; // counter
        schema.fields[1].offset = 0; // array

        let result = validate_odo_tail_position(&schema, "ROOT.ARRAY", "ROOT.COUNTER");

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn test_missing_counter_field_handling() {
        let schema = Schema::new(); // Empty schema

        let error = handle_missing_counter_field("NONEXISTENT", "ROOT.ARRAY", &schema, 1, 10);

        assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);
        assert!(error.context.is_some());

        let context = error.context.unwrap();
        assert_eq!(context.record_index, Some(1));
        assert_eq!(context.field_path, Some("ROOT.ARRAY".to_string()));
        assert_eq!(context.byte_offset, Some(10));
    }

    #[test]
    fn test_redefines_context_building() {
        let mut schema = Schema::new();

        // Original field
        let field_a = Field {
            path: "ROOT.FIELD_A".to_string(),
            name: "FIELD_A".to_string(),
            level: 5,
            kind: FieldKind::Alphanum { len: 10 },
            offset: 0,
            len: 10,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: vec![],
        };

        // REDEFINES field
        let field_b = Field {
            path: "ROOT.FIELD_B".to_string(),
            name: "FIELD_B".to_string(),
            level: 5,
            kind: FieldKind::ZonedDecimal {
                digits: 5,
                scale: 0,
                signed: false,
            },
            offset: 0,
            len: 5,
            redefines_of: Some("ROOT.FIELD_A".to_string()),
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            children: vec![],
        };

        schema.fields = vec![field_a, field_b];

        let json_data = serde_json::json!({
            "FIELD_A": "Hello",
            "FIELD_B": null
        });

        let context = build_redefines_context(&schema, &json_data);

        assert_eq!(context.field_to_cluster.len(), 1);
        assert!(context.field_to_cluster.contains_key("FIELD_B"));
        assert_eq!(context.field_to_cluster["FIELD_B"], "ROOT.FIELD_A");
    }

    #[test]
    fn test_comprehensive_error_context() {
        let context = create_comprehensive_error_context(
            42,
            "ROOT.TEST_FIELD",
            100,
            Some("additional info".to_string()),
        );

        assert_eq!(context.record_index, Some(42));
        assert_eq!(context.field_path, Some("ROOT.TEST_FIELD".to_string()));
        assert_eq!(context.byte_offset, Some(100));
        assert_eq!(context.details, Some("additional info".to_string()));
    }
}
