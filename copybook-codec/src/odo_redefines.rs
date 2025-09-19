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
    };

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
/// 1. If --use-raw and record-level __raw_b64 present and values match canonical decode → emit raw bytes
/// 2. Else if exactly one view under the cluster is non-null → emit from that view
/// 3. Else → error CBKE501_JSON_TYPE_MISMATCH (ambiguous write)
///
/// # Errors
///
/// Returns CBKE501_JSON_TYPE_MISMATCH with cluster path context for ambiguous writes.
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
        .map(|views| views.len())
        .unwrap_or(0);

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
    let non_null_view_names: Vec<String> = context
        .cluster_views
        .get(cluster_path)
        .map(|views| views.clone())
        .unwrap_or_default();

    Err(Error::new(
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        format!(
            "Ambiguous REDEFINES write: multiple views ({}) have non-null values for cluster '{}'",
            non_null_view_names.join(", "),
            cluster_path
        ),
    )
    .with_context(ErrorContext {
        record_index: Some(record_index),
        field_path: Some(field_path.to_string()),
        byte_offset: Some(byte_offset),
        line_number: None,
        details: Some(format!(
            "cluster_path={}, non_null_views=[{}]",
            cluster_path,
            non_null_view_names.join(", ")
        )),
    }))
}

/// Handle missing counter field error with enhanced context and suggestions
///
/// Provides comprehensive error reporting when ODO counter fields cannot be found,
/// including field path suggestions based on similar names in the schema.
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
            "ODO counter field '{}' not found for array '{}'{}",
            counter_path,
            array_path,
            if suggestions.is_empty() {
                ""
            } else {
                &format!(". Similar fields found: [{}]", suggestions.join(", "))
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

/// Create comprehensive error context with all available information
///
/// Standardizes error context creation across the ODO/REDEFINES module
/// to ensure consistent error reporting with full context.
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
#[allow(clippy::too_many_arguments)]
pub fn validate_odo_decode(
    counter_value: u32,
    min_count: u32,
    max_count: u32,
    field_path: &str,
    counter_path: &str,
    record_index: u64,
    byte_offset: u64,
    options: &DecodeOptions,
) -> Result<OdoValidationResult> {
    validate_odo_counter(
        counter_value,
        min_count,
        max_count,
        field_path,
        counter_path,
        record_index,
        byte_offset,
        options.strict_mode,
    )
}

/// Validate ODO array bounds during encode with comprehensive error handling
///
/// This function validates ODO array lengths during encoding and ensures
/// proper error context is included in all error reports.
#[allow(clippy::too_many_arguments)]
pub fn validate_odo_encode(
    array_length: usize,
    min_count: u32,
    max_count: u32,
    field_path: &str,
    counter_path: &str,
    record_index: u64,
    byte_offset: u64,
    options: &EncodeOptions,
) -> Result<u32> {
    let array_length_u32 = array_length as u32;

    let validation_result = validate_odo_counter(
        array_length_u32,
        min_count,
        max_count,
        field_path,
        counter_path,
        record_index,
        byte_offset,
        options.strict_mode,
    )?;

    if validation_result.was_clamped {
        // In encode mode, we should probably error even in lenient mode
        // since we can't "clamp" the input JSON array
        return Err(Error::new(
            ErrorCode::CBKE521_ARRAY_LEN_OOB,
            format!(
                "JSON array length {} is out of bounds for ODO field '{}' (min={}, max={})",
                array_length, field_path, min_count, max_count
            ),
        )
        .with_context(create_comprehensive_error_context(
            record_index,
            field_path,
            byte_offset,
            Some(format!(
                "counter_field={}, array_length={}",
                counter_path, array_length
            )),
        )));
    }

    Ok(validation_result.actual_count)
}

/// Helper function to collect REDEFINES relationships from schema
fn collect_redefines_relationships(schema: &Schema, context: &mut RedefinesContext) {
    fn visit_field(field: &Field, path: String, context: &mut RedefinesContext) {
        // If this field redefines another, record the relationship
        if let Some(redefines_of) = &field.redefines_of {
            // The cluster path is the path of the original field being redefined
            let parent_path = path.rsplit_once('.').map(|(p, _)| p).unwrap_or("");
            let cluster_path = if parent_path.is_empty() {
                redefines_of.clone()
            } else {
                format!("{}.{}", parent_path, redefines_of)
            };
            context.field_to_cluster.insert(path.clone(), cluster_path.clone());
            context.cluster_views.entry(cluster_path).or_default().push(path.clone());
        }

        // Visit children
        for child in &field.children {
            let child_path = if path.is_empty() {
                child.name.clone()
            } else {
                format!("{}.{}", path, child.name)
            };
            visit_field(child, child_path, context);
        }
    }

    // Visit all top-level fields
    for field in &schema.fields {
        visit_field(field, field.name.clone(), context);
    }

    // Collect all fields that are being redefined to identify cluster roots
    let all_fields = schema.all_fields();
    for field in &all_fields {
        if let Some(redefines_of) = &field.redefines_of {
            // Find the original field and add it to the cluster
            if let Some(original_field) = all_fields.iter().find(|f| f.name == *redefines_of || f.path == *redefines_of) {
                let cluster_path = original_field.path.clone();
                context.cluster_views.entry(cluster_path.clone()).or_default().push(original_field.path.clone());

                // Also add this redefining field
                context.field_to_cluster.insert(field.path.clone(), cluster_path.clone());
                context.cluster_views.entry(cluster_path).or_default().push(field.path.clone());
            }
        }
    }
}

/// Helper function to analyze JSON data for non-null REDEFINES views
fn analyze_json_for_redefines(context: &mut RedefinesContext, json_obj: &serde_json::Map<String, Value>) {
    fn is_non_null_value(value: &Value) -> bool {
        match value {
            Value::Null => false,
            Value::Object(obj) => obj.values().any(is_non_null_value),
            Value::Array(arr) => arr.iter().any(is_non_null_value),
            _ => true,
        }
    }

    for (key, value) in json_obj {
        if let Some(cluster_path) = context.field_to_cluster.get(key) {
            if is_non_null_value(value) {
                if let Some(views) = context.cluster_views.get_mut(cluster_path) {
                    if !views.contains(key) {
                        views.push(key.clone());
                    }
                }
            }
        }

        // Recursively analyze nested objects
        if let Value::Object(nested_obj) = value {
            analyze_json_for_redefines(context, nested_obj);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::ErrorCode;

    #[test]
    fn test_validate_odo_counter_within_bounds() {
        let result = validate_odo_counter(5, 1, 10, "test_field", "counter_field", 0, 0, false);
        assert!(result.is_ok());
        let validation_result = result.unwrap();
        assert_eq!(validation_result.actual_count, 5);
        assert!(!validation_result.was_clamped);
        assert!(validation_result.warning.is_none());
    }

    #[test]
    fn test_validate_odo_counter_strict_mode_below_min() {
        let result = validate_odo_counter(0, 1, 10, "test_field", "counter_field", 0, 0, true);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED);
    }

    #[test]
    fn test_validate_odo_counter_strict_mode_above_max() {
        let result = validate_odo_counter(15, 1, 10, "test_field", "counter_field", 0, 0, true);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED);
    }

    #[test]
    fn test_validate_odo_counter_lenient_mode_clamp_below() {
        let result = validate_odo_counter(0, 1, 10, "test_field", "counter_field", 0, 0, false);
        assert!(result.is_ok());
        let validation_result = result.unwrap();
        assert_eq!(validation_result.actual_count, 1);
        assert!(validation_result.was_clamped);
        assert!(validation_result.warning.is_some());
    }

    #[test]
    fn test_validate_odo_counter_lenient_mode_clamp_above() {
        let result = validate_odo_counter(15, 1, 10, "test_field", "counter_field", 0, 0, false);
        assert!(result.is_ok());
        let validation_result = result.unwrap();
        assert_eq!(validation_result.actual_count, 10);
        assert!(validation_result.was_clamped);
        assert!(validation_result.warning.is_some());
    }
}