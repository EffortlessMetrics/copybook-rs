//! Layout resolution and field offset calculation
//!
//! This module handles the computation of field byte offsets,
//! alignment padding, and REDEFINES cluster sizing.

use crate::{Error, ErrorCode, Field, FieldKind, Occurs, Result, Schema, TailODO, error};
use std::collections::HashMap;
use tracing::debug;

/// Maximum theoretical record size (16 MiB by default)
const MAX_RECORD_SIZE: u64 = 16 * 1024 * 1024;

/// Layout resolution context
#[derive(Debug)]
struct LayoutContext {
    /// Current byte offset (using u64 for overflow protection)
    current_offset: u64,
    /// REDEFINES clusters: `target_path` -> (`cluster_start_offset`, `cluster_max_size`)
    redefines_clusters: HashMap<String, (u64, u64)>,
    /// Track ODO arrays for tail validation
    odo_arrays: Vec<OdoInfo>,
    /// Track field paths for validation
    field_paths: HashMap<String, u64>, // path -> offset
}

/// Information about ODO arrays for validation
#[derive(Debug, Clone)]
struct OdoInfo {
    /// Path to the ODO array field
    array_path: String,
    /// Path to the counter field
    counter_path: String,
    /// Byte offset of the array
    array_offset: u64,
    /// Maximum count for space allocation
    max_count: u32,
    /// Minimum count
    min_count: u32,
}

impl LayoutContext {
    fn new() -> Self {
        Self {
            current_offset: 0,
            redefines_clusters: HashMap::new(),
            odo_arrays: Vec::new(),
            field_paths: HashMap::new(),
        }
    }
}

/// Resolve field layouts and compute byte offsets
///
/// This function implements the complete layout resolution algorithm including:
/// - Field byte offset calculation with SYNCHRONIZED alignment
/// - REDEFINES cluster sizing (max of all variants)
/// - OCCURS fixed arrays and ODO space allocation
/// - Alignment padding insertion and tracking
/// - Overflow protection with u64 arithmetic
///
/// # Errors
/// Returns an error if field configuration is invalid or the record size exceeds limits.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn resolve_layout(schema: &mut Schema) -> Result<()> {
    let mut context = LayoutContext::new();

    // First pass: collect all REDEFINES relationships
    collect_redefines_info(&schema.fields, &mut context);

    // Second pass: resolve all field layouts and collect information
    for field in &mut schema.fields {
        resolve_field_layout(field, &mut context, None)?;
    }

    // Validate ODO constraints (counter existence, not tail position)
    validate_odo_constraints(&context)?;

    // Calculate fixed record length if all fields are fixed
    calculate_fixed_record_length(schema, &context)?;

    // Detect and set tail ODO information
    detect_tail_odo(schema, &context);

    // Resolve RENAMES (level-66) aliases
    resolve_renames_aliases(&mut schema.fields)?;

    // Check for record size overflow
    if context.current_offset > MAX_RECORD_SIZE {
        return Err(error!(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            "Theoretical maximum record size {} bytes exceeds limit of {} bytes",
            context.current_offset,
            MAX_RECORD_SIZE
        ));
    }

    Ok(())
}

/// Collect information about REDEFINES relationships
fn collect_redefines_info(fields: &[Field], context: &mut LayoutContext) {
    for field in fields {
        if let Some(target) = field.redefines_of.as_ref() {
            // Initialize cluster for the target if not already present
            if !context.redefines_clusters.contains_key(target) {
                context.redefines_clusters.insert(target.clone(), (0, 0));
            }
        }

        // Recursively process children
        collect_redefines_info(&field.children, context);
    }
}

/// Resolve layout for a single field and its children
fn resolve_field_layout(
    field: &mut Field,
    context: &mut LayoutContext,
    parent_path: Option<&str>,
) -> Result<u64> {
    // Build full field path
    let field_path = if let Some(parent) = parent_path {
        format!("{}.{}", parent, field.name)
    } else {
        field.name.clone()
    };
    field.path.clone_from(&field_path);

    // Handle REDEFINES fields
    if let Some(target) = field.redefines_of.clone() {
        return resolve_redefines_field(field, context, &target, &field_path);
    }

    // Calculate field alignment and base size
    let (alignment, base_size) =
        calculate_field_size_and_alignment(&field.kind, field.synchronized);

    // Before calculating offset, ensure current_offset accounts for any completed REDEFINES clusters
    for (cluster_start, cluster_size) in context.redefines_clusters.values() {
        let cluster_end = cluster_start + cluster_size;
        context.current_offset = context.current_offset.max(cluster_end);
    }

    // Apply alignment padding if needed
    let aligned_offset = apply_alignment(context.current_offset, alignment);
    let padding_bytes = aligned_offset - context.current_offset;

    // PERFORMANCE OPTIMIZATION: Common case optimization for small padding values
    if padding_bytes > 0 {
        // Most sync padding is small (≤16 bytes), so this is the fast path
        field.sync_padding = Some(crate::utils::safe_ops::safe_u64_to_u16(
            padding_bytes,
            "sync padding calculation",
        )?);
    }

    // PERFORMANCE OPTIMIZATION: Most field offsets fit comfortably in u32
    field.offset =
        crate::utils::safe_ops::safe_u64_to_u32(aligned_offset, "field offset calculation")?;
    context.current_offset = aligned_offset;

    // Record field path and offset
    context
        .field_paths
        .insert(field_path.clone(), aligned_offset);

    // Handle COBOL REDEFINES clause processing
    //
    // In COBOL, REDEFINES allows multiple fields to occupy the same memory location,
    // providing different interpretations of the same data. For example:
    //
    // ```cobol
    // 01 DATE-FIELD      PIC X(8).
    // 01 DATE-PARTS REDEFINES DATE-FIELD.
    //    05 YEAR         PIC 9(4).
    //    05 MONTH        PIC 9(2).
    //    05 DAY          PIC 9(2).
    // ```
    //
    // Both DATE-FIELD and DATE-PARTS occupy the same 8 bytes, but provide
    // different access patterns to the same underlying data.
    let cluster_key = field.name.clone();
    if let Some((cluster_start, current_max)) =
        context.redefines_clusters.get(&cluster_key).copied()
    {
        // This field is being redefined by other fields - update cluster metadata
        let new_cluster_start = if cluster_start == 0 {
            aligned_offset
        } else {
            cluster_start
        };
        let field_effective_size = match &field.occurs {
            Some(Occurs::Fixed { count }) => u64::from(base_size) * u64::from(*count),
            Some(Occurs::ODO { max, .. }) => u64::from(base_size) * u64::from(*max),
            None => u64::from(base_size),
        };
        context.redefines_clusters.insert(
            cluster_key,
            (new_cluster_start, current_max.max(field_effective_size)),
        );
    }

    // Calculate effective field size including arrays
    // PERFORMANCE OPTIMIZATION: Most fields are simple scalars (fast path)
    let effective_size = match &field.occurs {
        Some(Occurs::Fixed { count }) => {
            // Fixed array: multiply base size by count
            // PERFORMANCE OPTIMIZATION: Use widening multiplication for speed
            let base_u64 = u64::from(base_size);
            let count_u64 = u64::from(*count);

            // Fast path for small arrays (most common case)
            if count_u64 <= 1000 && base_u64 <= 1000 {
                // Safe to multiply directly - common case optimization
                base_u64 * count_u64
            } else {
                // Checked multiplication for large arrays
                base_u64.checked_mul(count_u64).ok_or_else(|| {
                    error!(
                        ErrorCode::CBKS141_RECORD_TOO_LARGE,
                        "Fixed array size overflow for field '{}'", field.name
                    )
                })?
            }
        }
        Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) => {
            // ODO array: use maximum count for space allocation
            let base_u64 = u64::from(base_size);
            let max_u64 = u64::from(*max);

            // PERFORMANCE OPTIMIZATION: ODO arrays are typically small
            let size = if max_u64 <= 1000 && base_u64 <= 1000 {
                // Safe to multiply directly - common case optimization
                base_u64 * max_u64
            } else {
                // Checked multiplication for large ODO arrays
                base_u64.checked_mul(max_u64).ok_or_else(|| {
                    error!(
                        ErrorCode::CBKS141_RECORD_TOO_LARGE,
                        "ODO array size overflow for field '{}'", field.name
                    )
                })?
            };

            // Record ODO information for validation
            context.odo_arrays.push(OdoInfo {
                array_path: field_path.clone(),
                counter_path: counter_path.clone(),
                array_offset: aligned_offset,
                max_count: *max,
                min_count: *min,
            });

            size
        }
        None => u64::from(base_size), // Most common case: simple scalar field
    };

    // Handle group fields recursively
    if matches!(field.kind, FieldKind::Group) {
        let mut group_size = 0u64;
        let group_start_offset = context.current_offset;

        for child in &mut field.children {
            let child_end_offset = resolve_field_layout(child, context, Some(&field_path))?;
            group_size = group_size.max(child_end_offset - group_start_offset);
        }

        field.len =
            crate::utils::safe_ops::safe_u64_to_u32(group_size, "group field length calculation")?;
        let final_offset = group_start_offset + group_size;
        context.current_offset = final_offset;

        Ok(final_offset)
    } else {
        // Scalar field
        field.len = base_size;
        let final_offset = context.current_offset + effective_size;
        context.current_offset = final_offset;

        Ok(final_offset)
    }
}

/// Resolve REDEFINES field layout
fn resolve_redefines_field(
    field: &mut Field,
    context: &mut LayoutContext,
    target: &str,
    field_path: &str,
) -> Result<u64> {
    // Find the target field offset
    let target_offset = context
        .field_paths
        .get(target)
        .or_else(|| {
            // Try to find by field name if full path not found
            context
                .field_paths
                .iter()
                .find(|(path, _)| path.ends_with(&format!(".{}", target)) || path == &target)
                .map(|(_, offset)| offset)
        })
        .copied()
        .ok_or_else(|| {
            error!(
                ErrorCode::CBKP001_SYNTAX,
                "REDEFINES target '{}' not found for field '{}'", target, field.name
            )
        })?;

    // Calculate field size and alignment (NORMATIVE: alignment applied before cluster max selection)
    let (alignment, base_size) =
        calculate_field_size_and_alignment(&field.kind, field.synchronized);
    let aligned_offset = apply_alignment(target_offset, alignment);
    let padding_bytes = aligned_offset - target_offset;

    if padding_bytes > 0 {
        field.sync_padding = Some(crate::utils::safe_ops::safe_u64_to_u16(
            padding_bytes,
            "redefines sync padding calculation",
        )?);
    }

    field.offset = crate::utils::safe_ops::safe_u64_to_u32(
        aligned_offset,
        "redefines field offset calculation",
    )?;

    // Calculate effective size including arrays
    let effective_size = match &field.occurs {
        Some(Occurs::Fixed { count }) => u64::from(base_size)
            .checked_mul(u64::from(*count))
            .ok_or_else(|| {
                error!(
                    ErrorCode::CBKS141_RECORD_TOO_LARGE,
                    "REDEFINES array size overflow for field '{}'", field.name
                )
            })?,
        Some(Occurs::ODO { max, .. }) => u64::from(base_size)
            .checked_mul(u64::from(*max))
            .ok_or_else(|| {
                error!(
                    ErrorCode::CBKS141_RECORD_TOO_LARGE,
                    "REDEFINES ODO array size overflow for field '{}'", field.name
                )
            })?,
        None => u64::from(base_size),
    };

    // Handle group fields recursively
    if matches!(field.kind, FieldKind::Group) {
        let mut group_size = 0u64;
        let saved_offset = context.current_offset;
        context.current_offset = aligned_offset;

        for child in &mut field.children {
            let child_end_offset = resolve_field_layout(child, context, Some(field_path))?;
            group_size = group_size.max(child_end_offset - aligned_offset);
        }

        field.len = crate::utils::safe_ops::safe_u64_to_u32(
            group_size,
            "redefines group field length calculation",
        )?;
        context.current_offset = saved_offset; // Restore offset (REDEFINES doesn't advance)

        // Update cluster size
        let cluster_key = target.to_string();
        let (cluster_start, current_max) = context
            .redefines_clusters
            .get(&cluster_key)
            .copied()
            .unwrap_or((target_offset, 0));

        let new_max = current_max.max(group_size);
        context
            .redefines_clusters
            .insert(cluster_key, (cluster_start, new_max));

        Ok(aligned_offset + group_size)
    } else {
        // Scalar field
        field.len = base_size;

        // Update cluster size (NORMATIVE: deterministic max selection)
        let cluster_key = target.to_string();
        let (cluster_start, current_max) = context
            .redefines_clusters
            .get(&cluster_key)
            .copied()
            .unwrap_or((target_offset, 0u64));

        let new_max = current_max.max(effective_size);
        context
            .redefines_clusters
            .insert(cluster_key, (cluster_start, new_max));

        // Record field path
        context
            .field_paths
            .insert(field_path.to_string(), aligned_offset);

        Ok(aligned_offset + effective_size)
    }
}

/// Calculate field size and alignment requirements
fn calculate_field_size_and_alignment(kind: &FieldKind, synchronized: bool) -> (u64, u32) {
    let (size, natural_alignment) = match kind {
        FieldKind::Alphanum { len } => (*len, 1u64),
        FieldKind::ZonedDecimal { digits, .. } => (u32::from(*digits), 1u64),
        FieldKind::BinaryInt { bits, .. } => {
            let bytes = u32::from(*bits) / 8;
            let alignment = if synchronized { u64::from(bytes) } else { 1u64 };
            (bytes, alignment)
        }
        FieldKind::PackedDecimal { digits, .. } => {
            // Packed decimal: ceil((digits + 1) / 2) bytes
            #[allow(clippy::manual_midpoint)] // This is ceiling division, not midpoint
            let bytes = (u32::from(*digits) + 2) / 2; // ceil((digits + 1) / 2)
            (bytes, 1u64)
        }
        FieldKind::Group => (0, 1u64), // Groups don't have inherent size
        FieldKind::Condition { .. } => (0, 1u64), // Level-88 fields don't consume storage
        FieldKind::Renames { .. } => (0, 1u64), // Level-66 fields don't consume storage
    };

    let alignment = if synchronized && natural_alignment > 1 {
        natural_alignment
    } else {
        1u64
    };

    (alignment, size)
}

/// Apply alignment to an offset
fn apply_alignment(offset: u64, alignment: u64) -> u64 {
    if alignment <= 1 {
        offset
    } else {
        offset.div_ceil(alignment) * alignment
    }
}

/// Validate ODO constraints with comprehensive error handling
fn validate_odo_constraints(context: &LayoutContext) -> Result<()> {
    for odo in &context.odo_arrays {
        // Find counter field offset
        let counter_offset = context
            .field_paths
            .get(&odo.counter_path)
            .or_else(|| {
                // Try to find by field name if full path not found
                context
                    .field_paths
                    .iter()
                    .find(|(path, _)| {
                        path.ends_with(&format!(".{}", odo.counter_path))
                            || path == &&odo.counter_path
                    })
                    .map(|(_, offset)| offset)
            })
            .copied()
            .ok_or_else(|| {
                // Create comprehensive error for missing counter field
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!(
                        "ODO counter field '{}' not found for array '{}'",
                        odo.counter_path, odo.array_path
                    ),
                )
                .with_context(crate::error::ErrorContext {
                    record_index: None,
                    field_path: Some(odo.array_path.clone()),
                    byte_offset: Some(odo.array_offset),
                    line_number: None,
                    details: Some(format!(
                        "counter_field={}, searched_paths=[{}]",
                        odo.counter_path,
                        context
                            .field_paths
                            .keys()
                            .map(std::string::String::as_str)
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                })
            })?;

        // Validate that counter precedes array (NORMATIVE)
        if counter_offset >= odo.array_offset {
            return Err(Error::new(
                ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                format!(
                    "ODO counter '{}' (offset {}) must precede array '{}' (offset {}) in byte order", 
                    odo.counter_path, counter_offset, odo.array_path, odo.array_offset
                )
            ).with_context(crate::error::ErrorContext {
                record_index: None,
                field_path: Some(odo.array_path.clone()),
                byte_offset: Some(odo.array_offset),
                line_number: None,
                details: Some(format!("counter_offset={}, array_offset={}, min_count={}, max_count={}", 
                                    counter_offset, odo.array_offset, odo.min_count, odo.max_count)),
            }));
        }

        // ODO tail validation is now handled structurally during parsing
        // The following code is commented out to avoid conflicts with structural validation
        // let has_fields_after = context.field_paths.iter().any(|(path, offset)| {
        //     // Skip the ODO array itself and its counter
        //     if path == &odo.array_path || path == &odo.counter_path {
        //         return false;
        //     }
        //
        //     // Check if this field comes after the ODO array
        //     *offset > odo.array_offset
        // });

        // ODO tail validation is now handled structurally during parsing
        // Disable layout-based tail validation to avoid conflicts
        // if has_fields_after {
        //     return Err(Error::new(
        //         ErrorCode::CBKP021_ODO_NOT_TAIL,
        //         format!(
        //             "ODO array '{}' must be at tail position (no fields after it)",
        //             odo.array_path
        //         ),
        //     )
        //     .with_context(crate::error::ErrorContext {
        //         record_index: None,
        //         field_path: Some(odo.array_path.clone()),
        //         byte_offset: Some(odo.array_offset),
        //         line_number: None,
        //         details: Some(format!(
        //             "fields_after_odo=true, counter_field={}",
        //             odo.counter_path
        //         )),
        //     }));
        // }

        debug!(
            "ODO validation passed: array='{}', counter='{}', array_offset={}, counter_offset={}",
            odo.array_path, odo.counter_path, odo.array_offset, counter_offset
        );
    }

    Ok(())
}

/// Calculate fixed record length if applicable
fn calculate_fixed_record_length(schema: &mut Schema, context: &LayoutContext) -> Result<()> {
    // Check if all fields are fixed (no ODO arrays)
    let has_odo = context
        .odo_arrays
        .iter()
        .any(|odo| odo.max_count > odo.min_count);

    if !has_odo {
        // Calculate total size including REDEFINES clusters
        let mut total_size = context.current_offset;

        // Add the size of REDEFINES clusters (they don't advance current_offset)
        for (cluster_start, cluster_size) in context.redefines_clusters.values() {
            let cluster_end = cluster_start + cluster_size;
            total_size = total_size.max(cluster_end);
        }

        schema.lrecl_fixed = Some(crate::utils::safe_ops::safe_u64_to_u32(
            total_size,
            "fixed record length calculation",
        )?);
    }

    Ok(())
}

/// Detect and set tail ODO information
fn detect_tail_odo(schema: &mut Schema, context: &LayoutContext) {
    // Find the ODO array with the highest offset (tail position)
    if let Some(tail_odo) = context.odo_arrays.iter().max_by_key(|odo| odo.array_offset) {
        // Extract just the field name from the full path for consistency
        let array_field_name = tail_odo
            .array_path
            .split('.')
            .next_back()
            .unwrap_or(&tail_odo.array_path)
            .to_string();
        let counter_field_name = tail_odo
            .counter_path
            .split('.')
            .next_back()
            .unwrap_or(&tail_odo.counter_path)
            .to_string();

        schema.tail_odo = Some(TailODO {
            counter_path: counter_field_name,
            min_count: tail_odo.min_count,
            max_count: tail_odo.max_count,
            array_path: array_field_name,
        });
    }
}

fn head_ident_of_qname(q: &str) -> &str {
    // QNAME ::= IDENT ('OF' IDENT)* — current implementation uses only the head IDENT.
    q.split_whitespace().next().unwrap_or(q)
}

fn find_sibling_index_by_qname(siblings: &[crate::schema::Field], qname: &str) -> Option<usize> {
    let needle = head_ident_of_qname(qname).trim();
    siblings
        .iter()
        .enumerate()
        .filter(|(_, f)| f.level != 66 && f.level != 88) // storage-bearing fields only
        .find(|(_, f)| f.name.trim().eq_ignore_ascii_case(needle))
        .map(|(i, _)| i)
}

/// Resolve RENAMES (level-66) aliases (post-order).
/// Current implementation: same-scope resolution + contiguous slice → (offset, length, members).
fn resolve_renames_aliases(fields: &mut [crate::schema::Field]) -> Result<()> {
    use crate::error::ErrorCode;
    use crate::schema::{FieldKind, ResolvedRenames};

    // 1) Recurse first (post-order)
    for f in fields.iter_mut() {
        if !f.children.is_empty() {
            resolve_renames_aliases(&mut f.children)?;
        }
    }

    // 2) Resolve at this scope
    let mut resolutions: Vec<(usize, u32, u32, Vec<String>)> = Vec::new();
    for (idx, field) in fields.iter().enumerate() {
        if let FieldKind::Renames {
            ref from_field,
            ref thru_field,
        } = field.kind
        {
            let from_i = find_sibling_index_by_qname(fields, from_field).ok_or_else(|| {
                error!(
                    ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
                    "RENAMES from field '{}' not found", from_field
                )
            })?;
            let thru_i = find_sibling_index_by_qname(fields, thru_field).ok_or_else(|| {
                error!(
                    ErrorCode::CBKS602_RENAME_UNKNOWN_THRU,
                    "RENAMES thru field '{}' not found", thru_field
                )
            })?;

            if from_i > thru_i {
                return Err(error!(
                    ErrorCode::CBKS604_RENAME_REVERSED_RANGE,
                    "RENAMES from '{}' comes after thru '{}'", from_field, thru_field
                ));
            }

            // Validate no OCCURS boundary: check for OCCURS within the range
            // This must be checked first before other validations
            for f in &fields[from_i..=thru_i] {
                if matches!(f.occurs, Some(Occurs::Fixed { .. } | Occurs::ODO { .. })) {
                    return Err(error!(
                        ErrorCode::CBKS607_RENAME_CROSSES_OCCURS,
                        "RENAMES range from '{}' to '{}' crosses OCCURS boundary at field '{}'",
                        from_field,
                        thru_field,
                        f.name
                    ));
                }
            }

            // Helper: check if a field has storage-bearing children (not just level-88)
            let has_storage_children = |field: &Field| {
                field
                    .children
                    .iter()
                    .any(|child| child.level != 88 && child.level != 66)
            };

            // Validate no cross-branch: from/thru must not span across groups
            // Check if from is a group with storage-bearing children
            if has_storage_children(&fields[from_i]) {
                return Err(error!(
                    ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP,
                    "RENAMES from field '{}' is a group with storage-bearing children; cannot span groups",
                    from_field
                ));
            }
            // Check if thru is a group with storage-bearing children
            if has_storage_children(&fields[thru_i]) {
                return Err(error!(
                    ErrorCode::CBKS606_RENAME_THRU_CROSSES_GROUP,
                    "RENAMES thru field '{}' is a group with storage-bearing children; cannot span groups",
                    thru_field
                ));
            }
            // Check if any field in between is a group with storage-bearing children (would create cross-branch)
            if from_i + 1 < thru_i {
                for f in &fields[(from_i + 1)..thru_i] {
                    if has_storage_children(f) {
                        return Err(error!(
                            ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP,
                            "RENAMES range from '{}' to '{}' crosses group boundary at field '{}'",
                            from_field,
                            thru_field,
                            f.name
                        ));
                    }
                }
            }

            // Note: We do NOT enforce strict byte-level contiguity (CBKS603).
            // COBOL RENAMES requires fields to be in source order within the same group,
            // but REDEFINES can create overlapping offsets which is valid.
            // The key validations are: same group, no OCCURS, no cross-branch, from before thru.

            // Compute (offset, length) and members (storage-bearing only).
            let offset = fields[from_i].offset;
            let end_offset_u64 = (fields[thru_i].offset as u64) + (fields[thru_i].len as u64);
            let length: u32 = (end_offset_u64 - (offset as u64)).try_into().map_err(|_| {
                error!(
                    ErrorCode::CBKS141_RECORD_TOO_LARGE,
                    "RENAMES alias '{}' exceeds maximum size", field.name
                )
            })?;
            let mut members = Vec::new();
            for field in &fields[from_i..=thru_i] {
                let lvl = field.level;
                if lvl != 66 && lvl != 88 {
                    // storage-bearing only (exclude non-storage fields: 66, 88)
                    members.push(field.path.clone());
                }
            }

            resolutions.push((idx, offset, length, members));
        }
    }

    // Second pass: apply resolutions
    for (idx, offset, length, members) in resolutions {
        fields[idx].resolved_renames = Some(ResolvedRenames {
            offset,
            length,
            members,
        });
    }

    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use crate::schema::{Field, FieldKind, Occurs};

    #[test]
    fn test_simple_field_layout() {
        let mut schema = Schema::new();
        let field = Field::with_kind(1, "TEST-FIELD".to_string(), FieldKind::Alphanum { len: 10 });
        schema.fields.push(field);

        resolve_layout(&mut schema).unwrap();

        let field = &schema.fields[0];
        assert_eq!(field.offset, 0);
        assert_eq!(field.len, 10);
        assert_eq!(schema.lrecl_fixed, Some(10));
    }

    #[test]
    fn test_binary_field_alignment() {
        let mut schema = Schema::new();

        // Add a char field first to create misalignment
        let char_field =
            Field::with_kind(1, "CHAR-FIELD".to_string(), FieldKind::Alphanum { len: 1 });
        schema.fields.push(char_field);

        // Add synchronized binary field
        let mut binary_field = Field::with_kind(
            1,
            "BINARY-FIELD".to_string(),
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
        );
        binary_field.synchronized = true;
        schema.fields.push(binary_field);

        resolve_layout(&mut schema).unwrap();

        let char_field = &schema.fields[0];
        assert_eq!(char_field.offset, 0);
        assert_eq!(char_field.len, 1);

        let binary_field = &schema.fields[1];
        assert_eq!(binary_field.offset, 4); // Aligned to 4-byte boundary
        assert_eq!(binary_field.len, 4);
        assert_eq!(binary_field.sync_padding, Some(3)); // 3 padding bytes
    }

    #[test]
    fn test_occurs_fixed_array() {
        let mut schema = Schema::new();
        let mut field =
            Field::with_kind(1, "ARRAY-FIELD".to_string(), FieldKind::Alphanum { len: 5 });
        field.occurs = Some(Occurs::Fixed { count: 10 });
        schema.fields.push(field);

        resolve_layout(&mut schema).unwrap();

        let field = &schema.fields[0];
        assert_eq!(field.offset, 0);
        assert_eq!(field.len, 5); // Base field size
        assert_eq!(schema.lrecl_fixed, Some(50)); // 5 * 10
    }

    #[test]
    fn test_odo_array() {
        let mut schema = Schema::new();

        // Counter field
        let counter = Field::with_kind(
            1,
            "COUNTER".to_string(),
            FieldKind::ZonedDecimal {
                digits: 3,
                scale: 0,
                signed: false,
            },
        );
        schema.fields.push(counter);

        // ODO array field
        let mut array_field = Field::with_kind(
            1,
            "ARRAY-FIELD".to_string(),
            FieldKind::Alphanum { len: 10 },
        );
        array_field.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "COUNTER".to_string(),
        });
        schema.fields.push(array_field);

        resolve_layout(&mut schema).unwrap();

        let counter = &schema.fields[0];
        assert_eq!(counter.offset, 0);
        assert_eq!(counter.len, 3);

        let array_field = &schema.fields[1];
        assert_eq!(array_field.offset, 3);
        assert_eq!(array_field.len, 10); // Base field size

        // Should have tail ODO info
        assert!(schema.tail_odo.is_some());
        let tail_odo = schema.tail_odo.as_ref().unwrap();
        assert_eq!(tail_odo.counter_path, "COUNTER");
        assert_eq!(tail_odo.max_count, 5);

        // Should not have fixed LRECL due to ODO
        assert!(schema.lrecl_fixed.is_none());
    }

    #[test]
    fn test_redefines_cluster_sizing() {
        let mut schema = Schema::new();

        // Original field
        let field_a = Field::with_kind(1, "FIELD-A".to_string(), FieldKind::Alphanum { len: 10 });
        schema.fields.push(field_a);

        // Shorter redefines
        let mut field_b =
            Field::with_kind(1, "FIELD-B".to_string(), FieldKind::Alphanum { len: 5 });
        field_b.redefines_of = Some("FIELD-A".to_string());
        schema.fields.push(field_b);

        // Longer redefines
        let mut field_c =
            Field::with_kind(1, "FIELD-C".to_string(), FieldKind::Alphanum { len: 15 });
        field_c.redefines_of = Some("FIELD-A".to_string());
        schema.fields.push(field_c);

        resolve_layout(&mut schema).unwrap();

        let field_a = &schema.fields[0];
        assert_eq!(field_a.offset, 0);
        assert_eq!(field_a.len, 10);

        let field_b = &schema.fields[1];
        assert_eq!(field_b.offset, 0); // Same offset as FIELD-A
        assert_eq!(field_b.len, 5);

        let field_c = &schema.fields[2];
        assert_eq!(field_c.offset, 0); // Same offset as FIELD-A
        assert_eq!(field_c.len, 15);

        // Total record size should be based on largest variant (15)
        assert_eq!(schema.lrecl_fixed, Some(15));
    }

    #[test]
    fn test_record_size_overflow() {
        let mut schema = Schema::new();

        // Create a field that would cause overflow
        let mut huge_field = Field::with_kind(
            1,
            "HUGE-FIELD".to_string(),
            FieldKind::Alphanum { len: u32::MAX },
        );
        huge_field.occurs = Some(Occurs::Fixed { count: 1000 });
        schema.fields.push(huge_field);

        let result = resolve_layout(&mut schema);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKS141_RECORD_TOO_LARGE
        ));
    }

    #[test]
    fn test_odo_counter_validation() {
        let mut schema = Schema::new();

        // ODO array without counter field
        let mut array_field = Field::with_kind(
            1,
            "ARRAY-FIELD".to_string(),
            FieldKind::Alphanum { len: 10 },
        );
        array_field.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "NONEXISTENT".to_string(),
        });
        schema.fields.push(array_field);

        let result = resolve_layout(&mut schema);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKS121_COUNTER_NOT_FOUND
        ));
    }

    #[test]
    fn test_packed_decimal_size() {
        let mut schema = Schema::new();

        // 7-digit packed decimal should be 4 bytes: ceil((7+1)/2) = 4
        let field = Field::with_kind(
            1,
            "PACKED-FIELD".to_string(),
            FieldKind::PackedDecimal {
                digits: 7,
                scale: 2,
                signed: true,
            },
        );
        schema.fields.push(field);

        resolve_layout(&mut schema).unwrap();

        let field = &schema.fields[0];
        assert_eq!(field.len, 4);
    }

    #[test]
    fn test_binary_width_mapping() {
        let mut schema = Schema::new();

        // Test different binary widths
        let test_cases = vec![
            (4, 16),  // ≤4 digits → 2 bytes (16 bits)
            (9, 32),  // 5-9 digits → 4 bytes (32 bits)
            (18, 64), // 10-18 digits → 8 bytes (64 bits)
        ];

        for (digits, expected_bits) in test_cases {
            let field = Field::with_kind(
                1,
                format!("BINARY-{}", digits),
                FieldKind::BinaryInt {
                    bits: expected_bits,
                    signed: false,
                },
            );
            schema.fields.push(field);
        }

        resolve_layout(&mut schema).unwrap();

        assert_eq!(schema.fields[0].len, 2); // 16 bits = 2 bytes
        assert_eq!(schema.fields[1].len, 4); // 32 bits = 4 bytes
        assert_eq!(schema.fields[2].len, 8); // 64 bits = 8 bytes
    }
}
