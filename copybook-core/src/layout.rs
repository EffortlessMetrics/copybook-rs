//! Layout resolution and field offset calculation
//!
//! This module handles the computation of field byte offsets,
//! alignment padding, and REDEFINES cluster sizing.

use crate::dialect::{Dialect, effective_min_count};
use crate::feature_flags::{Feature, FeatureFlags};
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
/// The `dialect` parameter controls how ODO `min_count` is interpreted:
/// - `Normative`: `min_count` enforced as declared
/// - `ZeroTolerant`: `min_count` always treated as 0
/// - `OneTolerant`: `min_count` clamped to at least 1
///
/// # Errors
/// Returns an error if field configuration is invalid or the record size exceeds limits.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn resolve_layout(schema: &mut Schema, dialect: Dialect) -> Result<()> {
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

    // Detect and set tail ODO information (applying dialect to min_count)
    detect_tail_odo(schema, &context, dialect);

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

        // Calculate effective size including OCCURS on the group
        let group_effective_size = match &field.occurs {
            Some(Occurs::Fixed { count }) => {
                let count_u64 = u64::from(*count);
                if count_u64 <= 1000 && group_size <= 1000 {
                    group_size * count_u64
                } else {
                    group_size.checked_mul(count_u64).ok_or_else(|| {
                        error!(
                            ErrorCode::CBKS141_RECORD_TOO_LARGE,
                            "Group OCCURS size overflow for field '{}'", field.name
                        )
                    })?
                }
            }
            Some(Occurs::ODO { max, .. }) => {
                let max_u64 = u64::from(*max);
                if max_u64 <= 1000 && group_size <= 1000 {
                    group_size * max_u64
                } else {
                    group_size.checked_mul(max_u64).ok_or_else(|| {
                        error!(
                            ErrorCode::CBKS141_RECORD_TOO_LARGE,
                            "Group ODO size overflow for field '{}'", field.name
                        )
                    })?
                }
            }
            None => group_size,
        };

        let final_offset = group_start_offset + group_effective_size;
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

        // Calculate effective size including OCCURS on the group
        let group_effective_size = match &field.occurs {
            Some(Occurs::Fixed { count }) => {
                let count_u64 = u64::from(*count);
                group_size.checked_mul(count_u64).ok_or_else(|| {
                    error!(
                        ErrorCode::CBKS141_RECORD_TOO_LARGE,
                        "REDEFINES group OCCURS size overflow for field '{}'", field.name
                    )
                })?
            }
            Some(Occurs::ODO { max, .. }) => {
                let max_u64 = u64::from(*max);
                group_size.checked_mul(max_u64).ok_or_else(|| {
                    error!(
                        ErrorCode::CBKS141_RECORD_TOO_LARGE,
                        "REDEFINES group ODO size overflow for field '{}'", field.name
                    )
                })?
            }
            None => group_size,
        };

        // Update cluster size
        let cluster_key = target.to_string();
        let (cluster_start, current_max) = context
            .redefines_clusters
            .get(&cluster_key)
            .copied()
            .unwrap_or((target_offset, 0));

        let new_max = current_max.max(group_effective_size);
        context
            .redefines_clusters
            .insert(cluster_key, (cluster_start, new_max));

        Ok(aligned_offset + group_effective_size)
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
        FieldKind::ZonedDecimal {
            digits,
            sign_separate,
            ..
        } => {
            // SIGN SEPARATE adds an extra byte for the sign
            let base_size = u32::from(*digits);
            let size = if sign_separate.is_some() {
                base_size + 1 // Add 1 byte for separate sign
            } else {
                base_size
            };
            (size, 1u64)
        }
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
        FieldKind::EditedNumeric { width, .. } => (u32::from(*width), 1u64), // Phase E1: use display width
        FieldKind::FloatSingle => (4, if synchronized { 4u64 } else { 1u64 }),
        FieldKind::FloatDouble => (8, if synchronized { 8u64 } else { 1u64 }),
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
///
/// The `dialect` parameter controls how `min_count` is interpreted:
/// - `Normative`: `min_count` stored as declared
/// - `ZeroTolerant`: `min_count` stored as 0
/// - `OneTolerant`: `min_count` stored as max(1, declared)
fn detect_tail_odo(schema: &mut Schema, context: &LayoutContext, dialect: Dialect) {
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

        // Apply dialect to min_count: this is where dialect affects runtime behavior
        let effective_min = effective_min_count(dialect, tail_odo.min_count);

        schema.tail_odo = Some(TailODO {
            counter_path: counter_field_name,
            min_count: effective_min,
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

/// Find a field anywhere in the tree by name (for RENAMES R2/R3 support).
/// Returns the field reference if found.
fn find_field_by_name<'a>(
    fields: &'a [crate::schema::Field],
    name: &str,
) -> Option<&'a crate::schema::Field> {
    let needle = head_ident_of_qname(name).trim();

    for field in fields {
        // Check current field (exclude non-storage: 66, 88)
        if field.level != 66 && field.level != 88 && field.name.trim().eq_ignore_ascii_case(needle)
        {
            return Some(field);
        }

        // Recurse into children
        if !field.children.is_empty()
            && let Some(found) = find_field_by_name(&field.children, name)
        {
            return Some(found);
        }
    }

    None
}

/// Collect all storage-bearing field paths from a group (recursive).
/// Used for R2/R3 RENAMES over groups/subtrees.
fn collect_storage_paths(field: &crate::schema::Field, paths: &mut Vec<String>) {
    // If this field has storage (not a group-only node), add its path
    if !matches!(field.kind, crate::schema::FieldKind::Group) {
        paths.push(field.path.clone());
    }

    // Recurse into children to collect their storage paths
    for child in &field.children {
        if child.level != 66 && child.level != 88 {
            collect_storage_paths(child, paths);
        }
    }
}

/// Check if a field is a REDEFINES field (has redefines_of set)
/// Used for R4 pattern detection
fn field_is_redefines(field: &crate::schema::Field) -> bool {
    field.redefines_of.is_some()
}

/// Check if a field or any of its descendants have REDEFINES
/// Used for R4 pattern detection
fn field_has_redefines(field: &crate::schema::Field) -> bool {
    // Check if this field is a REDEFINES field
    if field_is_redefines(field) {
        return true;
    }
    // Check if any child has REDEFINES
    for child in &field.children {
        if field_has_redefines(child) {
            return true;
        }
    }
    false
}

/// Check if a field is an OCCURS array (fixed or ODO)
/// Used for R5 pattern detection
fn field_is_occurs(field: &crate::schema::Field) -> bool {
    matches!(
        field.occurs,
        Some(Occurs::Fixed { .. } | Occurs::ODO { .. })
    )
}

/// Check if a field is an ODO array specifically
/// Used for R5 pattern detection (ODO is not supported even with feature flag)
fn field_is_odo(field: &crate::schema::Field) -> bool {
    matches!(field.occurs, Some(Occurs::ODO { .. }))
}

/// Count how many REDEFINES alternatives exist in a range of fields
/// Used for R4 pattern validation
fn count_redefines_alternatives(
    fields: &[crate::schema::Field],
    from_idx: usize,
    thru_idx: usize,
) -> usize {
    let mut count = 0;
    for f in &fields[from_idx..=thru_idx] {
        if field_has_redefines(f) {
            count += 1;
        }
    }
    count
}

/// Resolve RENAMES (level-66) aliases (post-order).
/// Current implementation: same-scope resolution + contiguous slice → (offset, length, members).
/// Extended to detect and reject R4-R6 patterns unless feature flag is enabled.
fn resolve_renames_aliases(fields: &mut [crate::schema::Field]) -> Result<()> {
    use crate::error::ErrorCode;
    use crate::schema::{FieldKind, ResolvedRenames};

    // Check if R4-R6 feature flag is enabled
    let r4_r6_enabled = FeatureFlags::global().is_enabled(Feature::RenamesR4R6);

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
            // Try sibling lookup first (R1/R2 case)
            let from_i_opt = find_sibling_index_by_qname(fields, from_field);
            let thru_i_opt = find_sibling_index_by_qname(fields, thru_field);

            // Check if this is a nested single-group RENAMES (R3 case)
            let is_nested_single_group =
                from_i_opt.is_none() && thru_i_opt.is_none() && from_field == thru_field;

            if is_nested_single_group {
                // R3: Nested group RENAMES - find target anywhere in subtree
                let target_field = find_field_by_name(fields, from_field).ok_or_else(|| {
                    error!(
                        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
                        "RENAMES nested target field '{}' not found", from_field
                    )
                })?;

                // R6 check: Level-88 fields are naturally excluded (non-storage)
                // No special handling needed - Level-88 is already excluded in collect_storage_paths

                // Compute offset, length, and members from target group
                let offset = target_field.offset;
                let length = target_field.len;
                let mut members = Vec::new();
                collect_storage_paths(target_field, &mut members);

                // Store resolution for this alias
                resolutions.push((idx, offset, length, members));
                continue; // Skip to next field
            }

            // R1/R2: Same-scope RENAMES - use sibling lookup
            let from_i = from_i_opt.ok_or_else(|| {
                error!(
                    ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
                    "RENAMES from field '{}' not found", from_field
                )
            })?;
            let thru_i = thru_i_opt.ok_or_else(|| {
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

            // R5 check: Validate no OCCURS boundary (or ODO) within the range
            // This must be checked first before other validations
            for f in &fields[from_i..=thru_i] {
                if field_is_occurs(f) {
                    if field_is_odo(f) {
                        // R5: ODO arrays are not supported even with feature flag
                        return Err(error!(
                            ErrorCode::CBKS612_RENAME_ODO_NOT_SUPPORTED,
                            "RENAMES alias '{}' spans ODO array '{}'. This pattern is not supported.",
                            field.name,
                            f.name
                        ));
                    }
                    if !r4_r6_enabled {
                        // R5: OCCURS arrays are rejected when feature flag is disabled
                        return Err(error!(
                            ErrorCode::CBKS607_RENAME_CROSSES_OCCURS,
                            "RENAMES alias '{}' crosses OCCURS boundary at field '{}'. Enable RenamesR4R6 feature flag to support this pattern.",
                            field.name,
                            f.name
                        ));
                    }
                    // When feature flag is enabled, check for partial array span
                    if from_i != thru_i {
                        // R5: Partial array span is not supported even with feature flag
                        return Err(error!(
                            ErrorCode::CBKS611_RENAME_PARTIAL_OCCURS,
                            "RENAMES alias '{}' spans partial array elements. Only single-array RENAMES (FROM==THRU pointing to OCCURS field) is supported.",
                            field.name
                        ));
                    }
                    // R5: Single-array RENAMES is allowed when feature flag is enabled
                    // Continue with normal resolution
                }
            }

            // R4 check: Validate no REDEFINES within the range
            // This must be checked before group boundary validation
            if r4_r6_enabled {
                // When feature flag is enabled, check for multiple REDEFINES alternatives
                let redefines_count = count_redefines_alternatives(fields, from_i, thru_i);
                if redefines_count > 1 {
                    // R4: Multiple REDEFINES alternatives are not supported even with feature flag
                    return Err(error!(
                        ErrorCode::CBKS610_RENAME_MULTIPLE_REDEFINES,
                        "RENAMES alias '{}' spans multiple REDEFINES alternatives. Only single-alternative RENAMES is supported.",
                        field.name
                    ));
                }
                // R4: Single REDEFINES alternative is allowed when feature flag is enabled
                // Continue with normal resolution
            } else {
                // R4: REDEFINES are rejected when feature flag is disabled
                let redefines_count = count_redefines_alternatives(fields, from_i, thru_i);
                if redefines_count > 0 {
                    return Err(error!(
                        ErrorCode::CBKS609_RENAME_OVER_REDEFINES,
                        "RENAMES alias '{}' spans REDEFINES field(s). Enable RenamesR4R6 feature flag to support this pattern.",
                        field.name
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

            // R2/R3 support: Detect single-group RENAMES (FROM==THRU and it's a group)
            // This is valid for same-scope groups (R2) and nested groups (R3)
            let is_single_group_rename = from_i == thru_i && has_storage_children(&fields[from_i]);

            // Validate no cross-branch: from/thru must not span across groups
            // UNLESS it's a single-group rename (R2/R3 case)
            if !is_single_group_rename {
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

            // Collect members based on whether this is a single-group RENAMES (R2/R3) or range (R1)
            let mut members = Vec::new();
            if is_single_group_rename {
                // R2/R3: Single group RENAMES - collect storage paths from group's children
                collect_storage_paths(&fields[from_i], &mut members);
            } else {
                // R1: Range RENAMES - collect paths from the sibling range
                for field in &fields[from_i..=thru_i] {
                    let lvl = field.level;
                    if lvl != 66 && lvl != 88 {
                        // storage-bearing only (exclude non-storage fields: 66, 88)
                        members.push(field.path.clone());
                    }
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
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use crate::schema::{Field, FieldKind, Occurs};

    // Default dialect for tests (Normative = min_count enforced as declared)
    const TEST_DIALECT: Dialect = Dialect::Normative;

    #[test]
    fn test_simple_field_layout() {
        let mut schema = Schema::new();
        let field = Field::with_kind(1, "TEST-FIELD".to_string(), FieldKind::Alphanum { len: 10 });
        schema.fields.push(field);

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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
                sign_separate: None,
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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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

        let result = resolve_layout(&mut schema, TEST_DIALECT);
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

        let result = resolve_layout(&mut schema, TEST_DIALECT);
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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

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

        resolve_layout(&mut schema, TEST_DIALECT).unwrap();

        assert_eq!(schema.fields[0].len, 2); // 16 bits = 2 bytes
        assert_eq!(schema.fields[1].len, 4); // 32 bits = 4 bytes
        assert_eq!(schema.fields[2].len, 8); // 64 bits = 8 bytes
    }
}
