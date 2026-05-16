//! Copybook schema parsing and field analysis helpers.

use super::*;

pub(super) fn parse_copybook_schema(path: &Path) -> AuditResult<Schema> {
    let copybook_text = fs::read_to_string(path)?;
    Ok(copybook_core::parse_copybook(&copybook_text)?)
}

pub(super) fn collect_leaf_fields<'a>(fields: &'a [Field], out: &mut Vec<&'a Field>) {
    for field in fields {
        if field.children.is_empty() {
            out.push(field);
        } else {
            collect_leaf_fields(&field.children, out);
        }
    }
}

pub(super) fn map_field_type_name(field: &Field) -> String {
    match &field.kind {
        copybook_core::FieldKind::Alphanum { len } => format!("alphanum[{len}]"),
        copybook_core::FieldKind::ZonedDecimal { digits, .. } => format!("zoned-decimal[{digits}]"),
        copybook_core::FieldKind::BinaryInt { bits, .. } => format!("binary-int[{bits}]"),
        copybook_core::FieldKind::PackedDecimal { digits, .. } => {
            format!("packed-decimal[{digits}]")
        }
        copybook_core::FieldKind::Group => "group".to_string(),
        copybook_core::FieldKind::Condition { .. } => "condition".to_string(),
        copybook_core::FieldKind::Renames { .. } => "renames".to_string(),
        copybook_core::FieldKind::EditedNumeric { width, .. } => format!("edited-numeric[{width}]"),
        copybook_core::FieldKind::FloatSingle => "float-single".to_string(),
        copybook_core::FieldKind::FloatDouble => "float-double".to_string(),
    }
}

pub(super) fn estimate_schema_bytes(schema: &Schema) -> (u64, u64, u64) {
    let mut display_bytes = 0u64;
    let mut comp3_bytes = 0u64;
    let mut total_bytes = 0u64;

    let mut fields = Vec::new();
    collect_leaf_fields(&schema.fields, &mut fields);
    for field in fields {
        let (display, comp3) = match &field.kind {
            FieldKind::Alphanum { len } => (*len as u64, 0),
            FieldKind::PackedDecimal { digits, .. } => (0, u64::from(*digits) / 2 + 1),
            FieldKind::BinaryInt { bits, .. } => (0, u64::from(*bits) / 8),
            FieldKind::ZonedDecimal { digits, .. } => (0, (u64::from(*digits) + 1) / 2),
            FieldKind::EditedNumeric { width, .. } => (*width as u64, 0),
            FieldKind::FloatSingle => (0, 4),
            FieldKind::FloatDouble => (0, 8),
            _ => (0, 0),
        };

        display_bytes += display;
        comp3_bytes += comp3;
        total_bytes += display + comp3;
    }

    if total_bytes == 0 {
        total_bytes = schema.lrecl_fixed.unwrap_or(128) as u64;
        display_bytes = (total_bytes * 80) / 100;
        comp3_bytes = total_bytes - display_bytes;
    }

    (display_bytes, comp3_bytes, total_bytes)
}

fn is_sensitive_field_name(name: &str) -> bool {
    let candidate = name.to_ascii_uppercase();
    candidate.contains("SSN")
        || candidate.contains("SOCIAL")
        || candidate.contains("PASSWORD")
        || candidate.contains("CREDIT")
        || candidate.contains("CARD")
        || candidate.contains("ACCOUNT")
        || candidate.contains("DOB")
        || candidate.contains("BIRTH")
        || candidate.contains("SALARY")
        || candidate.contains("BALANCE")
        || candidate.contains("ROUTING")
        || candidate.contains("TAX-ID")
        || candidate.contains("MEDICAL")
        || candidate.contains("PATIENT")
        || candidate.contains("DIAGNOSIS")
        || candidate.contains("PIN")
        || candidate.contains("ENCRYPT")
        || candidate.contains("SECRET")
}

pub(super) fn collect_sensitive_fields(schema: &Schema) -> Vec<String> {
    let mut sensitive = Vec::new();
    let mut leaf_fields = Vec::new();
    collect_leaf_fields(&schema.fields, &mut leaf_fields);

    for field in leaf_fields {
        if is_sensitive_field_name(&field.name) {
            sensitive.push(field.name.clone());
        }
    }

    sensitive
}

pub(super) fn generate_synthetic_benchmark_data(schema: &Schema, records: usize) -> Vec<u8> {
    let (_, _, record_size) = estimate_schema_bytes(schema);
    let record_size = record_size.max(128);
    let total_records = records.max(1);
    vec![0xF0; record_size as usize * total_records]
}
