use copybook_core::{Field, Occurs, Schema};

#[inline]
pub(crate) fn calculate_schema_fixed_prefix(schema: &Schema) -> u32 {
    let mut first_odo_offset: Option<u32> = None;
    find_first_odo_offset(&schema.fields, &mut first_odo_offset);

    if let Some(offset) = first_odo_offset {
        offset
    } else if let Some(lrecl) = schema.lrecl_fixed {
        lrecl
    } else {
        let mut max_end = 0;
        find_record_end(&schema.fields, &mut max_end);
        max_end
    }
}

fn find_first_odo_offset(fields: &[Field], current: &mut Option<u32>) {
    for field in fields {
        if let Some(Occurs::ODO { .. }) = &field.occurs {
            let offset = field.offset;
            match current {
                Some(existing) => {
                    if offset < *existing {
                        *current = Some(offset);
                    }
                }
                None => *current = Some(offset),
            }
        }
        if !field.children.is_empty() {
            find_first_odo_offset(&field.children, current);
        }
    }
}

fn find_record_end(fields: &[Field], max_end: &mut u32) {
    for field in fields {
        let end = field.offset + field.len;
        if end > *max_end {
            *max_end = end;
        }
        if !field.children.is_empty() {
            find_record_end(&field.children, max_end);
        }
    }
}
