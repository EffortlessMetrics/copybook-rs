//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use crate::write_stdout_all;
use copybook_codec::Codepage;
use copybook_core::{Field, FieldKind, Occurs, ParseOptions, parse_copybook_with_options};
use std::fmt::Write as _;
use std::path::PathBuf;
use tracing::info;

pub fn run(
    copybook: &PathBuf,
    codepage: Codepage,
    strict: bool,
    strict_comments: bool,
    dialect: crate::DialectPreference,
) -> anyhow::Result<ExitCode> {
    info!("Inspecting copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict_comments,
        strict,
        codepage: codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: dialect.into(),
    };
    let schema = parse_copybook_with_options(&copybook_text, &options)?;

    // Display human-readable layout
    let mut output = String::new();
    output.push_str("Copybook Layout\n");
    output.push_str("===============\n");
    writeln!(output, "Codepage: {codepage:?}").ok();
    writeln!(output, "Fixed LRECL: {:?}", schema.lrecl_fixed).ok();
    output.push('\n');
    writeln!(
        output,
        "{:<40} {:<8} {:<8} {:<12} {:<20}",
        "Structure", "Offset", "Length", "Type", "Details"
    )
    .ok();
    writeln!(output, "{:-<88}", "").ok();

    for field in &schema.fields {
        let (type_str, details) = get_field_info(field);
        writeln!(
            output,
            "{:<40} {:<8} {:<8} {:<12} {:<20}",
            field.name, field.offset, field.len, type_str, details
        )
        .ok();

        print_tree_recursive(&field.children, "", &mut output);
    }

    write_stdout_all(output.as_bytes())?;

    info!("Inspect completed successfully");
    Ok(ExitCode::Ok)
}

fn get_field_info(field: &Field) -> (String, String) {
    let type_str = match &field.kind {
        FieldKind::Alphanum { len } => format!("X({len})"),
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
        } => {
            if *signed {
                format!("S9({digits})V9({scale})")
            } else {
                format!("9({digits})V9({scale})")
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            format!("COMP-{} ({}bit)", if *signed { "S" } else { "" }, bits)
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            if *signed {
                format!("S9({digits})V9({scale}) COMP-3")
            } else {
                format!("9({digits})V9({scale}) COMP-3")
            }
        }
        FieldKind::Group => "GROUP".to_string(),
        FieldKind::Condition { values } => {
            format!("LEVEL-88: {values:?}")
        }
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            format!("RENAMES {from_field} THRU {thru_field}")
        }
        FieldKind::EditedNumeric { pic_string, .. } => {
            format!("EDITED PIC {pic_string}")
        }
    };

    let details = if let Some(ref occurs) = field.occurs {
        match occurs {
            Occurs::Fixed { count } => format!("OCCURS {count}"),
            Occurs::ODO {
                min,
                max,
                counter_path,
            } => {
                format!("ODO {min}-{max} ({counter_path})")
            }
        }
    } else {
        String::new()
    };

    (type_str, details)
}

fn print_tree_recursive(fields: &[Field], prefix: &str, output: &mut String) {
    for (i, field) in fields.iter().enumerate() {
        let is_last = i == fields.len() - 1;

        let connector = if is_last { "└── " } else { "├── " };
        let name_display = format!("{}{}{}", prefix, connector, field.name);

        let (type_str, details) = get_field_info(field);

        writeln!(
            output,
            "{:<40} {:<8} {:<8} {:<12} {:<20}",
            name_display, field.offset, field.len, type_str, details
        )
        .ok();

        if !field.children.is_empty() {
            let child_prefix_add = if is_last { "    " } else { "│   " };
            let child_prefix = format!("{}{}", prefix, child_prefix_add);
            print_tree_recursive(&field.children, &child_prefix, output);
        }
    }
}
