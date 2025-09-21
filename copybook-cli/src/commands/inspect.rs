//! Inspect command implementation

use crate::utils::read_file_or_stdin;
use copybook_codec::Codepage;
use copybook_core::{ParseOptions, parse_copybook_with_options};
use std::path::PathBuf;
use tracing::info;

pub fn run(
    copybook: &PathBuf,
    codepage: Codepage,
    strict: bool,
    strict_comments: bool,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Inspecting copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict_comments: false,
        strict,
        codepage: codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
    };
    let schema = parse_copybook_with_options(&copybook_text, &options)?;

    // Display human-readable layout
    println!("Copybook Layout");
    println!("===============");
    println!("Codepage: {codepage:?}");
    println!("Fixed LRECL: {:?}", schema.lrecl_fixed);
    println!();

    println!(
        "{:<40} {:<8} {:<8} {:<12} {:<20}",
        "Field Path", "Offset", "Length", "Type", "Details"
    );
    println!("{:-<88}", "");

    for field in schema.all_fields() {
        let type_str = match &field.kind {
            copybook_core::FieldKind::Alphanum { len } => format!("X({len})"),
            copybook_core::FieldKind::ZonedDecimal {
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
            copybook_core::FieldKind::BinaryInt { bits, signed } => {
                format!("COMP-{} ({}bit)", if *signed { "S" } else { "" }, bits)
            }
            copybook_core::FieldKind::PackedDecimal {
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
            copybook_core::FieldKind::Group => "GROUP".to_string(),
        };

        let details = if let Some(ref occurs) = field.occurs {
            match occurs {
                copybook_core::Occurs::Fixed { count } => format!("OCCURS {count}"),
                copybook_core::Occurs::ODO {
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

        println!(
            "{:<40} {:<8} {:<8} {:<12} {:<20}",
            field.path, field.offset, field.len, type_str, details
        );
    }

    info!("Inspect completed successfully");
    Ok(0)
}
