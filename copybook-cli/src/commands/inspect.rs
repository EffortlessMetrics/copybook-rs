//! Inspect command implementation

use copybook_codec::Codepage;
use copybook_core::parse_copybook;
use std::fs;
use std::path::PathBuf;
use tracing::info;

pub async fn run(copybook: PathBuf, codepage: Codepage) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Inspecting copybook: {:?}", copybook);

    // Read copybook file
    let copybook_text = fs::read_to_string(&copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

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
