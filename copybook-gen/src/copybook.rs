//! Synthetic copybook generation

use crate::GeneratorConfig;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::fmt::Write;

/// Field type for synthetic generation
#[derive(Debug, Clone, Copy)]
pub enum FieldType {
    Alphanum,
    ZonedDecimal,
    PackedDecimal,
    Binary,
    Group,
}

/// Copybook generation template
#[derive(Debug, Clone, Copy)]
pub enum CopybookTemplate {
    /// Simple flat record with various field types
    Simple,
    /// Record with REDEFINES clauses
    WithRedefines,
    /// Record with OCCURS clauses
    WithOccurs,
    /// Record with ODO (OCCURS DEPENDING ON)
    WithODO,
    /// Record with SYNCHRONIZED fields
    WithSync,
    /// Complex record combining multiple features
    Complex,
    /// Performance test - DISPLAY-heavy
    DisplayHeavy,
    /// Performance test - COMP-3-heavy
    Comp3Heavy,
}

/// Generate a synthetic COBOL copybook
#[must_use]
pub fn generate_synthetic_copybook(config: &GeneratorConfig) -> String {
    generate_copybook_with_template(config, CopybookTemplate::Simple)
}

/// Generate copybook with specific template
#[must_use]
pub fn generate_copybook_with_template(
    config: &GeneratorConfig,
    template: CopybookTemplate,
) -> String {
    let mut rng = StdRng::seed_from_u64(config.seed);

    match template {
        CopybookTemplate::Simple => generate_simple_copybook(&mut rng, config),
        CopybookTemplate::WithRedefines => generate_redefines_copybook(&mut rng, config),
        CopybookTemplate::WithOccurs => generate_occurs_copybook(&mut rng, config),
        CopybookTemplate::WithODO => generate_odo_copybook(&mut rng, config),
        CopybookTemplate::WithSync => generate_sync_copybook(&mut rng, config),
        CopybookTemplate::Complex => generate_complex_copybook(&mut rng, config),
        CopybookTemplate::DisplayHeavy => generate_display_heavy_copybook(&mut rng, config),
        CopybookTemplate::Comp3Heavy => generate_comp3_heavy_copybook(&mut rng, config),
    }
}

#[allow(clippy::too_many_lines)]
fn generate_simple_copybook(rng: &mut StdRng, config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - Simple\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Generate various field types
    let field_count = if config.include_edge_cases { 20 } else { 10 };

    for i in 1..=field_count {
        let field_type = rng.gen_range(0..5);
        match field_type {
            0 => {
                // Alphanumeric field
                let len = if config.include_edge_cases && rng.gen_bool(0.2) {
                    // Edge cases: very small or large
                    if rng.gen_bool(0.5) {
                        1
                    } else {
                        rng.gen_range(100..=200)
                    }
                } else {
                    rng.gen_range(1..=50)
                };
                write!(copybook, "           05  ALPHA-{i:02}     PIC X({len}).\n").unwrap();
            }
            1 => {
                // Zoned decimal
                let digits = if config.include_edge_cases && rng.gen_bool(0.2) {
                    rng.gen_range(15..=18) // Large numbers
                } else {
                    rng.gen_range(1..=9)
                };
                let scale = if rng.gen_bool(0.3) {
                    rng.gen_range(0..=4)
                } else {
                    0
                };
                let signed = rng.gen_bool(0.5);

                let pic = if scale > 0 {
                    if signed {
                        format!("S9({})V9({})", digits - scale, scale)
                    } else {
                        format!("9({})V9({})", digits - scale, scale)
                    }
                } else if signed {
                    format!("S9({digits})")
                } else {
                    format!("9({digits})")
                };

                write!(copybook, "           05  ZONED-{i:02}     PIC {pic}.\n").unwrap();
            }
            2 => {
                // Binary field
                let digits = if config.include_edge_cases && rng.gen_bool(0.2) {
                    rng.gen_range(15..=18) // Large binary
                } else {
                    rng.gen_range(1..=9)
                };
                let signed = rng.gen_bool(0.5);

                let pic = if signed {
                    format!("S9({digits}) COMP")
                } else {
                    format!("9({digits}) COMP")
                };

                write!(copybook, "           05  BINARY-{i:02}    PIC {pic}.\n").unwrap();
            }
            3 => {
                // Packed decimal
                let digits = if config.include_edge_cases && rng.gen_bool(0.2) {
                    rng.gen_range(15..=18) // Large packed
                } else {
                    rng.gen_range(1..=9)
                };
                let scale = if rng.gen_bool(0.3) {
                    rng.gen_range(0..=4)
                } else {
                    0
                };
                let signed = rng.gen_bool(0.5);

                let pic = if scale > 0 {
                    if signed {
                        format!("S9({})V9({}) COMP-3", digits - scale, scale)
                    } else {
                        format!("9({})V9({}) COMP-3", digits - scale, scale)
                    }
                } else if signed {
                    format!("S9({digits}) COMP-3")
                } else {
                    format!("9({digits}) COMP-3")
                };

                write!(copybook, "           05  PACKED-{i:02}    PIC {pic}.\n").unwrap();
            }
            4 => {
                // Group with sub-fields
                write!(copybook, "           05  GROUP-{i:02}.\n").unwrap();

                let sub_count = rng.gen_range(2..=4);
                for j in 1..=sub_count {
                    let sub_type = rng.gen_range(0..3);
                    match sub_type {
                        0 => {
                            let len = rng.gen_range(5..=20);
                            write!(copybook, "               10  SUB-{i}-{j}   PIC X({len}).\n")
                                .unwrap();
                        }
                        1 => {
                            let digits = rng.gen_range(3..=7);
                            write!(
                                copybook,
                                "               10  NUM-{i}-{j}   PIC 9({digits}).\n"
                            )
                            .unwrap();
                        }
                        2 => {
                            let digits = rng.gen_range(3..=7);
                            write!(
                                copybook,
                                "               10  PKD-{i}-{j}   PIC 9({digits}) COMP-3.\n"
                            )
                            .unwrap();
                        }
                        _ => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    copybook
}

fn generate_redefines_copybook(_rng: &mut StdRng, _config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - REDEFINES\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Base field
    copybook.push_str("           05  BASE-FIELD      PIC X(20).\n");

    // REDEFINES with different interpretations
    copybook.push_str("           05  NUMERIC-VIEW REDEFINES BASE-FIELD\n");
    copybook.push_str("                               PIC 9(20).\n");

    copybook.push_str("           05  PACKED-VIEW REDEFINES BASE-FIELD.\n");
    copybook.push_str("               10  PKD-PART1   PIC 9(5) COMP-3.\n");
    copybook.push_str("               10  PKD-PART2   PIC 9(5) COMP-3.\n");
    copybook.push_str("               10  FILLER      PIC X(10).\n");

    // Another base field with multiple redefines
    copybook.push_str("           05  VARIANT-DATA    PIC X(16).\n");
    copybook.push_str("           05  INT-VARIANT REDEFINES VARIANT-DATA.\n");
    copybook.push_str("               10  INT-VAL1    PIC 9(4) COMP.\n");
    copybook.push_str("               10  INT-VAL2    PIC 9(4) COMP.\n");
    copybook.push_str("               10  INT-VAL3    PIC 9(4) COMP.\n");
    copybook.push_str("               10  INT-VAL4    PIC 9(4) COMP.\n");

    copybook.push_str("           05  DECIMAL-VARIANT REDEFINES VARIANT-DATA.\n");
    copybook.push_str("               10  DEC-VAL1    PIC 9(7) COMP-3.\n");
    copybook.push_str("               10  DEC-VAL2    PIC 9(7) COMP-3.\n");
    copybook.push_str("               10  FILLER      PIC X(8).\n");

    copybook
}

fn generate_occurs_copybook(rng: &mut StdRng, config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - OCCURS\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Simple OCCURS
    let array_size = if config.include_edge_cases && rng.gen_bool(0.3) {
        rng.gen_range(100..=500) // Large arrays
    } else {
        rng.gen_range(5..=20)
    };

    write!(
        copybook,
        "           05  SIMPLE-ARRAY    OCCURS {array_size} TIMES PIC X(10).\n"
    )
    .unwrap();

    // OCCURS with group
    let group_size = rng.gen_range(3..=10);
    write!(
        copybook,
        "           05  GROUP-ARRAY     OCCURS {group_size} TIMES.\n"
    )
    .unwrap();
    copybook.push_str("               10  ITEM-ID     PIC 9(5).\n");
    copybook.push_str("               10  ITEM-NAME   PIC X(20).\n");
    copybook.push_str("               10  ITEM-VALUE  PIC 9(7)V99 COMP-3.\n");

    // Nested OCCURS
    let outer_size = rng.gen_range(2..=5);
    let inner_size = rng.gen_range(3..=8);
    write!(
        copybook,
        "           05  NESTED-ARRAY    OCCURS {outer_size} TIMES.\n"
    )
    .unwrap();
    write!(
        copybook,
        "               10  INNER-ARRAY OCCURS {inner_size} TIMES PIC 9(4) COMP.\n"
    )
    .unwrap();

    copybook
}

fn generate_odo_copybook(rng: &mut StdRng, _config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - ODO\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // ODO at record tail
    let max_count = rng.gen_range(10..=50);
    let min_count = rng.gen_range(1..=5);

    copybook.push_str("           05  HEADER-DATA.\n");
    copybook.push_str("               10  RECORD-TYPE PIC X(4).\n");
    copybook.push_str("               10  ITEM-COUNT  PIC 9(3) COMP.\n");
    copybook.push_str("               10  TIMESTAMP   PIC X(20).\n");

    write!(
        copybook,
        "           05  VARIABLE-ITEMS  OCCURS {min_count} TO {max_count} TIMES\n"
    )
    .unwrap();
    copybook.push_str("                               DEPENDING ON ITEM-COUNT.\n");
    copybook.push_str("               10  ITEM-CODE   PIC X(8).\n");
    copybook.push_str("               10  ITEM-QTY    PIC 9(5) COMP-3.\n");
    copybook.push_str("               10  ITEM-PRICE  PIC 9(7)V99 COMP-3.\n");

    copybook
}

fn generate_sync_copybook(_rng: &mut StdRng, _config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - SYNCHRONIZED\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Mix of synchronized and non-synchronized fields
    copybook.push_str("           05  TEXT-FIELD1     PIC X(3).\n");
    copybook.push_str("           05  SYNC-BINARY1    PIC 9(4) COMP SYNCHRONIZED.\n");
    copybook.push_str("           05  TEXT-FIELD2     PIC X(5).\n");
    copybook.push_str("           05  SYNC-BINARY2    PIC 9(9) COMP SYNCHRONIZED.\n");
    copybook.push_str("           05  TEXT-FIELD3     PIC X(1).\n");
    copybook.push_str("           05  SYNC-BINARY3    PIC 9(18) COMP SYNCHRONIZED.\n");

    // Group with synchronized fields
    copybook.push_str("           05  SYNC-GROUP.\n");
    copybook.push_str("               10  GROUP-TEXT  PIC X(7).\n");
    copybook.push_str("               10  GROUP-BIN   PIC 9(4) COMP SYNCHRONIZED.\n");
    copybook.push_str("               10  GROUP-PKD   PIC 9(7) COMP-3.\n");

    copybook
}

fn generate_complex_copybook(rng: &mut StdRng, config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - Complex\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Header with various types
    copybook.push_str("           05  HEADER.\n");
    copybook.push_str("               10  REC-TYPE    PIC X(4).\n");
    copybook.push_str("               10  REC-LEN     PIC 9(5) COMP.\n");
    copybook.push_str("               10  CREATE-DATE PIC 9(8).\n");
    copybook.push_str("               10  FILLER      PIC X(4).\n");

    // REDEFINES section
    copybook.push_str("           05  PAYLOAD-DATA    PIC X(100).\n");
    copybook.push_str("           05  CUSTOMER-DATA REDEFINES PAYLOAD-DATA.\n");
    copybook.push_str("               10  CUST-ID     PIC 9(9) COMP.\n");
    copybook.push_str("               10  CUST-NAME   PIC X(30).\n");
    copybook.push_str("               10  CUST-ADDR   PIC X(50).\n");
    copybook.push_str("               10  CUST-BALANCE PIC S9(9)V99 COMP-3.\n");
    copybook.push_str("               10  FILLER      PIC X(7).\n");

    copybook.push_str("           05  PRODUCT-DATA REDEFINES PAYLOAD-DATA.\n");
    copybook.push_str("               10  PROD-CODE   PIC X(12).\n");
    copybook.push_str("               10  PROD-DESC   PIC X(40).\n");
    copybook.push_str("               10  PROD-PRICE  PIC 9(7)V99 COMP-3.\n");
    copybook.push_str("               10  PROD-QTY    PIC 9(7) COMP.\n");
    copybook.push_str("               10  FILLER      PIC X(35).\n");

    // OCCURS section with SYNCHRONIZED
    let array_size = rng.gen_range(5..=15);
    write!(
        copybook,
        "           05  DETAIL-ITEMS    OCCURS {array_size} TIMES.\n"
    )
    .unwrap();
    copybook.push_str("               10  ITEM-SEQ    PIC 9(3) COMP SYNCHRONIZED.\n");
    copybook.push_str("               10  ITEM-TYPE   PIC X(2).\n");
    copybook.push_str("               10  ITEM-AMT    PIC S9(9)V99 COMP-3.\n");
    copybook.push_str("               10  ITEM-FLAGS  PIC X(8).\n");

    // Tail ODO if edge cases enabled
    if config.include_edge_cases && rng.gen_bool(0.5) {
        let max_notes = rng.gen_range(10..=30);
        copybook.push_str("           05  NOTE-COUNT      PIC 9(3) COMP.\n");
        write!(
            copybook,
            "           05  NOTES           OCCURS 0 TO {max_notes} TIMES\n"
        )
        .unwrap();
        copybook.push_str("                               DEPENDING ON NOTE-COUNT.\n");
        copybook.push_str("               10  NOTE-TEXT   PIC X(80).\n");
        copybook.push_str("               10  NOTE-DATE   PIC 9(8).\n");
    }

    copybook
}

fn generate_display_heavy_copybook(_rng: &mut StdRng, _config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - DISPLAY Heavy\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Many DISPLAY fields for performance testing
    for i in 1..=50 {
        let field_type = i % 4;
        match field_type {
            0 => {
                write!(copybook, "           05  TEXT-{i:02}       PIC X(20).\n").unwrap();
            }
            1 => {
                write!(copybook, "           05  NUM-{i:02}        PIC 9(10).\n").unwrap();
            }
            2 => {
                write!(copybook, "           05  DECIMAL-{i:02}    PIC 9(8)V99.\n").unwrap();
            }
            3 => {
                write!(copybook, "           05  SIGNED-{i:02}     PIC S9(9).\n").unwrap();
            }
            _ => unreachable!(),
        }
    }

    copybook
}

fn generate_comp3_heavy_copybook(_rng: &mut StdRng, _config: &GeneratorConfig) -> String {
    let mut copybook = String::new();
    copybook.push_str("      * Generated synthetic copybook - COMP-3 Heavy\n");
    copybook.push_str("       01  RECORD-ROOT.\n");

    // Many COMP-3 fields for performance testing
    for i in 1..=40 {
        let field_type = i % 3;
        match field_type {
            0 => {
                write!(
                    copybook,
                    "           05  PACKED-{i:02}     PIC 9(7) COMP-3.\n"
                )
                .unwrap();
            }
            1 => {
                write!(
                    copybook,
                    "           05  DECIMAL-{i:02}    PIC 9(9)V99 COMP-3.\n"
                )
                .unwrap();
            }
            2 => {
                write!(
                    copybook,
                    "           05  SIGNED-{i:02}     PIC S9(11)V99 COMP-3.\n"
                )
                .unwrap();
            }
            _ => unreachable!(),
        }
    }

    // Add some text fields for mixed workload
    for i in 1..=10 {
        write!(copybook, "           05  TEXT-{i:02}       PIC X(15).\n").unwrap();
    }

    copybook
}

/// Generate negative test copybooks (invalid syntax)
#[must_use]
pub fn generate_invalid_copybook(config: &GeneratorConfig) -> Vec<(String, String)> {
    let _rng = StdRng::seed_from_u64(config.seed);

    vec![
        // Invalid level numbers
        (
            "invalid_level".to_string(),
            "       00  INVALID-LEVEL PIC X(10).\n".to_string(),
        ),

        // Invalid PIC clauses
        (
            "invalid_pic".to_string(),
            "       01  ROOT.\n           05  BAD-PIC PIC Z(10).\n".to_string(),
        ),

        // REDEFINES target not found
        (
            "redefines_missing".to_string(),
            "       01  ROOT.\n           05  FIELD1 PIC X(10).\n           05  FIELD2 REDEFINES MISSING PIC 9(10).\n".to_string()
        ),

        // ODO not at tail
        (
            "odo_not_tail".to_string(),
            "       01  ROOT.\n           05  COUNT PIC 9(3) COMP.\n           05  ARRAY OCCURS 1 TO 10 DEPENDING ON COUNT PIC X(5).\n           05  TRAILER PIC X(10).\n".to_string()
        ),

        // ODO counter in REDEFINES
        (
            "odo_counter_in_redefines".to_string(),
            "       01  ROOT.\n           05  BASE PIC X(10).\n           05  REDEF REDEFINES BASE.\n               10  COUNT PIC 9(3) COMP.\n               10  FILLER PIC X(7).\n           05  ARRAY OCCURS 1 TO 5 DEPENDING ON COUNT PIC X(5).\n".to_string()
        ),
    ]
}
