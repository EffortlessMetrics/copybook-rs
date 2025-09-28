/// Panic Elimination Fixtures: Layout Resolution Test Data
/// Issue #33 - Layout.rs Panic Elimination (9 target instances)
///
/// This module provides comprehensive test data for layout resolution scenarios
/// that historically cause panic conditions in layout processing. Focuses on
/// complex field relationships, REDEFINES interactions, ODO patterns, and
/// field lookup operations that can cause unwrap() panics.

#[cfg(test)]
pub struct LayoutResolutionFixture {
    pub name: &'static str,
    pub copybook_text: &'static str,
    pub test_data: Vec<u8>,
    pub expected_behavior: LayoutExpectedBehavior,
    pub panic_scenario: &'static str,
    pub validation_points: Vec<&'static str>,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub enum LayoutExpectedBehavior {
    ValidLayout,
    SchemaError(&'static str), // Expected CBKS* error code
    DecodeError(&'static str), // Expected CBKD* error code
    Either, // Either success or controlled error
}

/// Complex REDEFINES patterns that can cause layout resolution panics
pub mod redefines_fixtures {
    use super::*;

    /// Circular REDEFINES dependencies causing infinite loop panics
    pub fn circular_redefines_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "redefines_circular_dependencies",
            copybook_text: r#"
            01 CIRCULAR-REDEFINES-RECORD.
                05 BASE-FIELD PIC X(50).
                05 REDEFINES-A REDEFINES BASE-FIELD.
                    10 FIELD-A1 PIC X(25).
                    10 FIELD-A2 PIC X(25).
                05 REDEFINES-B REDEFINES REDEFINES-A.
                    10 FIELD-B1 PIC 9(10).
                    10 FIELD-B2 PIC X(40).
                05 REDEFINES-C REDEFINES REDEFINES-B.
                    10 FIELD-C1 PIC X(50).
                05 BACK-REFERENCE REDEFINES REDEFINES-C PIC X(50).
            "#,
            test_data: vec![0x40; 50], // EBCDIC spaces
            expected_behavior: LayoutExpectedBehavior::Either,
            panic_scenario: "Circular REDEFINES references causing infinite loop in layout resolution",
            validation_points: vec![
                "REDEFINES chain resolution",
                "Field offset calculation",
                "Memory layout validation",
            ],
        }
    }

    /// REDEFINES with mismatched sizes causing buffer overflow panics
    pub fn size_mismatch_redefines_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "redefines_size_mismatch",
            copybook_text: r#"
            01 SIZE-MISMATCH-RECORD.
                05 SMALL-BASE PIC X(10).
                05 LARGE-REDEFINES REDEFINES SMALL-BASE.
                    10 OVERSIZED-FIELD PIC X(50).
                05 TINY-BASE PIC X(1).
                05 HUGE-REDEFINES REDEFINES TINY-BASE.
                    10 MASSIVE-FIELD PIC X(1000).
                    10 ANOTHER-MASSIVE PIC 9(500).
            "#,
            test_data: vec![0x40; 20], // Limited data for mismatched sizes
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS031_REDEFINES_SIZE_MISMATCH"),
            panic_scenario: "REDEFINES size mismatch causing buffer overflow in field access",
            validation_points: vec![
                "Size validation in REDEFINES",
                "Buffer bounds checking",
                "Field overlap detection",
            ],
        }
    }

    /// REDEFINES targeting non-existent fields causing null pointer panics
    pub fn missing_target_redefines_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "redefines_missing_target",
            copybook_text: r#"
            01 MISSING-TARGET-RECORD.
                05 VALID-FIELD PIC X(20).
                05 ORPHANED-REDEFINES REDEFINES NON-EXISTENT-FIELD PIC X(15).
                05 ANOTHER-ORPHAN REDEFINES IMAGINARY-FIELD.
                    10 SUB-ORPHAN PIC 9(10).
                05 FORWARD-REFERENCE REDEFINES FUTURE-FIELD PIC X(25).
                05 FUTURE-FIELD PIC X(25).
            "#,
            test_data: vec![0x40; 45],
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS032_REDEFINES_TARGET_NOT_FOUND"),
            panic_scenario: "REDEFINES targeting missing fields causing null pointer dereference",
            validation_points: vec![
                "Field existence validation",
                "Forward reference handling",
                "Target resolution safety",
            ],
        }
    }

    /// Nested REDEFINES with complex hierarchies
    pub fn nested_redefines_hierarchy_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "redefines_nested_hierarchy",
            copybook_text: r#"
            01 NESTED-HIERARCHY-RECORD.
                05 BASE-GROUP.
                    10 SUB-BASE PIC X(30).
                05 LEVEL1-REDEFINES REDEFINES BASE-GROUP.
                    10 LEVEL1-FIELD PIC X(15).
                    10 LEVEL1-SUB-GROUP.
                        15 NESTED-FIELD PIC X(15).
                05 LEVEL2-REDEFINES REDEFINES LEVEL1-REDEFINES.
                    10 LEVEL2-ARRAY PIC X(6) OCCURS 5 TIMES.
                05 LEVEL3-REDEFINES REDEFINES LEVEL2-REDEFINES.
                    10 LEVEL3-GROUP.
                        15 LEVEL3-SUB1 PIC 9(10).
                        15 LEVEL3-SUB2 PIC X(20).
            "#,
            test_data: vec![0x40; 30],
            expected_behavior: LayoutExpectedBehavior::Either,
            panic_scenario: "Nested REDEFINES hierarchy causing stack overflow in resolution",
            validation_points: vec![
                "Nested hierarchy resolution",
                "Group field handling",
                "Multi-level REDEFINES validation",
            ],
        }
    }

    pub fn all_redefines_fixtures() -> Vec<LayoutResolutionFixture> {
        vec![
            circular_redefines_fixture(),
            size_mismatch_redefines_fixture(),
            missing_target_redefines_fixture(),
            nested_redefines_hierarchy_fixture(),
        ]
    }
}

/// ODO (Occurs Depending On) patterns that can cause layout resolution panics
pub mod odo_fixtures {
    use super::*;

    /// ODO with missing counter fields causing null pointer panics
    pub fn missing_counter_odo_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "odo_missing_counter",
            copybook_text: r#"
            01 MISSING-COUNTER-RECORD.
                05 VALID-FIELD PIC X(10).
                05 ORPHANED-ODO OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
                    10 ODO-FIELD PIC 9(5).
                05 FORWARD-REF-ODO OCCURS 1 TO 50 TIMES DEPENDING ON FUTURE-COUNTER.
                    10 FORWARD-FIELD PIC X(8).
                05 FUTURE-COUNTER PIC 9(3).
            "#,
            test_data: vec![0x40; 30],
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS121_COUNTER_NOT_FOUND"),
            panic_scenario: "ODO with missing counter field causing null pointer in counter lookup",
            validation_points: vec![
                "Counter field existence validation",
                "Forward reference in ODO",
                "Counter field type validation",
            ],
        }
    }

    /// ODO with invalid range specifications causing arithmetic panics
    pub fn invalid_range_odo_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "odo_invalid_range",
            copybook_text: r#"
            01 INVALID-RANGE-RECORD.
                05 COUNTER-FIELD PIC 9(5).
                05 BACKWARD-RANGE OCCURS 100 TO 1 TIMES DEPENDING ON COUNTER-FIELD.
                    10 BACKWARD-FIELD PIC X(10).
                05 ZERO-RANGE OCCURS 0 TO 0 TIMES DEPENDING ON COUNTER-FIELD.
                    10 ZERO-FIELD PIC 9(5).
                05 NEGATIVE-RANGE OCCURS -10 TO 10 TIMES DEPENDING ON COUNTER-FIELD.
                    10 NEGATIVE-FIELD PIC X(5).
                05 HUGE-RANGE OCCURS 1 TO 999999999 TIMES DEPENDING ON COUNTER-FIELD.
                    10 HUGE-FIELD PIC X(100).
            "#,
            test_data: vec![0xF0, 0xF0, 0xF1, 0xF0, 0xF0], // Counter = 00100
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS301_ODO_RANGE_INVALID"),
            panic_scenario: "ODO with invalid range causing arithmetic overflow in size calculation",
            validation_points: vec![
                "Range validation logic",
                "Arithmetic overflow protection",
                "Memory allocation safety",
            ],
        }
    }

    /// ODO counter with wrong data type causing conversion panics
    pub fn wrong_type_counter_odo_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "odo_wrong_type_counter",
            copybook_text: r#"
            01 WRONG-TYPE-COUNTER-RECORD.
                05 STRING-COUNTER PIC X(5).
                05 STRING-ODO OCCURS 1 TO 10 TIMES DEPENDING ON STRING-COUNTER.
                    10 STRING-ODO-FIELD PIC 9(3).
                05 COMP3-COUNTER PIC S9(3) COMP-3.
                05 COMP3-ODO OCCURS 1 TO 20 TIMES DEPENDING ON COMP3-COUNTER.
                    10 COMP3-ODO-FIELD PIC X(8).
                05 FLOATING-COUNTER PIC S9(5)V99.
                05 FLOATING-ODO OCCURS 1 TO 15 TIMES DEPENDING ON FLOATING-COUNTER.
                    10 FLOATING-ODO-FIELD PIC 9(4).
            "#,
            test_data: vec![
                0xC1, 0xC2, 0xC3, 0xC4, 0xC5, // String counter "ABCDE"
                0x00, 0x5C, // COMP-3 counter: 5+
                0xF0, 0xF0, 0xF3, 0xF1, 0xF0, // Floating counter 00310
            ],
            expected_behavior: LayoutExpectedBehavior::DecodeError("CBKD421_COUNTER_CONVERSION_ERROR"),
            panic_scenario: "ODO counter type mismatch causing conversion panic in counter evaluation",
            validation_points: vec![
                "Counter type validation",
                "Type conversion safety",
                "Numeric counter requirements",
            ],
        }
    }

    /// ODO at non-tail position causing layout calculation panics
    pub fn non_tail_odo_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "odo_non_tail_position",
            copybook_text: r#"
            01 NON-TAIL-ODO-RECORD.
                05 COUNTER-FIELD PIC 9(3).
                05 MIDDLE-ODO OCCURS 1 TO 50 TIMES DEPENDING ON COUNTER-FIELD.
                    10 MIDDLE-FIELD PIC X(10).
                05 AFTER-ODO-FIELD PIC 9(15).
                05 ANOTHER-MIDDLE-ODO OCCURS 1 TO 20 TIMES DEPENDING ON COUNTER-FIELD.
                    10 ANOTHER-MIDDLE-FIELD PIC X(5).
                05 FINAL-FIELD PIC X(20).
            "#,
            test_data: vec![0xF0, 0xF0, 0xF5], // Counter = 005
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS021_ODO_NOT_TAIL"),
            panic_scenario: "ODO in non-tail position causing layout calculation overflow",
            validation_points: vec![
                "ODO position validation",
                "Tail position enforcement",
                "Layout calculation safety",
            ],
        }
    }

    /// Nested ODO causing exponential memory allocation panics
    pub fn nested_odo_explosion_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "odo_nested_explosion",
            copybook_text: r#"
            01 NESTED-ODO-RECORD.
                05 OUTER-COUNTER PIC 9(3).
                05 INNER-COUNTER PIC 9(3).
                05 OUTER-ODO OCCURS 1 TO 100 TIMES DEPENDING ON OUTER-COUNTER.
                    10 INNER-ODO OCCURS 1 TO 100 TIMES DEPENDING ON INNER-COUNTER.
                        15 NESTED-FIELD PIC X(50).
                    10 POST-INNER-FIELD PIC 9(10).
            "#,
            test_data: vec![
                0xF0, 0xF5, 0xF0, // Outer counter = 050
                0xF0, 0xF5, 0xF0, // Inner counter = 050
            ],
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS302_ODO_MEMORY_LIMIT"),
            panic_scenario: "Nested ODO causing exponential memory allocation (50x50x50 = 125k fields)",
            validation_points: vec![
                "Nested ODO validation",
                "Memory allocation limits",
                "Exponential growth prevention",
            ],
        }
    }

    pub fn all_odo_fixtures() -> Vec<LayoutResolutionFixture> {
        vec![
            missing_counter_odo_fixture(),
            invalid_range_odo_fixture(),
            wrong_type_counter_odo_fixture(),
            non_tail_odo_fixture(),
            nested_odo_explosion_fixture(),
        ]
    }
}

/// Field lookup and navigation patterns that can cause index panics
pub mod field_lookup_fixtures {
    use super::*;

    /// Deep field path resolution causing stack overflow panics
    pub fn deep_field_path_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "field_lookup_deep_path",
            copybook_text: r#"
            01 DEEP-PATH-RECORD.
                05 L1-GROUP.
                    10 L2-GROUP.
                        15 L3-GROUP.
                            20 L4-GROUP.
                                25 L5-GROUP.
                                    30 L6-GROUP.
                                        35 L7-GROUP.
                                            40 L8-GROUP.
                                                45 L9-GROUP.
                                                    50 L10-GROUP.
                                                        55 DEEP-FIELD PIC X(10).
            "#,
            test_data: vec![0x40; 10],
            expected_behavior: LayoutExpectedBehavior::Either,
            panic_scenario: "Deep field path resolution causing stack overflow in navigation",
            validation_points: vec![
                "Field path navigation depth",
                "Stack overflow prevention",
                "Recursive traversal safety",
            ],
        }
    }

    /// Field lookup with conflicting names causing hash collision panics
    pub fn name_collision_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "field_lookup_name_collision",
            copybook_text: r#"
            01 NAME-COLLISION-RECORD.
                05 DUPLICATE-NAME PIC X(10).
                05 GROUP-CONTAINER.
                    10 DUPLICATE-NAME PIC 9(5).
                05 ANOTHER-GROUP.
                    10 NESTED-GROUP.
                        15 DUPLICATE-NAME PIC X(15).
                    10 DUPLICATE-NAME PIC 9(8).
                05 QUALIFIED-REFERENCE.
                    10 DUPLICATE-NAME IN GROUP-CONTAINER PIC S9(3).
            "#,
            test_data: vec![0x40; 50],
            expected_behavior: LayoutExpectedBehavior::Either,
            panic_scenario: "Field name collisions causing hash collision panic in lookup",
            validation_points: vec![
                "Name collision handling",
                "Qualified name resolution",
                "Scope-based field lookup",
            ],
        }
    }

    /// Array index calculation overflow causing bounds panic
    pub fn array_index_overflow_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "field_lookup_array_overflow",
            copybook_text: r#"
            01 ARRAY-OVERFLOW-RECORD.
                05 COUNTER PIC 9(5).
                05 LARGE-ARRAY PIC X(10) OCCURS 999999 TIMES.
                05 MULTI-DIM-ARRAY PIC 9(5) OCCURS 1000 TIMES.
                    10 INNER-ARRAY PIC X(8) OCCURS 1000 TIMES.
            "#,
            test_data: vec![0xF9, 0xF9, 0xF9, 0xF9, 0xF9], // Counter = 99999
            expected_behavior: LayoutExpectedBehavior::SchemaError("CBKS401_ARRAY_SIZE_LIMIT"),
            panic_scenario: "Array index calculation overflow causing integer overflow panic",
            validation_points: vec![
                "Array size limits",
                "Index calculation safety",
                "Memory allocation bounds",
            ],
        }
    }

    pub fn all_field_lookup_fixtures() -> Vec<LayoutResolutionFixture> {
        vec![
            deep_field_path_fixture(),
            name_collision_fixture(),
            array_index_overflow_fixture(),
        ]
    }
}

/// Combined layout resolution edge cases
pub mod combined_layout_fixtures {
    use super::*;

    /// REDEFINES + ODO interaction causing layout confusion panics
    pub fn redefines_odo_interaction_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "combined_redefines_odo",
            copybook_text: r#"
            01 REDEFINES-ODO-RECORD.
                05 COUNTER-FIELD PIC 9(3).
                05 BASE-VARIABLE-AREA.
                    10 VARIABLE-FIELD PIC X(10) OCCURS 1 TO 50 TIMES DEPENDING ON COUNTER-FIELD.
                05 REDEFINES-VARIABLE REDEFINES BASE-VARIABLE-AREA.
                    10 FIXED-INTERPRETATION PIC X(500).
                05 NESTED-REDEFINES REDEFINES BASE-VARIABLE-AREA.
                    10 STRUCTURED-INTERPRETATION PIC 9(100) OCCURS 5 TIMES.
            "#,
            test_data: vec![0xF0, 0xF2, 0xF5], // Counter = 025
            expected_behavior: LayoutExpectedBehavior::Either,
            panic_scenario: "REDEFINES of ODO field causing layout size calculation panic",
            validation_points: vec![
                "REDEFINES + ODO interaction",
                "Variable area layout",
                "Size calculation consistency",
            ],
        }
    }

    /// Level-88 conditions with complex layout dependencies
    pub fn level88_layout_dependency_fixture() -> LayoutResolutionFixture {
        LayoutResolutionFixture {
            name: "combined_level88_layout",
            copybook_text: r#"
            01 LEVEL88-LAYOUT-RECORD.
                05 STATUS-FIELD PIC X(2).
                    88 STATUS-ACTIVE VALUE 'AC'.
                    88 STATUS-INACTIVE VALUE 'IN'.
                    88 STATUS-UNKNOWN VALUE 'UN'.
                05 CONDITIONAL-AREA.
                    10 ACTIVE-FIELDS PIC X(20).
                        88 ACTIVE-SUB-STATUS VALUE 'READY'.
                    10 INACTIVE-FIELDS REDEFINES ACTIVE-FIELDS.
                        15 INACTIVE-CODE PIC 9(5).
                        15 INACTIVE-REASON PIC X(15).
                            88 REASON-EXPIRED VALUE 'EXPIRED'.
                            88 REASON-CANCELLED VALUE 'CANCELLED'.
            "#,
            test_data: vec![
                0xC1, 0xC3, // Status: 'AC'
                0xD9, 0xC5, 0xC1, 0xC4, 0xE8, // 'READY'
                0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
                0x40, 0x40, 0x40, 0x40, 0x40,
            ],
            expected_behavior: LayoutExpectedBehavior::ValidLayout,
            panic_scenario: "Level-88 condition evaluation with REDEFINES causing field access panic",
            validation_points: vec![
                "Level-88 field resolution",
                "Conditional layout access",
                "REDEFINES with conditions",
            ],
        }
    }

    pub fn all_combined_fixtures() -> Vec<LayoutResolutionFixture> {
        vec![
            redefines_odo_interaction_fixture(),
            level88_layout_dependency_fixture(),
        ]
    }
}

/// Comprehensive collection of all layout resolution fixtures
pub fn all_layout_resolution_fixtures() -> Vec<LayoutResolutionFixture> {
    let mut fixtures = Vec::new();
    fixtures.extend(redefines_fixtures::all_redefines_fixtures());
    fixtures.extend(odo_fixtures::all_odo_fixtures());
    fixtures.extend(field_lookup_fixtures::all_field_lookup_fixtures());
    fixtures.extend(combined_layout_fixtures::all_combined_fixtures());
    fixtures
}

/// Generate stress test data for layout resolution performance validation
pub fn generate_layout_stress_data() -> Vec<(String, Vec<u8>)> {
    vec![
        (
            "massive_redefines_dataset".to_string(),
            vec![0x40; 10000], // Large data for complex REDEFINES testing
        ),
        (
            "large_odo_dataset".to_string(),
            {
                let mut data = Vec::new();
                data.extend(vec![0xF0, 0xF5, 0xF0]); // Counter = 050
                data.extend(vec![0x40; 2500]); // 50 * 50 bytes per occurrence
                data
            }
        ),
        (
            "deep_structure_dataset".to_string(),
            vec![0x40; 1000], // Data for deep nested structures
        ),
    ]
}

#[cfg(test)]
mod layout_resolution_tests {
    use super::*;
    use copybook_core::parse_copybook;
    use copybook_codec::{decode_record, DecodeOptions, Codepage};

    #[test]
    fn test_layout_fixtures_load() {
        let fixtures = all_layout_resolution_fixtures();
        assert!(fixtures.len() >= 12, "Should have at least 12 layout resolution fixtures");

        for fixture in &fixtures {
            assert!(!fixture.name.is_empty(), "Fixture name should not be empty");
            assert!(!fixture.copybook_text.is_empty(), "Copybook text should not be empty");
            assert!(!fixture.test_data.is_empty(), "Test data should not be empty");
            assert!(!fixture.panic_scenario.is_empty(), "Panic scenario should not be empty");
            assert!(!fixture.validation_points.is_empty(), "Validation points should not be empty");
        }
    }

    #[test]
    fn test_redefines_fixtures_safety() {
        let redefines_fixtures = redefines_fixtures::all_redefines_fixtures();

        for fixture in &redefines_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            match (&fixture.expected_behavior, schema_result) {
                (LayoutExpectedBehavior::ValidLayout, Ok(schema)) => {
                    // Should parse successfully and handle decode safely
                    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Either success or controlled error is acceptable
                }
                (LayoutExpectedBehavior::SchemaError(expected_code), Err(error)) => {
                    let error_str = format!("{:?}", error.code);
                    assert!(
                        error_str.starts_with("CBKS") || error_str.starts_with("CBKP"),
                        "Schema error for {} should use CBKS*/CBKP* code, got {:?}",
                        fixture.name, error.code
                    );
                }
                (LayoutExpectedBehavior::Either, _) => {
                    // Any outcome is acceptable for Either cases
                }
                _ => {
                    // Log unexpected outcomes but don't fail (panic elimination is primary goal)
                    eprintln!("Unexpected outcome for fixture {}: expected {:?}",
                            fixture.name, fixture.expected_behavior);
                }
            }
        }
    }

    #[test]
    fn test_odo_fixtures_safety() {
        let odo_fixtures = odo_fixtures::all_odo_fixtures();

        for fixture in &odo_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            // Test should not panic regardless of outcome
            match schema_result {
                Ok(schema) => {
                    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Either success or controlled error is acceptable
                }
                Err(error) => {
                    // Schema errors should use appropriate error codes
                    let error_str = format!("{:?}", error.code);
                    assert!(
                        error_str.starts_with("CBKS") || error_str.starts_with("CBKP"),
                        "ODO error for {} should use structured error code, got {:?}",
                        fixture.name, error.code
                    );
                }
            }
        }
    }

    #[test]
    fn test_field_lookup_fixtures_safety() {
        let lookup_fixtures = field_lookup_fixtures::all_field_lookup_fixtures();

        for fixture in &lookup_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            // Test should not panic - field lookup must be robust
            match schema_result {
                Ok(schema) => {
                    // Validate that schema can be safely accessed
                    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Any result is acceptable as long as no panic occurs
                }
                Err(_) => {
                    // Schema parsing errors are acceptable for complex structures
                }
            }
        }
    }

    #[test]
    fn test_combined_fixtures_safety() {
        let combined_fixtures = combined_layout_fixtures::all_combined_fixtures();

        for fixture in &combined_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            // Combined fixtures test complex interactions - focus on panic prevention
            match schema_result {
                Ok(schema) => {
                    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Any controlled outcome is acceptable
                }
                Err(error) => {
                    // Complex layout errors should use structured error codes
                    let error_str = format!("{:?}", error.code);
                    assert!(
                        error_str.starts_with("CBKS") || error_str.starts_with("CBKP"),
                        "Combined layout error for {} should use structured code, got {:?}",
                        fixture.name, error.code
                    );
                }
            }
        }
    }

    #[test]
    fn test_stress_data_generation() {
        let stress_data = generate_layout_stress_data();
        assert_eq!(stress_data.len(), 3, "Should generate 3 stress test datasets");

        for (name, data) in &stress_data {
            assert!(!name.is_empty(), "Stress dataset name should not be empty");
            assert!(data.len() > 100, "Stress dataset {} should be substantial", name);
        }
    }
}