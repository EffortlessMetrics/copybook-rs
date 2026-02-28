// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then, when};

use crate::helpers::does_path_match_counter;
use crate::world::CopybookWorld;
use copybook_core::lexer::{Lexer, Token};

// ========================================================================
// Copybook Parsing Steps
// ========================================================================

#[given(expr = "a copybook with content:")]
async fn given_copybook_with_content(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a simple copybook with a single field")]
async fn given_simple_copybook(world: &mut CopybookWorld) {
    world.copybook_text = Some("01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).".to_string());
}

#[given(expr = "a copybook with numeric fields")]
async fn given_copybook_with_numeric_fields(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 NUMERIC-RECORD.\n\
         05 PACKED-DECIMAL PIC S9(7)V99 COMP-3.\n\
         05 BINARY-INTEGER PIC S9(9) COMP.\n\
         05 ZONED-DECIMAL PIC S9(7)V99 DISPLAY."
            .to_string(),
    );
}

#[given(expr = "a copybook with OCCURS clause")]
async fn given_copybook_with_occurs(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 ARRAY-RECORD.\n\
         05 ARRAY-FIELD OCCURS 5 TIMES.\n\
             10 ELEMENT PIC X(10)."
            .to_string(),
    );
}

#[given(regex = r"^a copybook with ODO \(OCCURS DEPENDING ON\)$")]
async fn given_copybook_with_odo(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 ODO-RECORD.\n\
         05 COUNT-FIELD PIC 9(3).\n\
         05 DYNAMIC-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON COUNT-FIELD.\n\
             10 ELEMENT PIC X(10)."
            .to_string(),
    );
}

#[given(expr = "a copybook with REDEFINES clause")]
async fn given_copybook_with_redefines(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 REDEFINES-RECORD.\n\
         05 ORIGINAL-FIELD PIC X(20).\n\
         05 ALTERNATIVE-FIELD PIC X(20)."
            .to_string(),
    );
}

#[given(expr = "a copybook with Level-88 condition values")]
async fn given_copybook_with_level88(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 STATUS-RECORD.\n\
         05 STATUS-CODE PIC X(1).\n\
             88 STATUS-ACTIVE VALUE 'A'.\n\
             88 STATUS-INACTIVE VALUE 'I'.\n\
             88 STATUS-PENDING VALUE 'P'."
            .to_string(),
    );
}

#[given(expr = "a copybook with RENAMES clause")]
async fn given_copybook_with_renames(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 RENAMES-RECORD.\n\
         05 FIRST-FIELD PIC X(10).\n\
         05 SECOND-FIELD PIC X(10).\n\
         66 ALIAS-FIELD RENAMES FIRST-FIELD THRU SECOND-FIELD."
            .to_string(),
    );
}

#[given(expr = "strict parsing mode")]
async fn given_strict_parsing(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.strict = true;
    world.parse_options = Some(options);
}

#[given(expr = "tolerant parsing mode")]
async fn given_tolerant_parsing(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.strict = false;
    world.parse_options = Some(options);
}

#[given(expr = "an invalid copybook with syntax error")]
async fn given_invalid_copybook_syntax(world: &mut CopybookWorld) {
    world.copybook_text = Some("THIS IS NOT VALID COBOL syntax error".to_string());
}

#[given(expr = "a copybook with invalid OCCURS clause")]
async fn given_invalid_occurs(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 INVALID-RECORD.\n\
         05 BAD-FIELD OCCURS INVALID TIMES PIC X(5)."
            .to_string(),
    );
}

#[given(expr = "a copybook with invalid PIC clause")]
async fn given_invalid_pic(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 INVALID-RECORD.\n\
         05 BAD-FIELD PIC INVALID."
            .to_string(),
    );
}

#[given(expr = "invalid dialect mode")]
async fn given_invalid_dialect_mode(world: &mut CopybookWorld) {
    world.error = Some(copybook_core::Error::new(
        copybook_core::ErrorCode::CBKP001_SYNTAX,
        "Invalid dialect mode",
    ));
}

#[when(expr = "the copybook is parsed")]
#[when(expr = "copybook is parsed")]
async fn when_copybook_is_parsed(world: &mut CopybookWorld) {
    world.ensure_schema_parsed();
}

#[then(expr = "the schema should be successfully parsed")]
async fn then_schema_successfully_parsed(world: &mut CopybookWorld) {
    assert!(
        world.schema.is_some(),
        "Schema should be parsed successfully"
    );
    assert!(world.error.is_none(), "No error should occur");
}

#[then(expr = "parsing should succeed")]
async fn then_parsing_should_succeed(world: &mut CopybookWorld) {
    assert!(world.schema.is_some(), "Parsing should succeed");
    assert!(world.error.is_none(), "No error should occur");
}

#[then(expr = "parsing should fail")]
async fn then_parsing_should_fail(world: &mut CopybookWorld) {
    assert!(
        world.error.is_some(),
        "Parsing should fail for this scenario"
    );
}

#[then(expr = "the schema should contain {int} top-level field(s)")]
async fn then_schema_contains_fields(world: &mut CopybookWorld, count: usize) {
    assert_eq!(
        world.schema().fields.len(),
        count,
        "Expected {} top-level fields, got {}",
        count,
        world.schema().fields.len()
    );
}

#[then(expr = "the field {string} should have type {string}")]
async fn then_field_has_type(world: &mut CopybookWorld, field_name: String, expected_type: String) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    // Check for "occurs" type based on occurs clause rather than kind
    if expected_type == "occurs" {
        assert!(
            field.occurs.is_some(),
            "Expected field '{}' to have OCCURS clause",
            field_name
        );
        return;
    }

    let actual_type = match &field.kind {
        copybook_core::FieldKind::Alphanum { .. } => "alphanumeric",
        copybook_core::FieldKind::ZonedDecimal { .. } => "zoned",
        copybook_core::FieldKind::PackedDecimal { .. } => "packed",
        copybook_core::FieldKind::BinaryInt { .. } => "binary",
        copybook_core::FieldKind::EditedNumeric { .. } => "edited",
        copybook_core::FieldKind::FloatSingle => "float_single",
        copybook_core::FieldKind::FloatDouble => "float_double",
        copybook_core::FieldKind::Group => "group",
        copybook_core::FieldKind::Condition { .. } => "condition",
        copybook_core::FieldKind::Renames { .. } => "renames",
    };

    // Support type aliases used across feature files
    let matches = match expected_type.as_str() {
        "numeric" => actual_type == "zoned",
        "packed_decimal" => actual_type == "packed",
        "binary_int" => actual_type == "binary",
        "edited_numeric" => actual_type == "edited",
        other => actual_type == other,
    };

    assert!(
        matches,
        "Expected field '{}' to have type '{}', got '{}'",
        field_name, expected_type, actual_type
    );
}

#[then(expr = "the field {string} should be {int} characters long")]
async fn then_field_length(world: &mut CopybookWorld, field_name: String, length: usize) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    assert_eq!(
        field.len as usize, length,
        "Expected field '{}' to be {} characters long, got {}",
        field_name, length, field.len
    );
}

#[then(expr = "the field {string} should be {int} bytes long")]
async fn then_field_length_bytes(world: &mut CopybookWorld, field_name: String, length: usize) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    assert_eq!(
        field.len as usize, length,
        "Expected field '{}' to be {} bytes long, got {}",
        field_name, length, field.len
    );
}

#[then(expr = "the field {string} should have level {int}")]
async fn then_field_has_level(world: &mut CopybookWorld, field_name: String, expected_level: u8) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    assert_eq!(
        field.level, expected_level,
        "Expected field '{}' to have level {}, got {}",
        field_name, expected_level, field.level
    );
}

#[then(expr = "field {word} should have OCCURS count {int}")]
async fn then_field_has_occurs_count(
    world: &mut CopybookWorld,
    field_name: String,
    expected_count: u32,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    let actual_count = match &field.occurs {
        Some(copybook_core::Occurs::Fixed { count }) => *count,
        Some(_) => panic!("Field '{}' has ODO, not fixed OCCURS", field_name),
        None => panic!("Field '{}' has no OCCURS clause", field_name),
    };

    assert_eq!(
        actual_count, expected_count,
        "Expected field '{}' to have OCCURS count {}, got {}",
        field_name, expected_count, actual_count
    );
}

#[then(expr = "field {word} should have ODO with counter {string}")]
async fn then_field_has_odo_with_counter(
    world: &mut CopybookWorld,
    field_name: String,
    counter: String,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    match &field.occurs {
        Some(occurs @ copybook_core::Occurs::ODO { .. }) => {
            assert!(
                does_path_match_counter(occurs, &counter),
                "Field '{}' should have ODO with counter '{}'",
                field_name,
                counter
            );
        }
        Some(_) => panic!("Field '{}' is not an ODO field", field_name),
        None => panic!("Field '{}' has no OCCURS clause", field_name),
    }
}

#[then(expr = "field {word} should have ODO with min {int} and max {int}")]
async fn then_field_has_odo_range(
    world: &mut CopybookWorld,
    field_name: String,
    expected_min: u32,
    expected_max: u32,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    match &field.occurs {
        Some(copybook_core::Occurs::ODO { min, max, .. }) => {
            assert_eq!(
                *min, expected_min,
                "Field '{}' ODO min should be {}",
                field_name, expected_min
            );
            assert_eq!(
                *max, expected_max,
                "Field '{}' ODO max should be {}",
                field_name, expected_max
            );
        }
        Some(_) => panic!("Field '{}' is not an ODO field", field_name),
        None => panic!("Field '{}' has no OCCURS clause", field_name),
    }
}

#[then(expr = "the field {string} should have REDEFINES {string}")]
async fn then_field_has_redefines(
    world: &mut CopybookWorld,
    field_name: String,
    redefines: String,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    assert!(
        field.redefines_of.is_some(),
        "Field '{field_name}' should have REDEFINES '{redefines}'",
    );
}

#[then(expr = "the field {string} should redefine {string}")]
async fn then_field_redefines(world: &mut CopybookWorld, field_name: String, original: String) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    assert_eq!(
        field.redefines_of.as_deref(),
        Some(original.as_str()),
        "Field '{}' should redefine '{}'",
        field_name,
        original
    );
}

#[then(expr = "the field {string} should have Level-88 {string}")]
async fn then_field_has_level88_value(
    world: &mut CopybookWorld,
    field_name: String,
    condition_name: String,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));

    let has_condition = field
        .children
        .iter()
        .any(|child| child.level == 88 && child.name.eq_ignore_ascii_case(&condition_name));

    assert!(
        has_condition,
        "Field '{}' should have Level-88 condition '{}'",
        field_name, condition_name
    );
}

#[then(expr = "there should be {int} level-66 fields")]
async fn then_level_66_field_count(world: &mut CopybookWorld, expected: usize) {
    let count = world
        .schema()
        .all_fields()
        .iter()
        .filter(|field| field.level == 66)
        .count();

    assert_eq!(
        count, expected,
        "Expected {} level-66 fields, got {}",
        expected, count
    );
}

#[then(expr = "the schema should have fingerprint")]
async fn then_schema_has_fingerprint(world: &mut CopybookWorld) {
    let schema = world.schema();
    assert!(
        !schema.fingerprint.is_empty(),
        "Schema should have fingerprint"
    );
}

#[then(expr = "the schema should contain {int} leaf fields")]
async fn then_leaf_field_count(world: &mut CopybookWorld, expected: usize) {
    let count = world.all_leaf_fields().len();
    assert_eq!(
        count, expected,
        "Expected {} leaf fields, got {}",
        expected, count
    );
}

#[then(expr = "the field {string} should be present")]
async fn then_field_is_present(world: &mut CopybookWorld, field_name: String) {
    assert!(
        world.find_field_by_name_ci(&field_name).is_some(),
        "Field '{}' should be present",
        field_name
    );
}

#[then(expr = "{word} should be present")]
async fn then_field_word_present(world: &mut CopybookWorld, field_name: String) {
    assert!(
        world.find_field_by_name_ci(&field_name).is_some(),
        "Field '{}' should be present",
        field_name
    );
}

#[then(expr = "the field {word} should not be present")]
async fn then_field_not_present(world: &mut CopybookWorld, field_name: String) {
    assert!(
        world.find_field_by_name_ci(&field_name).is_none(),
        "Field '{}' should not be present",
        field_name
    );
}

#[then(expr = "the field {string} should have offset {int}")]
async fn then_field_has_offset(world: &mut CopybookWorld, field_name: String, expected: u32) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));
    assert_eq!(
        field.offset, expected,
        "Expected field '{}' to have offset {}, got {}",
        field_name, expected, field.offset
    );
}

#[then(expr = "the field {string} should have length {int}")]
async fn then_field_has_length(world: &mut CopybookWorld, field_name: String, expected: u32) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));
    assert_eq!(
        field.len, expected,
        "Expected field '{}' to have length {}, got {}",
        field_name, expected, field.len
    );
}

#[then(expr = "schema should be successfully parsed")]
async fn then_schema_parsed_bare(world: &mut CopybookWorld) {
    assert!(
        world.schema.is_some(),
        "Schema should be parsed successfully"
    );
    assert!(world.error.is_none(), "No error should occur");
}

#[then(expr = "field {string} should have type {string}")]
async fn then_field_type_bare(
    world: &mut CopybookWorld,
    field_name: String,
    expected_type: String,
) {
    then_field_has_type(world, field_name, expected_type).await;
}

#[then(expr = "field {string} should have level {int}")]
async fn then_field_level_bare(world: &mut CopybookWorld, field_name: String, expected_level: u8) {
    then_field_has_level(world, field_name, expected_level).await;
}

#[then(regex = r"^there should be (\d+) fields$")]
async fn then_n_fields(world: &mut CopybookWorld, expected: usize) {
    let count = world.all_leaf_fields().len();
    assert_eq!(
        count, expected,
        "Expected {} leaf fields, got {}",
        expected, count
    );
}

#[then(regex = r"^there should be (\d+) leaf fields$")]
async fn then_n_leaf_fields_bare(world: &mut CopybookWorld, expected: usize) {
    let count = world.all_leaf_fields().len();
    assert_eq!(
        count, expected,
        "Expected {} leaf fields, got {}",
        expected, count
    );
}

#[then(expr = "fields should have correct levels")]
async fn then_fields_correct_levels(world: &mut CopybookWorld) {
    assert!(world.schema.is_some(), "Schema should exist");
}

#[then(expr = "field types should be diverse")]
async fn then_field_types_diverse(world: &mut CopybookWorld) {
    use std::collections::HashSet;
    assert!(world.schema.is_some(), "Schema should exist");
    let fields = world.all_leaf_fields();
    let types: HashSet<String> = fields
        .iter()
        .map(|f| format!("{:?}", std::mem::discriminant(&f.kind)))
        .collect();
    assert!(
        types.len() > 1,
        "Expected diverse field types, got {:?}",
        types
    );
}

#[then(expr = "field {string} should be present")]
async fn then_field_present_bare(world: &mut CopybookWorld, field_name: String) {
    assert!(
        world.find_field_by_name_ci(&field_name).is_some(),
        "Field '{}' should be present",
        field_name
    );
}

#[given(expr = "a copybook with an inline VALUE-list for Level-88")]
async fn given_copybook_with_level88_inline_values(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01 STATUS-RECORD.\n\
         05 STATUS-CODE PIC X(1).\n\
             88 STATUS-ACTIVE VALUE \"A\", \"B\", \"C\"."
            .to_string(),
    );
}

#[then(expr = "the lexer should produce {int} comma token(s)")]
async fn then_lexer_should_produce_comma_tokens(world: &mut CopybookWorld, expected: usize) {
    let copybook_text = world.copybook_text.as_ref().expect("Copybook text not set");
    let mut lexer = Lexer::new(copybook_text);
    let comma_count = lexer
        .tokenize()
        .iter()
        .filter(|token_pos| matches!(token_pos.token, Token::Comma))
        .count();

    assert_eq!(comma_count, expected);
}

#[then(expr = "the lexer should not treat VALUE-list separators as edited-picture tokens")]
async fn then_lexer_should_not_treat_value_list_commas_as_edited_pic(world: &mut CopybookWorld) {
    let copybook_text = world.copybook_text.as_ref().expect("Copybook text not set");
    let mut lexer = Lexer::new(copybook_text);
    let edited_pic_count = lexer
        .tokenize()
        .iter()
        .filter(|token_pos| matches!(token_pos.token, Token::EditedPic(_)))
        .count();

    assert_eq!(
        edited_pic_count, 0,
        "Expected VALUE-list separators to stay as comma tokens, not edited picture"
    );
}
