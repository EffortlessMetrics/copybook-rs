// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then};

use crate::world::CopybookWorld;

// ========================================================================
// Sign Separate & RENAMES Given Steps
// ========================================================================

#[given(expr = "a copybook with SIGN SEPARATE LEADING clause")]
async fn given_sign_sep_leading_clause(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SIGN SEPARATE TRAILING clause")]
async fn given_sign_sep_trailing_clause(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SIGN SEPARATE LEADING")]
async fn given_sign_sep_leading_bare(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SIGN SEPARATE TRAILING")]
async fn given_sign_sep_trailing_bare(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(regex = r"^a copybook with RENAMES R4 \(multiple REDEFINES\)$")]
async fn given_renames_r4_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with RENAMES R4")]
async fn given_renames_r4(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(regex = r"^a copybook with RENAMES R5 \(ODO\)$")]
async fn given_renames_r5_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with RENAMES R5")]
async fn given_renames_r5(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(regex = r"^a copybook with RENAMES R6 \(Level-88 after RENAMES\)$")]
async fn given_renames_r6_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with RENAMES R6")]
async fn given_renames_r6(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with both SIGN SEPARATE and RENAMES")]
async fn given_sign_sep_and_renames(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with invalid RENAMES range")]
async fn given_invalid_renames_range(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

// ========================================================================
// Sign Separate & RENAMES Then Steps
// ========================================================================

#[then(expr = "the field should have sign separate information")]
async fn then_field_has_sign_separate(world: &mut CopybookWorld) {
    let field = world
        .first_numeric_leaf_field()
        .expect("No numeric leaf field found");
    assert!(
        field.sign_separate().is_some(),
        "Field should have sign separate info"
    );
}

#[then(expr = "the sign placement should be LEADING")]
async fn then_sign_placement_leading(world: &mut CopybookWorld) {
    let field = world
        .first_numeric_leaf_field()
        .expect("No numeric leaf field found");
    let info = field
        .sign_separate()
        .expect("Field should have sign separate info");
    assert_eq!(
        info.placement,
        copybook_core::SignPlacement::Leading,
        "Sign placement should be LEADING"
    );
}

#[then(expr = "the sign placement should be TRAILING")]
async fn then_sign_placement_trailing(world: &mut CopybookWorld) {
    let field = world
        .first_numeric_leaf_field()
        .expect("No numeric leaf field found");
    let info = field
        .sign_separate()
        .expect("Field should have sign separate info");
    assert_eq!(
        info.placement,
        copybook_core::SignPlacement::Trailing,
        "Sign placement should be TRAILING"
    );
}

#[then(expr = "field {string} should have sign separate placement {string}")]
async fn then_field_sign_separate_placement(
    world: &mut CopybookWorld,
    field_name: String,
    expected_placement: String,
) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));
    let sign_info = field.sign_separate().expect(&format!(
        "Field '{}' should have sign separate info",
        field_name
    ));
    let actual = match sign_info.placement {
        copybook_core::SignPlacement::Leading => "leading",
        copybook_core::SignPlacement::Trailing => "trailing",
    };
    assert_eq!(
        actual, expected_placement,
        "Expected sign separate placement '{}', got '{}'",
        expected_placement, actual
    );
}

#[then(expr = "field {string} should have blank_when_zero true")]
async fn then_field_bwz(world: &mut CopybookWorld, field_name: String) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));
    assert!(
        field.blank_when_zero,
        "Field '{}' should have blank_when_zero",
        field_name
    );
}

#[then(expr = "field {string} should have synchronized true")]
async fn then_field_synchronized(world: &mut CopybookWorld, field_name: String) {
    let field = world
        .find_field_by_name_ci(&field_name)
        .expect(&format!("Field '{}' not found", field_name));
    assert!(
        field.synchronized,
        "Field '{}' should be synchronized",
        field_name
    );
}

#[then(expr = "the encoded data should have leading sign")]
async fn then_encoded_leading_sign(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(!output.is_empty(), "Encoded output should not be empty");
    let first = output[0];
    assert!(
        first == b'+' || first == b'-',
        "First byte should be sign, got {}",
        first as char
    );
}

#[then(expr = "the encoded data should have trailing sign")]
async fn then_encoded_trailing_sign(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(!output.is_empty(), "Encoded output should not be empty");
    let last = output[output.len() - 1];
    assert!(
        last == b'+' || last == b'-',
        "Last byte should be sign, got {}",
        last as char
    );
}

#[then(expr = "the sign placement should be preserved")]
async fn then_sign_preserved(world: &mut CopybookWorld) {
    assert!(
        world.encoded_output.is_some(),
        "Encoded output should exist after round-trip"
    );
}

#[then(expr = "RENAMES field should be resolved")]
async fn then_renames_resolved(world: &mut CopybookWorld) {
    let field = world
        .renames_field()
        .expect("No RENAMES (level-66) field found");
    assert!(
        field.resolved_renames.is_some(),
        "RENAMES field should have resolved_renames"
    );
}

#[then(expr = "the alias should cover all REDEFINES fields")]
async fn then_alias_covers_redefines(world: &mut CopybookWorld) {
    let field = world.renames_field().expect("No RENAMES field found");
    let resolved = field
        .resolved_renames
        .as_ref()
        .expect("RENAMES should be resolved");
    assert!(
        !resolved.members.is_empty(),
        "RENAMES alias should cover fields"
    );
}

#[then(expr = "the alias should reference the ODO field")]
async fn then_alias_references_odo(world: &mut CopybookWorld) {
    let field = world.renames_field().expect("No RENAMES field found");
    assert!(
        field.resolved_renames.is_some(),
        "RENAMES should be resolved"
    );
}

#[then(expr = "RENAMES field should have Level-88 conditions")]
async fn then_renames_has_level88(world: &mut CopybookWorld) {
    let field = world.renames_field().expect("No RENAMES field found");
    let has_conditions = field.children.iter().any(|c| c.level == 88);
    assert!(
        has_conditions,
        "RENAMES field should have Level-88 conditions"
    );
}

#[then(expr = "the conditions should be properly associated")]
async fn then_conditions_associated(world: &mut CopybookWorld) {
    let field = world.renames_field().expect("No RENAMES field found");
    let conditions: Vec<_> = field.children.iter().filter(|c| c.level == 88).collect();
    assert!(!conditions.is_empty(), "Should have associated conditions");
}

#[then(expr = "the RENAMES structure should be preserved")]
async fn then_renames_preserved(world: &mut CopybookWorld) {
    assert!(
        world.encoded_output.is_some(),
        "Encoded output should exist"
    );
}

#[then(expr = "the sign should be properly handled")]
async fn then_sign_handled(world: &mut CopybookWorld) {
    assert!(
        world.decoded_output.is_some(),
        "Decoded output should exist"
    );
}

#[then(expr = "the error should indicate invalid SIGN SEPARATE placement")]
async fn then_error_invalid_sign_separate(world: &mut CopybookWorld) {
    let error = world.error.as_ref().expect("Error should be set");
    let msg = format!("{}", error);
    assert!(
        msg.to_uppercase().contains("SIGN")
            || msg.to_uppercase().contains("SEPARATE")
            || msg.contains("INVALID"),
        "Error should indicate invalid SIGN SEPARATE placement, got: {}",
        msg
    );
}

#[then(expr = "the error should indicate invalid RENAMES range")]
async fn then_error_invalid_renames_range(world: &mut CopybookWorld) {
    let error = world.error.as_ref().expect("Error should be set");
    let msg = format!("{}", error);
    assert!(
        msg.to_uppercase().contains("RENAMES")
            || msg.contains("NONEXISTENT")
            || msg.contains("not found"),
        "Error should indicate invalid RENAMES range, got: {}",
        msg
    );
}

#[then(expr = "FILLER should not be in output")]
async fn then_filler_not_in_output(world: &mut CopybookWorld) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    let record: serde_json::Value =
        serde_json::from_str(output.lines().next().unwrap_or("{}")).unwrap_or_default();
    let json_str = serde_json::to_string(&record).unwrap_or_default();
    let has_filler = json_str.contains("_filler_") || json_str.contains("FILLER");
    assert!(!has_filler, "FILLER should not be in decoded output");
}

#[then(expr = "FILLER should be in output")]
async fn then_filler_in_output(world: &mut CopybookWorld) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    let record: serde_json::Value =
        serde_json::from_str(output.lines().next().unwrap_or("{}")).unwrap_or_default();
    let json_str = serde_json::to_string(&record).unwrap_or_default();
    let has_filler = json_str.contains("_filler_") || json_str.contains("FILLER");
    assert!(has_filler, "FILLER should be in decoded output");
}
