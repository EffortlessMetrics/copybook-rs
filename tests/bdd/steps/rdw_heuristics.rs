use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};
use copybook_corruption_detectors::{
    detect_ebcdic_corruption, detect_packed_corruption,
};
use cucumber::{then, when};

use crate::helpers::parse_binary_literal;
use crate::world::CopybookWorld;

#[when(expr = "the rdw ascii-corruption heuristic evaluates header {string}")]
async fn when_evaluates_rdw_heuristic(world: &mut CopybookWorld, raw_header: String) {
    let bytes = parse_binary_literal(&raw_header);
    assert_eq!(
        bytes.len(),
        4,
        "RDW headers in BDD should include exactly 4 bytes"
    );

    world.rdw_predicate_result = Some(rdw_is_suspect_ascii_corruption_slice(&bytes));
}

#[when(expr = "the rdw corruption detector evaluates header {string}")]
async fn when_evaluates_rdw_detector(world: &mut CopybookWorld, raw_header: String) {
    let bytes = parse_binary_literal(&raw_header);
    assert_eq!(
        bytes.len(),
        4,
        "RDW headers in BDD should include exactly 4 bytes"
    );

    world.rdw_predicate_result = Some(detect_rdw_ascii_corruption(&bytes).is_some());
}

#[then(expr = "the rdw ascii-corruption heuristic should report ASCII corruption")]
async fn then_rdw_heuristic_reports(world: &mut CopybookWorld) {
    assert!(world
        .rdw_predicate_result
        .expect("rdw heuristic should have been evaluated"));
}

#[then(expr = "the rdw ascii-corruption heuristic should not report ASCII corruption")]
async fn then_rdw_heuristic_does_not_report(world: &mut CopybookWorld) {
    assert!(
        !world
            .rdw_predicate_result
            .expect("rdw heuristic should have been evaluated")
    );
}

#[then(expr = "the rdw corruption detector should report corruption")]
async fn then_rdw_detector_reports(world: &mut CopybookWorld) {
    assert!(
        world
            .rdw_predicate_result
            .expect("rdw corruption detector should have been evaluated")
    );
}

#[then(expr = "the rdw corruption detector should not report corruption")]
async fn then_rdw_detector_does_not_report(world: &mut CopybookWorld) {
    assert!(
        !world
            .rdw_predicate_result
            .expect("rdw corruption detector should have been evaluated")
    );
}

#[when(expr = "the ebcdic corruption predicate evaluates bytes {string}")]
async fn when_evaluates_ebcdic_corruption_predicate(world: &mut CopybookWorld, raw_bytes: String) {
    let bytes = parse_binary_literal(&raw_bytes);
    let has_corruption = bytes.iter().copied().any(is_likely_corrupted_ebcdic_byte);
    world.rdw_predicate_result = Some(has_corruption);
}

#[then(expr = "the ebcdic corruption predicate should detect corruption")]
async fn then_ebcdic_predicate_detects(world: &mut CopybookWorld) {
    assert!(world.rdw_predicate_result.expect("predicate should have been evaluated"));
}

#[then(expr = "the ebcdic corruption predicate should not detect corruption")]
async fn then_ebcdic_predicate_not_detect(world: &mut CopybookWorld) {
    assert!(!world.rdw_predicate_result.expect("predicate should have been evaluated"));
}

#[when(expr = "the packed corruption predicate evaluates bytes {string}")]
async fn when_evaluates_packed_corruption_predicate(world: &mut CopybookWorld, raw_bytes: String) {
    let bytes = parse_binary_literal(&raw_bytes);
    let mut has_corruption = false;

    for (index, byte) in bytes.iter().copied().enumerate() {
        if is_invalid_comp3_high_nibble(byte) {
            has_corruption = true;
        }

        if index + 1 == bytes.len() {
            if is_invalid_comp3_sign_nibble(byte) {
                has_corruption = true;
            }
        } else if is_invalid_comp3_low_nibble(byte) {
            has_corruption = true;
        }
    }

    world.rdw_predicate_result = Some(has_corruption);
}

#[then(expr = "the packed corruption predicate should detect corruption")]
async fn then_packed_predicate_detects(world: &mut CopybookWorld) {
    assert!(world.rdw_predicate_result.expect("predicate should have been evaluated"));
}

#[then(expr = "the packed corruption predicate should not detect corruption")]
async fn then_packed_predicate_not_detect(world: &mut CopybookWorld) {
    assert!(!world.rdw_predicate_result.expect("predicate should have been evaluated"));
}

#[when(expr = "the ebcdic corruption detector evaluates bytes {string}")]
async fn when_evaluates_ebcdic_corruption_detector(world: &mut CopybookWorld, raw_bytes: String) {
    let bytes = parse_binary_literal(&raw_bytes);
    let result = !detect_ebcdic_corruption(&bytes, "BDD.FIELD").is_empty();
    world.rdw_predicate_result = Some(result);
}

#[then(expr = "the ebcdic corruption detector should report corruption")]
async fn then_ebcdic_detector_reports(world: &mut CopybookWorld) {
    assert!(world
        .rdw_predicate_result
        .expect("ebcdic detector should have been evaluated"));
}

#[then(expr = "the ebcdic corruption detector should not report corruption")]
async fn then_ebcdic_detector_should_not_report(world: &mut CopybookWorld) {
    assert!(!world
        .rdw_predicate_result
        .expect("ebcdic detector should have been evaluated"));
}

#[when(expr = "the packed corruption detector evaluates bytes {string}")]
async fn when_evaluates_packed_corruption_detector(world: &mut CopybookWorld, raw_bytes: String) {
    let bytes = parse_binary_literal(&raw_bytes);
    let result = !detect_packed_corruption(&bytes, "BDD.FIELD").is_empty();
    world.rdw_predicate_result = Some(result);
}

#[then(expr = "the packed corruption detector should report corruption")]
async fn then_packed_detector_reports(world: &mut CopybookWorld) {
    assert!(world
        .rdw_predicate_result
        .expect("packed detector should have been evaluated"));
}

#[then(expr = "the packed corruption detector should not report corruption")]
async fn then_packed_detector_should_not_report(world: &mut CopybookWorld) {
    assert!(!world
        .rdw_predicate_result
        .expect("packed detector should have been evaluated"));
}
