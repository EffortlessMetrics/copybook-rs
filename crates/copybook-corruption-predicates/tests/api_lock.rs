// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_corruption_predicates as predicates;

#[test]
fn comp3_predicates_match_expected_layout() {
    assert!(predicates::is_invalid_comp3_high_nibble(0xA1));
    assert!(!predicates::is_invalid_comp3_high_nibble(0x41));

    assert!(predicates::is_invalid_comp3_low_nibble(0x1A));
    assert!(!predicates::is_invalid_comp3_low_nibble(0x1C));

    assert!(predicates::is_invalid_comp3_sign_nibble(0x12));
    assert!(!predicates::is_invalid_comp3_sign_nibble(0x1C));
}

#[test]
fn ascii_printable_predicate_is_stable() {
    assert!(predicates::is_ascii_printable(b'A'));
    assert!(!predicates::is_ascii_printable(0x1F));
}

#[test]
fn ebcdic_control_predicate_is_stable() {
    assert!(predicates::is_likely_corrupted_ebcdic_byte(0x00));
    assert!(predicates::is_likely_corrupted_ebcdic_byte(0x7F));
    assert!(!predicates::is_likely_corrupted_ebcdic_byte(0x41));
}
