// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for `copybook-codepage` public API.

use copybook_codepage::{Codepage, UnmappablePolicy, get_zoned_sign_table, space_byte};
use std::str::FromStr;

// ---------------------------------------------------------------------------
// Codepage enum – all variants
// ---------------------------------------------------------------------------

#[test]
fn codepage_all_variants_exist() {
    let variants = [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];
    assert_eq!(variants.len(), 6);
}

// ---------------------------------------------------------------------------
// Codepage::is_ascii / is_ebcdic
// ---------------------------------------------------------------------------

#[test]
fn codepage_ascii_is_ascii_and_not_ebcdic() {
    assert!(Codepage::ASCII.is_ascii());
    assert!(!Codepage::ASCII.is_ebcdic());
}

#[test]
fn codepage_ebcdic_variants_are_ebcdic_and_not_ascii() {
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert!(cp.is_ebcdic(), "{cp} should be EBCDIC");
        assert!(!cp.is_ascii(), "{cp} should not be ASCII");
    }
}

#[test]
fn codepage_is_ascii_and_is_ebcdic_are_mutually_exclusive() {
    for cp in [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert_ne!(cp.is_ascii(), cp.is_ebcdic(), "mutual exclusion for {cp}");
    }
}

// ---------------------------------------------------------------------------
// Codepage::code_page_number
// ---------------------------------------------------------------------------

#[test]
fn codepage_code_page_numbers() {
    assert_eq!(Codepage::ASCII.code_page_number(), None);
    assert_eq!(Codepage::CP037.code_page_number(), Some(37));
    assert_eq!(Codepage::CP273.code_page_number(), Some(273));
    assert_eq!(Codepage::CP500.code_page_number(), Some(500));
    assert_eq!(Codepage::CP1047.code_page_number(), Some(1047));
    assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
}

// ---------------------------------------------------------------------------
// Codepage::description
// ---------------------------------------------------------------------------

#[test]
fn codepage_descriptions_contain_variant_name() {
    assert!(Codepage::ASCII.description().contains("ASCII"));
    assert!(Codepage::CP037.description().contains("037"));
    assert!(Codepage::CP273.description().contains("273"));
    assert!(Codepage::CP500.description().contains("500"));
    assert!(Codepage::CP1047.description().contains("1047"));
    assert!(Codepage::CP1140.description().contains("1140"));
}

#[test]
fn codepage_ebcdic_descriptions_mention_ebcdic() {
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert!(
            cp.description().contains("EBCDIC"),
            "{cp} description should mention EBCDIC"
        );
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

#[test]
fn codepage_display_lowercase_names() {
    assert_eq!(Codepage::ASCII.to_string(), "ascii");
    assert_eq!(Codepage::CP037.to_string(), "cp037");
    assert_eq!(Codepage::CP273.to_string(), "cp273");
    assert_eq!(Codepage::CP500.to_string(), "cp500");
    assert_eq!(Codepage::CP1047.to_string(), "cp1047");
    assert_eq!(Codepage::CP1140.to_string(), "cp1140");
}

#[test]
fn unmappable_policy_display() {
    assert_eq!(UnmappablePolicy::Error.to_string(), "error");
    assert_eq!(UnmappablePolicy::Replace.to_string(), "replace");
    assert_eq!(UnmappablePolicy::Skip.to_string(), "skip");
}

// ---------------------------------------------------------------------------
// FromStr – valid inputs
// ---------------------------------------------------------------------------

#[test]
fn codepage_from_str_lowercase() {
    assert_eq!(Codepage::from_str("ascii").unwrap(), Codepage::ASCII);
    assert_eq!(Codepage::from_str("cp037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("cp273").unwrap(), Codepage::CP273);
    assert_eq!(Codepage::from_str("cp500").unwrap(), Codepage::CP500);
    assert_eq!(Codepage::from_str("cp1047").unwrap(), Codepage::CP1047);
    assert_eq!(Codepage::from_str("cp1140").unwrap(), Codepage::CP1140);
}

#[test]
fn codepage_from_str_uppercase() {
    assert_eq!(Codepage::from_str("ASCII").unwrap(), Codepage::ASCII);
    assert_eq!(Codepage::from_str("CP037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("CP273").unwrap(), Codepage::CP273);
    assert_eq!(Codepage::from_str("CP500").unwrap(), Codepage::CP500);
    assert_eq!(Codepage::from_str("CP1047").unwrap(), Codepage::CP1047);
    assert_eq!(Codepage::from_str("CP1140").unwrap(), Codepage::CP1140);
}

#[test]
fn codepage_from_str_mixed_case() {
    assert_eq!(Codepage::from_str("Ascii").unwrap(), Codepage::ASCII);
    assert_eq!(Codepage::from_str("Cp037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("cP500").unwrap(), Codepage::CP500);
}

// ---------------------------------------------------------------------------
// FromStr – fallback / default behaviour
// ---------------------------------------------------------------------------

#[test]
fn codepage_from_str_unknown_defaults_to_cp037() {
    assert_eq!(Codepage::from_str("unknown").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("ebcdic").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("utf8").unwrap(), Codepage::CP037);
}

#[test]
fn codepage_from_str_empty_string_defaults_to_cp037() {
    assert_eq!(Codepage::from_str("").unwrap(), Codepage::CP037);
}

#[test]
fn codepage_from_str_never_errors() {
    // FromStr::Err = Infallible, so any input succeeds.
    for input in ["", "gibberish", "cp99999", "  ", "ñ", "🦀"] {
        assert!(Codepage::from_str(input).is_ok(), "should parse: {input:?}");
    }
}

// ---------------------------------------------------------------------------
// FromStr – UnmappablePolicy
// ---------------------------------------------------------------------------

#[test]
fn unmappable_policy_from_str_valid() {
    assert_eq!(
        UnmappablePolicy::from_str("error").unwrap(),
        UnmappablePolicy::Error
    );
    assert_eq!(
        UnmappablePolicy::from_str("replace").unwrap(),
        UnmappablePolicy::Replace
    );
    assert_eq!(
        UnmappablePolicy::from_str("skip").unwrap(),
        UnmappablePolicy::Skip
    );
}

#[test]
fn unmappable_policy_from_str_case_insensitive() {
    assert_eq!(
        UnmappablePolicy::from_str("REPLACE").unwrap(),
        UnmappablePolicy::Replace
    );
    assert_eq!(
        UnmappablePolicy::from_str("Skip").unwrap(),
        UnmappablePolicy::Skip
    );
    assert_eq!(
        UnmappablePolicy::from_str("ERROR").unwrap(),
        UnmappablePolicy::Error
    );
}

#[test]
fn unmappable_policy_from_str_unknown_defaults_to_error() {
    assert_eq!(
        UnmappablePolicy::from_str("unknown").unwrap(),
        UnmappablePolicy::Error
    );
    assert_eq!(
        UnmappablePolicy::from_str("").unwrap(),
        UnmappablePolicy::Error
    );
}

#[test]
fn unmappable_policy_from_str_never_errors() {
    for input in ["", "anything", "  ", "🔥"] {
        assert!(
            UnmappablePolicy::from_str(input).is_ok(),
            "should parse: {input:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// Display / FromStr round-trip
// ---------------------------------------------------------------------------

#[test]
fn codepage_display_roundtrip_for_recognized_variants() {
    // CP037 is excluded: its Display is "cp037" but FromStr("cp037") matches
    // via the catch-all, so it still works.
    for cp in [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let s = cp.to_string();
        let parsed: Codepage = s.parse().unwrap();
        assert_eq!(cp, parsed, "roundtrip failed for {cp}");
    }
}

#[test]
fn unmappable_policy_display_roundtrip() {
    for policy in [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ] {
        let s = policy.to_string();
        let parsed: UnmappablePolicy = s.parse().unwrap();
        assert_eq!(policy, parsed, "roundtrip failed for {policy}");
    }
}

// ---------------------------------------------------------------------------
// Serde round-trip
// ---------------------------------------------------------------------------

#[test]
fn codepage_serde_roundtrip_all_variants() {
    for cp in [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let json = serde_json::to_string(&cp).unwrap();
        let deserialized: Codepage = serde_json::from_str(&json).unwrap();
        assert_eq!(cp, deserialized, "serde roundtrip failed for {cp}");
    }
}

#[test]
fn unmappable_policy_serde_roundtrip_all_variants() {
    for policy in [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ] {
        let json = serde_json::to_string(&policy).unwrap();
        let deserialized: UnmappablePolicy = serde_json::from_str(&json).unwrap();
        assert_eq!(policy, deserialized, "serde roundtrip failed for {policy}");
    }
}

#[test]
fn codepage_serde_json_representation() {
    // Serde derives produce externally-tagged enum by default.
    let json = serde_json::to_string(&Codepage::CP037).unwrap();
    assert_eq!(json, "\"CP037\"");
    let json = serde_json::to_string(&Codepage::ASCII).unwrap();
    assert_eq!(json, "\"ASCII\"");
}

#[test]
fn unmappable_policy_serde_json_representation() {
    let json = serde_json::to_string(&UnmappablePolicy::Replace).unwrap();
    assert_eq!(json, "\"Replace\"");
}

// ---------------------------------------------------------------------------
// Debug
// ---------------------------------------------------------------------------

#[test]
fn codepage_debug_matches_variant_name() {
    assert_eq!(format!("{:?}", Codepage::ASCII), "ASCII");
    assert_eq!(format!("{:?}", Codepage::CP037), "CP037");
    assert_eq!(format!("{:?}", Codepage::CP273), "CP273");
    assert_eq!(format!("{:?}", Codepage::CP500), "CP500");
    assert_eq!(format!("{:?}", Codepage::CP1047), "CP1047");
    assert_eq!(format!("{:?}", Codepage::CP1140), "CP1140");
}

#[test]
fn unmappable_policy_debug_matches_variant_name() {
    assert_eq!(format!("{:?}", UnmappablePolicy::Error), "Error");
    assert_eq!(format!("{:?}", UnmappablePolicy::Replace), "Replace");
    assert_eq!(format!("{:?}", UnmappablePolicy::Skip), "Skip");
}

// ---------------------------------------------------------------------------
// Clone / Copy / Eq
// ---------------------------------------------------------------------------

#[test]
fn codepage_is_copy() {
    let cp = Codepage::CP037;
    let copy = cp; // Copy
    assert_eq!(cp, copy);
}

#[test]
fn unmappable_policy_is_copy() {
    let p = UnmappablePolicy::Skip;
    let copy = p; // Copy
    assert_eq!(p, copy);
}

#[test]
fn codepage_variants_are_distinct() {
    let all = [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];
    for (i, a) in all.iter().enumerate() {
        for (j, b) in all.iter().enumerate() {
            if i == j {
                assert_eq!(a, b);
            } else {
                assert_ne!(a, b, "{a} and {b} should differ");
            }
        }
    }
}

#[test]
fn unmappable_policy_variants_are_distinct() {
    let all = [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ];
    for (i, a) in all.iter().enumerate() {
        for (j, b) in all.iter().enumerate() {
            if i == j {
                assert_eq!(a, b);
            } else {
                assert_ne!(a, b, "{a} and {b} should differ");
            }
        }
    }
}

// ---------------------------------------------------------------------------
// space_byte
// ---------------------------------------------------------------------------

#[test]
fn space_byte_ascii_is_0x20() {
    assert_eq!(space_byte(Codepage::ASCII), 0x20);
}

#[test]
fn space_byte_all_ebcdic_is_0x40() {
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert_eq!(space_byte(cp), 0x40, "space byte for {cp}");
    }
}

#[test]
fn space_byte_consistent_with_is_ebcdic() {
    for cp in [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let expected = if cp.is_ebcdic() { 0x40 } else { 0x20 };
        assert_eq!(space_byte(cp), expected);
    }
}

// ---------------------------------------------------------------------------
// Zoned sign table
// ---------------------------------------------------------------------------

#[test]
fn zoned_sign_table_ascii_all_unsigned() {
    let table = get_zoned_sign_table(Codepage::ASCII);
    for (i, &(signed, negative)) in table.iter().enumerate() {
        assert!(!signed, "ASCII nibble 0x{i:X} should not be signed");
        assert!(!negative, "ASCII nibble 0x{i:X} should not be negative");
    }
}

#[test]
fn zoned_sign_table_ebcdic_positive_nibbles() {
    let table = get_zoned_sign_table(Codepage::CP037);
    // 0xC = positive, 0xF = positive (default unsigned zone)
    assert_eq!(table[0xC], (true, false));
    assert_eq!(table[0xF], (true, false));
}

#[test]
fn zoned_sign_table_ebcdic_negative_nibble_d() {
    let table = get_zoned_sign_table(Codepage::CP037);
    assert_eq!(table[0xD], (true, true));
}

#[test]
fn zoned_sign_table_ebcdic_unsigned_nibbles_0_through_b_and_e() {
    let table = get_zoned_sign_table(Codepage::CP037);
    for i in 0x0..=0xB {
        assert_eq!(
            table[i],
            (false, false),
            "nibble 0x{i:X} should be unsigned"
        );
    }
    assert_eq!(table[0xE], (false, false), "nibble 0xE should be unsigned");
}

#[test]
fn zoned_sign_table_same_for_all_ebcdic_codepages() {
    let reference = get_zoned_sign_table(Codepage::CP037);
    for cp in [
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert_eq!(
            get_zoned_sign_table(cp),
            reference,
            "zoned table for {cp} should match CP037"
        );
    }
}

#[test]
fn zoned_sign_table_has_16_entries() {
    assert_eq!(get_zoned_sign_table(Codepage::ASCII).len(), 16);
    assert_eq!(get_zoned_sign_table(Codepage::CP037).len(), 16);
}

// ---------------------------------------------------------------------------
// Edge cases – whitespace / special characters in FromStr
// ---------------------------------------------------------------------------

#[test]
fn codepage_from_str_with_whitespace_defaults_to_cp037() {
    // Leading/trailing whitespace is not trimmed by the implementation.
    assert_eq!(Codepage::from_str(" cp037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("cp037 ").unwrap(), Codepage::CP037);
}

#[test]
fn unmappable_policy_from_str_with_whitespace_defaults_to_error() {
    assert_eq!(
        UnmappablePolicy::from_str(" replace").unwrap(),
        UnmappablePolicy::Error
    );
}

#[test]
fn codepage_from_str_numeric_only_defaults_to_cp037() {
    // "037", "273" etc. are unrecognised (need "cp" prefix).
    assert_eq!(Codepage::from_str("037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("273").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("500").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("1047").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("1140").unwrap(), Codepage::CP037);
}
