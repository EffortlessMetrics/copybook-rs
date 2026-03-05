// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-codepage: all variants, Display, FromStr,
//! description, code_page_number, space_byte, zoned sign tables, serde, and
//! UnmappablePolicy semantics.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codepage::{Codepage, UnmappablePolicy, get_zoned_sign_table, space_byte};
use std::str::FromStr;

const ALL_CODEPAGES: [Codepage; 6] = [
    Codepage::ASCII,
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ============================================================================
// 1. All codepage variants exist and are distinct
// ============================================================================

#[test]
fn all_six_variants_are_distinct() {
    for (i, a) in ALL_CODEPAGES.iter().enumerate() {
        for (j, b) in ALL_CODEPAGES.iter().enumerate() {
            if i == j {
                assert_eq!(a, b);
            } else {
                assert_ne!(a, b, "{a} and {b} should be distinct");
            }
        }
    }
}

// ============================================================================
// 2. Display names are lowercase
// ============================================================================

#[test]
fn display_names_are_lowercase() {
    let expected = ["ascii", "cp037", "cp273", "cp500", "cp1047", "cp1140"];
    for (cp, exp) in ALL_CODEPAGES.iter().zip(expected.iter()) {
        assert_eq!(&cp.to_string(), exp, "Display for {cp:?}");
    }
}

// ============================================================================
// 3. FromStr parsing — valid lowercase
// ============================================================================

#[test]
fn from_str_lowercase_all_variants() {
    assert_eq!(Codepage::from_str("ascii").unwrap(), Codepage::ASCII);
    assert_eq!(Codepage::from_str("cp037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("cp273").unwrap(), Codepage::CP273);
    assert_eq!(Codepage::from_str("cp500").unwrap(), Codepage::CP500);
    assert_eq!(Codepage::from_str("cp1047").unwrap(), Codepage::CP1047);
    assert_eq!(Codepage::from_str("cp1140").unwrap(), Codepage::CP1140);
}

// ============================================================================
// 4. FromStr parsing — case insensitive
// ============================================================================

#[test]
fn from_str_case_insensitive() {
    assert_eq!(Codepage::from_str("ASCII").unwrap(), Codepage::ASCII);
    assert_eq!(Codepage::from_str("CP037").unwrap(), Codepage::CP037);
    assert_eq!(Codepage::from_str("Cp273").unwrap(), Codepage::CP273);
    assert_eq!(Codepage::from_str("cP500").unwrap(), Codepage::CP500);
    assert_eq!(Codepage::from_str("CP1047").unwrap(), Codepage::CP1047);
    assert_eq!(Codepage::from_str("CP1140").unwrap(), Codepage::CP1140);
}

// ============================================================================
// 5. FromStr — invalid names default to CP037
// ============================================================================

#[test]
fn from_str_unknown_defaults_to_cp037() {
    for input in ["unknown", "ebcdic", "utf-8", "037", "gibberish", ""] {
        assert_eq!(
            Codepage::from_str(input).unwrap(),
            Codepage::CP037,
            "input '{input}' should default to CP037"
        );
    }
}

#[test]
fn from_str_never_returns_error() {
    // FromStr::Err is Infallible
    for input in ["", "🦀", " cp037 ", "12345", "null"] {
        assert!(Codepage::from_str(input).is_ok(), "input '{input}'");
    }
}

// ============================================================================
// 6. Display -> FromStr roundtrip
// ============================================================================

#[test]
fn display_from_str_roundtrip_all_variants() {
    for cp in ALL_CODEPAGES {
        let displayed = cp.to_string();
        let parsed: Codepage = displayed.parse().unwrap();
        assert_eq!(cp, parsed, "roundtrip failed for {cp}");
    }
}

// ============================================================================
// 7. Description contains useful info
// ============================================================================

#[test]
fn description_ascii_mentions_ascii() {
    assert!(Codepage::ASCII.description().contains("ASCII"));
}

#[test]
fn description_ebcdic_mentions_ebcdic() {
    for cp in ALL_EBCDIC {
        assert!(
            cp.description().contains("EBCDIC"),
            "{cp} description should mention EBCDIC"
        );
    }
}

#[test]
fn description_contains_code_page_number() {
    assert!(Codepage::CP037.description().contains("037"));
    assert!(Codepage::CP273.description().contains("273"));
    assert!(Codepage::CP500.description().contains("500"));
    assert!(Codepage::CP1047.description().contains("1047"));
    assert!(Codepage::CP1140.description().contains("1140"));
}

// ============================================================================
// 8. code_page_number
// ============================================================================

#[test]
fn code_page_number_ascii_is_none() {
    assert_eq!(Codepage::ASCII.code_page_number(), None);
}

#[test]
fn code_page_number_ebcdic_values() {
    assert_eq!(Codepage::CP037.code_page_number(), Some(37));
    assert_eq!(Codepage::CP273.code_page_number(), Some(273));
    assert_eq!(Codepage::CP500.code_page_number(), Some(500));
    assert_eq!(Codepage::CP1047.code_page_number(), Some(1047));
    assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
}

#[test]
fn code_page_number_all_ebcdic_are_some() {
    for cp in ALL_EBCDIC {
        assert!(cp.code_page_number().is_some(), "{cp} should have a number");
    }
}

// ============================================================================
// 9. is_ascii / is_ebcdic mutually exclusive
// ============================================================================

#[test]
fn is_ascii_is_ebcdic_mutually_exclusive() {
    for cp in ALL_CODEPAGES {
        assert_ne!(
            cp.is_ascii(),
            cp.is_ebcdic(),
            "{cp}: is_ascii and is_ebcdic must be XOR"
        );
    }
}

#[test]
fn only_ascii_variant_returns_is_ascii_true() {
    assert!(Codepage::ASCII.is_ascii());
    for cp in ALL_EBCDIC {
        assert!(!cp.is_ascii(), "{cp} should not be ASCII");
    }
}

// ============================================================================
// 10. space_byte
// ============================================================================

#[test]
fn space_byte_ascii_0x20() {
    assert_eq!(space_byte(Codepage::ASCII), 0x20);
}

#[test]
fn space_byte_all_ebcdic_0x40() {
    for cp in ALL_EBCDIC {
        assert_eq!(space_byte(cp), 0x40, "{cp} space byte");
    }
}

// ============================================================================
// 11. Zoned sign table — structure
// ============================================================================

#[test]
fn zoned_sign_table_has_16_entries() {
    for cp in ALL_CODEPAGES {
        assert_eq!(get_zoned_sign_table(cp).len(), 16, "{cp}");
    }
}

#[test]
fn zoned_sign_table_ascii_all_unsigned() {
    let table = get_zoned_sign_table(Codepage::ASCII);
    for entry in table {
        assert_eq!(*entry, (false, false));
    }
}

#[test]
fn zoned_sign_table_ebcdic_c_positive_d_negative_f_positive() {
    for cp in ALL_EBCDIC {
        let table = get_zoned_sign_table(cp);
        assert_eq!(table[0xC], (true, false), "{cp}: 0xC positive");
        assert_eq!(table[0xD], (true, true), "{cp}: 0xD negative");
        assert_eq!(table[0xF], (true, false), "{cp}: 0xF positive");
    }
}

#[test]
fn zoned_sign_table_all_ebcdic_codepages_identical() {
    let reference = get_zoned_sign_table(Codepage::CP037);
    for cp in [
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        assert_eq!(get_zoned_sign_table(cp), reference, "{cp} vs CP037");
    }
}

// ============================================================================
// 12. UnmappablePolicy — Display
// ============================================================================

#[test]
fn unmappable_policy_display() {
    assert_eq!(UnmappablePolicy::Error.to_string(), "error");
    assert_eq!(UnmappablePolicy::Replace.to_string(), "replace");
    assert_eq!(UnmappablePolicy::Skip.to_string(), "skip");
}

// ============================================================================
// 13. UnmappablePolicy — FromStr
// ============================================================================

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
        UnmappablePolicy::from_str("SKIP").unwrap(),
        UnmappablePolicy::Skip
    );
    assert_eq!(
        UnmappablePolicy::from_str("ERROR").unwrap(),
        UnmappablePolicy::Error
    );
}

#[test]
fn unmappable_policy_from_str_unknown_defaults_to_error() {
    for input in ["", "unknown", "none", "🦀"] {
        assert_eq!(
            UnmappablePolicy::from_str(input).unwrap(),
            UnmappablePolicy::Error,
            "input '{input}' should default to Error"
        );
    }
}

// ============================================================================
// 14. UnmappablePolicy — Display/FromStr roundtrip
// ============================================================================

#[test]
fn unmappable_policy_display_from_str_roundtrip() {
    for policy in [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ] {
        let s = policy.to_string();
        let parsed: UnmappablePolicy = s.parse().unwrap();
        assert_eq!(policy, parsed, "roundtrip for {policy}");
    }
}

// ============================================================================
// 15. Serde roundtrip
// ============================================================================

#[test]
fn codepage_serde_roundtrip_all() {
    for cp in ALL_CODEPAGES {
        let json = serde_json::to_string(&cp).unwrap();
        let back: Codepage = serde_json::from_str(&json).unwrap();
        assert_eq!(cp, back, "serde roundtrip for {cp}");
    }
}

#[test]
fn unmappable_policy_serde_roundtrip_all() {
    for policy in [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ] {
        let json = serde_json::to_string(&policy).unwrap();
        let back: UnmappablePolicy = serde_json::from_str(&json).unwrap();
        assert_eq!(policy, back, "serde roundtrip for {policy}");
    }
}

#[test]
fn codepage_serde_json_string_representation() {
    assert_eq!(
        serde_json::to_string(&Codepage::ASCII).unwrap(),
        "\"ASCII\""
    );
    assert_eq!(
        serde_json::to_string(&Codepage::CP037).unwrap(),
        "\"CP037\""
    );
    assert_eq!(
        serde_json::to_string(&Codepage::CP1140).unwrap(),
        "\"CP1140\""
    );
}

// ============================================================================
// 16. Debug format matches variant name
// ============================================================================

#[test]
fn codepage_debug_format() {
    assert_eq!(format!("{:?}", Codepage::ASCII), "ASCII");
    assert_eq!(format!("{:?}", Codepage::CP037), "CP037");
    assert_eq!(format!("{:?}", Codepage::CP273), "CP273");
    assert_eq!(format!("{:?}", Codepage::CP500), "CP500");
    assert_eq!(format!("{:?}", Codepage::CP1047), "CP1047");
    assert_eq!(format!("{:?}", Codepage::CP1140), "CP1140");
}

#[test]
fn unmappable_policy_debug_format() {
    assert_eq!(format!("{:?}", UnmappablePolicy::Error), "Error");
    assert_eq!(format!("{:?}", UnmappablePolicy::Replace), "Replace");
    assert_eq!(format!("{:?}", UnmappablePolicy::Skip), "Skip");
}
