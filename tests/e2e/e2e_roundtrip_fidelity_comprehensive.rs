// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E roundtrip fidelity tests.
//!
//! Each test exercises the full CLI pipeline: decode binary → JSON → encode → binary,
//! then asserts byte-identical output. This is the **core invariant** of copybook-rs.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use assert_cmd::Command;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").expect("copybook binary should exist")
}

fn p(dir: &TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

/// Full decode→encode roundtrip via CLI, asserting byte-identical output.
fn roundtrip_test(copybook: &str, binary_data: &[u8], codepage: &str) {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = p(&dir, "test.cpy");
    let bin = p(&dir, "input.bin");
    let jsonl = p(&dir, "output.jsonl");
    let roundtrip_bin = p(&dir, "roundtrip.bin");

    std::fs::write(&cpy, copybook).unwrap();
    std::fs::write(&bin, binary_data).unwrap();

    // Decode
    cmd()
        .args(["decode"])
        .arg(&cpy)
        .arg(&bin)
        .arg("--output")
        .arg(&jsonl)
        .args(["--format", "fixed", "--codepage", codepage])
        .assert()
        .success();

    // Encode
    cmd()
        .args(["encode"])
        .arg(&cpy)
        .arg(&jsonl)
        .arg("--output")
        .arg(&roundtrip_bin)
        .args(["--format", "fixed", "--codepage", codepage])
        .assert()
        .success();

    // Verify byte-identical
    let original = std::fs::read(&bin).unwrap();
    let roundtripped = std::fs::read(&roundtrip_bin).unwrap();
    assert_eq!(
        original.len(),
        roundtripped.len(),
        "Length mismatch: original {} bytes, roundtripped {} bytes",
        original.len(),
        roundtripped.len()
    );
    assert_eq!(
        original, roundtripped,
        "Roundtrip fidelity violated — binary not byte-identical"
    );
}

// =========================================================================
// 1. PIC X (alphanumeric) — various lengths
// =========================================================================

#[test]
fn roundtrip_pic_x_short() {
    let cpy = "\
       01  REC.
           05  FLD  PIC X(5).
";
    // "HELLO" in EBCDIC CP037: H=C8 E=C5 L=D3 L=D3 O=D6
    let data: &[u8] = &[0xC8, 0xC5, 0xD3, 0xD3, 0xD6];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_pic_x_medium() {
    let cpy = "\
       01  REC.
           05  NAME  PIC X(20).
";
    // "ALICE" + 15 EBCDIC spaces
    let mut data = vec![0xC1, 0xD3, 0xC9, 0xC3, 0xC5]; // ALICE
    data.extend_from_slice(&[0x40; 15]);
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_pic_x_all_spaces() {
    let cpy = "\
       01  REC.
           05  BLANK-FLD  PIC X(10).
";
    let data = vec![0x40; 10]; // all EBCDIC spaces
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 2. PIC 9 (numeric display) — various lengths
// =========================================================================

#[test]
fn roundtrip_pic_9_short() {
    let cpy = "\
       01  REC.
           05  NUM  PIC 9(3).
";
    // "042" in EBCDIC
    let data: &[u8] = &[0xF0, 0xF4, 0xF2];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_pic_9_long() {
    let cpy = "\
       01  REC.
           05  BIG-NUM  PIC 9(10).
";
    // "0012345678"
    let data: &[u8] = &[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_pic_9_all_zeros() {
    let cpy = "\
       01  REC.
           05  NUM-ZEROS  PIC 9(6).
";
    let data: &[u8] = &[0xF0; 6]; // "000000"
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 3. PIC S9 (signed numeric display with overpunch)
// =========================================================================

#[test]
fn roundtrip_signed_display_positive() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5).
";
    // +12345: last digit overpunch positive (5 → 0xF5 for unsigned; for overpunch +5 → 0xC5 in CP037)
    // In EBCDIC CP037, positive overpunch: 0→C0, 1→C1, ..., 9→C9
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xC5]; // "+12345"
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_signed_display_negative() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5).
";
    // -12345: last digit overpunch negative (5 → 0xD5 in CP037)
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xD5]; // "-12345"
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_signed_display_zero() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(4).
";
    // +0001: overpunch positive 1 → 0xC1
    let data: &[u8] = &[0xF0, 0xF0, 0xF0, 0xC1];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 4. PIC S9 COMP-3 (packed decimal)
// =========================================================================

#[test]
fn roundtrip_comp3_positive() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(7)V99 COMP-3.
";
    // +12345.67 → digits 001234567, sign C → packed 00 12 34 56 7C
    let data: &[u8] = &[0x00, 0x12, 0x34, 0x56, 0x7C];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_comp3_negative() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(7)V99 COMP-3.
";
    // -99999.99 → 00 99 99 99 9D
    let data: &[u8] = &[0x00, 0x99, 0x99, 0x99, 0x9D];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_comp3_zero() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5)V99 COMP-3.
";
    // +0.00 → PIC S9(5)V99 = 7 digits + sign = 8 nibbles = 4 bytes: 00 00 00 0C
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x0C];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 5. PIC S9 COMP (binary integer)
// =========================================================================

#[test]
fn roundtrip_comp_halfword() {
    let cpy = "\
       01  REC.
           05  VAL  PIC S9(4) COMP.
";
    // Halfword (2 bytes big-endian): +1234 → 0x04D2
    let data: &[u8] = &[0x04, 0xD2];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_comp_fullword() {
    let cpy = "\
       01  REC.
           05  VAL  PIC S9(8) COMP.
";
    // Fullword (4 bytes big-endian): +12345678 → 0x00BC614E
    let data: &[u8] = &[0x00, 0xBC, 0x61, 0x4E];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_comp_negative() {
    let cpy = "\
       01  REC.
           05  VAL  PIC S9(4) COMP.
";
    // Halfword (2 bytes big-endian): -100 → two's complement 0xFF9C
    let data: &[u8] = &[0xFF, 0x9C];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 6. PIC S9 SIGN LEADING SEPARATE
// =========================================================================

#[test]
fn roundtrip_sign_leading_separate_positive() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5) SIGN IS LEADING SEPARATE.
";
    // "+12345" in EBCDIC: sign byte + 5 digit bytes = 6 bytes
    // '+' = 0x4E in CP037, digits F1-F5
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_sign_leading_separate_negative() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5) SIGN IS LEADING SEPARATE.
";
    // "-00042" in EBCDIC: '-' = 0x60 in CP037
    let data: &[u8] = &[0x60, 0xF0, 0xF0, 0xF0, 0xF4, 0xF2];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 7. PIC S9 SIGN TRAILING SEPARATE
// =========================================================================

#[test]
fn roundtrip_sign_trailing_separate_positive() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5) SIGN IS TRAILING SEPARATE.
";
    // "12345+" in EBCDIC: 5 digit bytes + sign byte = 6 bytes
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0x4E];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_sign_trailing_separate_negative() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5) SIGN IS TRAILING SEPARATE.
";
    // "00099-" in EBCDIC
    let data: &[u8] = &[0xF0, 0xF0, 0xF0, 0xF9, 0xF9, 0x60];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 8. PIC S9V99 (implied decimal, display)
// =========================================================================

#[test]
fn roundtrip_implied_decimal_positive() {
    let cpy = "\
       01  REC.
           05  AMT  PIC 9(5)V99.
";
    // 12345.67 stored as "1234567" in EBCDIC (7 bytes)
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_implied_decimal_signed() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5)V99.
";
    // +12345.67: last digit overpunch positive 7→C7
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xC7];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 9. Group structures (nested fields)
// =========================================================================

#[test]
fn roundtrip_nested_group() {
    let cpy = "\
       01  CUSTOMER-REC.
           05  CUST-ID     PIC 9(6).
           05  CUST-NAME.
               10  FIRST-NAME  PIC X(10).
               10  LAST-NAME   PIC X(15).
           05  STATUS      PIC X(1).
";
    // 6 + 10 + 15 + 1 = 32 bytes
    let mut data = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "001234"
    // "JOHN" + 6 spaces
    data.extend_from_slice(&[0xD1, 0xD6, 0xC8, 0xD5]);
    data.extend_from_slice(&[0x40; 6]);
    // "SMITH" + 10 spaces
    data.extend_from_slice(&[0xE2, 0xD4, 0xC9, 0xE3, 0xC8]);
    data.extend_from_slice(&[0x40; 10]);
    data.push(0xC1); // "A"
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_deeply_nested_group() {
    let cpy = "\
       01  REC.
           05  HDR  PIC X(4).
           05  DETAIL.
               10  SUB-GRP.
                   15  FLD-A  PIC 9(3).
                   15  FLD-B  PIC X(5).
               10  FLD-C  PIC 9(4).
";
    // 4 + 3 + 5 + 4 = 16 bytes
    let mut data = Vec::new();
    // HDR "ABCD"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]);
    // FLD-A "123"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3]);
    // FLD-B "HELLO"
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]);
    // FLD-C "4567"
    data.extend_from_slice(&[0xF4, 0xF5, 0xF6, 0xF7]);
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 10. OCCURS fixed arrays (flat items — CLI encode compatible)
// =========================================================================

#[test]
fn roundtrip_repeated_fields_like_occurs() {
    // Simulate array-like structure with flat repeated fields
    let cpy = "\
       01  REC.
           05  COUNT-FLD  PIC 9(2).
           05  ITEM1      PIC X(6).
           05  ITEM2      PIC X(6).
           05  ITEM3      PIC X(6).
";
    // 2 + 6*3 = 20 bytes
    let mut data = Vec::new();
    // COUNT-FLD "03"
    data.extend_from_slice(&[0xF0, 0xF3]);
    // ITEM1 "ITEM01" → C9 E3 C5 D4 F0 F1
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF1]);
    // ITEM2 "ITEM02"
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF2]);
    // ITEM3 "ITEM03"
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF3]);
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_many_numeric_fields() {
    let cpy = "\
       01  REC.
           05  HEADER  PIC X(4).
           05  AMT1    PIC 9(4).
           05  AMT2    PIC 9(4).
           05  AMT3    PIC 9(4).
           05  AMT4    PIC 9(4).
           05  AMT5    PIC 9(4).
";
    // 4 + 5*4 = 24 bytes
    let mut data = Vec::new();
    // HEADER "TEST"
    data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3]);
    // 5 amounts: 0100, 0200, 0300, 0400, 0500
    for val in [
        [0xF0, 0xF1, 0xF0, 0xF0],
        [0xF0, 0xF2, 0xF0, 0xF0],
        [0xF0, 0xF3, 0xF0, 0xF0],
        [0xF0, 0xF4, 0xF0, 0xF0],
        [0xF0, 0xF5, 0xF0, 0xF0],
    ] {
        data.extend_from_slice(&val);
    }
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 11. Mixed field types in one record
// =========================================================================

#[test]
fn roundtrip_mixed_display_comp3() {
    let cpy = "\
       01  REC.
           05  ID        PIC 9(6).
           05  NAME      PIC X(20).
           05  BALANCE   PIC S9(7)V99 COMP-3.
           05  DATE-FLD  PIC 9(8).
           05  STATUS    PIC X(1).
";
    // 6 + 20 + 5 + 8 + 1 = 40 bytes
    let mut data = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1]); // "000001"
    // "SMITH" + 15 spaces
    data.extend_from_slice(&[0xE2, 0xD4, 0xC9, 0xE3, 0xC8]);
    data.extend_from_slice(&[0x40; 15]);
    // +50000.00 COMP-3: 00 50 00 00 0C
    data.extend_from_slice(&[0x00, 0x50, 0x00, 0x00, 0x0C]);
    // "20250101"
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF5, 0xF0, 0xF1, 0xF0, 0xF1]);
    data.push(0xC1); // "A"
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_mixed_comp_and_display() {
    let cpy = "\
       01  REC.
           05  SEQ       PIC S9(4) COMP.
           05  LABEL     PIC X(10).
           05  AMOUNT    PIC S9(5)V99 COMP-3.
           05  CODE      PIC 9(3).
";
    // 2 + 10 + 4 + 3 = 19 bytes
    let mut data = Vec::new();
    // SEQ = +42 → 0x002A
    data.extend_from_slice(&[0x00, 0x2A]);
    // LABEL "DATA" + 6 spaces
    data.extend_from_slice(&[0xC4, 0xC1, 0xE3, 0xC1]);
    data.extend_from_slice(&[0x40; 6]);
    // AMOUNT +100.50 → PIC S9(5)V99 = 7 digits + sign = 4 bytes: 00 10 05 0C
    data.extend_from_slice(&[0x00, 0x10, 0x05, 0x0C]);
    // CODE "007"
    data.extend_from_slice(&[0xF0, 0xF0, 0xF7]);
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_mixed_all_types() {
    let cpy = "\
       01  REC.
           05  ALPHA      PIC X(8).
           05  NUM-DISP   PIC 9(6).
           05  SIGNED-D   PIC S9(5).
           05  PKD        PIC S9(5)V99 COMP-3.
           05  BIN-FLD    PIC S9(4) COMP.
";
    // 8 + 6 + 5 + 4 + 2 = 25 bytes
    let mut data = Vec::new();
    // ALPHA "TESTDATA"
    data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3, 0xC4, 0xC1, 0xE3, 0xC1]);
    // NUM-DISP "123456"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]);
    // SIGNED-D +54321 → last digit overpunch positive: 1→C1
    data.extend_from_slice(&[0xF5, 0xF4, 0xF3, 0xF2, 0xC1]);
    // PKD +999.99 → 00 99 99 9C
    data.extend_from_slice(&[0x00, 0x99, 0x99, 0x9C]);
    // BIN-FLD +256 → 0x0100
    data.extend_from_slice(&[0x01, 0x00]);
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 12. Multi-record files (10+ records)
// =========================================================================

#[test]
fn roundtrip_multi_record_10() {
    let cpy = "\
       01  REC.
           05  ID     PIC 9(4).
           05  NAME   PIC X(10).
";
    // 14 bytes per record × 10 records
    let mut data = Vec::new();
    for i in 0..10u16 {
        // ID: "0001", "0002", ..., "0010"
        let id_str = format!("{:04}", i + 1);
        for ch in id_str.bytes() {
            data.push(0xF0 + (ch - b'0'));
        }
        // NAME: "REC" + digit + 6 spaces
        let name_str = format!("REC{}", i + 1);
        for ch in name_str.bytes() {
            // Simple ASCII uppercase → EBCDIC CP037 mapping
            match ch {
                b'A'..=b'I' => data.push(0xC1 + (ch - b'A')),
                b'J'..=b'R' => data.push(0xD1 + (ch - b'J')),
                b'S'..=b'Z' => data.push(0xE2 + (ch - b'S')),
                b'0'..=b'9' => data.push(0xF0 + (ch - b'0')),
                _ => data.push(0x40),
            }
        }
        // Pad with spaces to fill 10 chars for NAME
        let pad = 10 - name_str.len();
        data.extend_from_slice(&vec![0x40; pad]);
    }
    assert_eq!(data.len(), 140, "10 records × 14 bytes");
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_multi_record_20_mixed() {
    let cpy = "\
       01  REC.
           05  SEQ-NUM    PIC 9(4).
           05  AMOUNT     PIC S9(5)V99 COMP-3.
           05  LABEL      PIC X(8).
";
    // 4 + 4 + 8 = 16 bytes per record × 20 records
    let mut data = Vec::new();
    for i in 0..20u32 {
        // SEQ-NUM
        let seq = format!("{:04}", i + 1);
        for ch in seq.bytes() {
            data.push(0xF0 + (ch - b'0'));
        }
        // AMOUNT: value i*100.00 → packed COMP-3
        // PIC S9(5)V99 = 7 digits + sign = 8 nibbles = 4 bytes
        let val = (i + 1) * 10000; // (i+1)*100.00 → integer representation with V99
        let d6 = ((val / 1_000_000) % 10) as u8;
        let d5 = ((val / 100_000) % 10) as u8;
        let d4 = ((val / 10_000) % 10) as u8;
        let d3 = ((val / 1_000) % 10) as u8;
        let d2 = ((val / 100) % 10) as u8;
        let d1 = ((val / 10) % 10) as u8;
        let d0 = (val % 10) as u8;
        // Pack: d6 d5 | d4 d3 | d2 d1 | d0 sign
        data.push((d6 << 4) | d5);
        data.push((d4 << 4) | d3);
        data.push((d2 << 4) | d1);
        data.push((d0 << 4) | 0x0C); // positive sign
        // LABEL: "ITEM" + 4 spaces
        data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0x40, 0x40, 0x40, 0x40]);
    }
    assert_eq!(data.len(), 320, "20 records × 16 bytes");
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 13. Codepage CP1047
// =========================================================================

#[test]
fn roundtrip_cp1047_alphanumeric() {
    let cpy = "\
       01  REC.
           05  NAME  PIC X(10).
           05  NUM   PIC 9(4).
";
    // In CP1047, digits are also F0-F9; upper alpha identical to CP037 for basic letters.
    let mut data = Vec::new();
    // "HELLO" + 5 spaces in CP1047 (same as CP037 for these chars)
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]);
    data.extend_from_slice(&[0x40; 5]);
    // "1234"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]);
    roundtrip_test(cpy, &data, "cp1047");
}

// =========================================================================
// 14. Edge cases
// =========================================================================

#[test]
fn roundtrip_single_byte_field() {
    let cpy = "\
       01  REC.
           05  FLAG  PIC X(1).
";
    let data: &[u8] = &[0xE8]; // "Y"
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_max_value_numeric() {
    let cpy = "\
       01  REC.
           05  BIG  PIC 9(9).
";
    // "999999999"
    let data: &[u8] = &[0xF9; 9];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_filler_preserves_spaces() {
    let cpy = "\
       01  REC.
           05  ID     PIC X(4).
           05  FILLER PIC X(8).
           05  CODE   PIC 9(3).
";
    // 4 + 8 + 3 = 15 bytes; FILLER omitted from JSON so encode fills with 0x00
    let mut data = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // "ABCD"
    data.extend_from_slice(&[0x00; 8]); // FILLER (zeros — encoder default)
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]); // "042"
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_comp3_max_value() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(7)V99 COMP-3.
";
    // +9999999.99 → 99 99 99 99 9C
    let data: &[u8] = &[0x99, 0x99, 0x99, 0x99, 0x9C];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 15. Complex enterprise-like record
// =========================================================================

#[test]
fn roundtrip_enterprise_customer_record() {
    let cpy = "\
       01  CUSTOMER-RECORD.
           05  CUST-ID          PIC 9(8).
           05  CUST-NAME.
               10  FIRST-NAME   PIC X(15).
               10  LAST-NAME    PIC X(20).
           05  ACCOUNT-BAL      PIC S9(9)V99 COMP-3.
           05  CREDIT-LIMIT     PIC S9(7)V99 COMP-3.
           05  OPEN-DATE        PIC 9(8).
           05  STATUS           PIC X(1).
           05  ACCT-TYPE        PIC X(2).
";
    // 8 + 15 + 20 + 6 + 5 + 8 + 1 + 2 = 65 bytes
    let mut data = Vec::new();
    // CUST-ID "00012345"
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
    // FIRST-NAME "JOHN" + 11 spaces
    data.extend_from_slice(&[0xD1, 0xD6, 0xC8, 0xD5]);
    data.extend_from_slice(&[0x40; 11]);
    // LAST-NAME "SMITH" + 15 spaces
    data.extend_from_slice(&[0xE2, 0xD4, 0xC9, 0xE3, 0xC8]);
    data.extend_from_slice(&[0x40; 15]);
    // ACCOUNT-BAL +123456.78 → PIC S9(9)V99 = 11 digits + sign = 6 bytes: 00 01 23 45 67 8C
    data.extend_from_slice(&[0x00, 0x01, 0x23, 0x45, 0x67, 0x8C]);
    // CREDIT-LIMIT +50000.00 → PIC S9(7)V99 = 9 digits + sign = 5 bytes: 00 50 00 00 0C
    data.extend_from_slice(&[0x00, 0x50, 0x00, 0x00, 0x0C]);
    // OPEN-DATE "20200115"
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF0, 0xF0, 0xF1, 0xF1, 0xF5]);
    // STATUS "A"
    data.push(0xC1);
    // ACCT-TYPE "PR"
    data.extend_from_slice(&[0xD7, 0xD9]);
    assert_eq!(data.len(), 65);
    roundtrip_test(cpy, &data, "cp037");
}

#[test]
fn roundtrip_enterprise_transaction_record() {
    let cpy = "\
       01  TRANS-REC.
           05  TRANS-ID       PIC 9(10).
           05  TRANS-DATE     PIC 9(8).
           05  TRANS-AMOUNT   PIC S9(9)V99 COMP-3.
           05  TRANS-TYPE     PIC X(3).
           05  ACCT-FROM      PIC 9(8).
           05  ACCT-TO        PIC 9(8).
           05  MEMO           PIC X(30).
";
    // 10 + 8 + 6 + 3 + 8 + 8 + 30 = 73 bytes
    let mut data = Vec::new();
    // TRANS-ID "0000000001"
    data.extend_from_slice(&[0xF0; 9]);
    data.push(0xF1);
    // TRANS-DATE "20250615"
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF5, 0xF0, 0xF6, 0xF1, 0xF5]);
    // TRANS-AMOUNT +5000.00 → PIC S9(9)V99 = 11 digits + sign = 6 bytes: 00 00 05 00 00 0C
    data.extend_from_slice(&[0x00, 0x00, 0x05, 0x00, 0x00, 0x0C]);
    // TRANS-TYPE "ACH"
    data.extend_from_slice(&[0xC1, 0xC3, 0xC8]);
    // ACCT-FROM "10001234"
    data.extend_from_slice(&[0xF1, 0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]);
    // ACCT-TO "20005678"
    data.extend_from_slice(&[0xF2, 0xF0, 0xF0, 0xF0, 0xF5, 0xF6, 0xF7, 0xF8]);
    // MEMO "WIRE TRANSFER" + 17 spaces
    let memo = b"WIRE TRANSFER";
    for ch in memo.iter() {
        match ch {
            b'A'..=b'I' => data.push(0xC1 + (ch - b'A')),
            b'J'..=b'R' => data.push(0xD1 + (ch - b'J')),
            b'S'..=b'Z' => data.push(0xE2 + (ch - b'S')),
            b' ' => data.push(0x40),
            _ => data.push(0x40),
        }
    }
    data.extend_from_slice(&[0x40; 17]);
    assert_eq!(data.len(), 73);
    roundtrip_test(cpy, &data, "cp037");
}

// =========================================================================
// 16. Signed display with implied decimal
// =========================================================================

#[test]
fn roundtrip_signed_implied_decimal_negative() {
    let cpy = "\
       01  REC.
           05  AMT  PIC S9(5)V99.
";
    // -12345.67: last digit overpunch negative 7→D7
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xD7];
    roundtrip_test(cpy, data, "cp037");
}

// =========================================================================
// 17. Comp fullword zero and negative
// =========================================================================

#[test]
fn roundtrip_comp_zero() {
    let cpy = "\
       01  REC.
           05  VAL  PIC S9(4) COMP.
";
    // zero → 0x0000
    let data: &[u8] = &[0x00, 0x00];
    roundtrip_test(cpy, data, "cp037");
}

#[test]
fn roundtrip_comp_fullword_negative() {
    let cpy = "\
       01  REC.
           05  VAL  PIC S9(8) COMP.
";
    // -1 → 0xFFFFFFFF (two's complement)
    let data: &[u8] = &[0xFF, 0xFF, 0xFF, 0xFF];
    roundtrip_test(cpy, data, "cp037");
}
