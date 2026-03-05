#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for structured parser input generation.
///
/// Instead of feeding raw bytes directly, this target composes structured
/// copybook-like text from fuzzer input: level numbers, field names, and
/// PIC clauses are derived from arbitrary bytes to exercise the parser
/// with syntactically plausible but unpredictable input.
fuzz_target!(|data: &[u8]| {
    if data.len() < 4 {
        return;
    }

    let copybook_text = build_structured_copybook(data);
    if copybook_text.is_empty() {
        return;
    }

    // Parse with default options
    let _ = parse_copybook(&copybook_text);

    // Parse with strict mode
    let strict = ParseOptions {
        strict: true,
        strict_comments: true,
        ..Default::default()
    };
    let _ = parse_copybook_with_options(&copybook_text, &strict);

    // Parse with tolerant/filler options
    let tolerant = ParseOptions {
        allow_inline_comments: true,
        emit_filler: true,
        ..Default::default()
    };
    let _ = parse_copybook_with_options(&copybook_text, &tolerant);
});

/// Build a structured COBOL copybook from arbitrary bytes.
///
/// Uses the fuzzer input to select level numbers, field names, PIC types,
/// and sizes while maintaining rough COBOL syntax structure.
fn build_structured_copybook(data: &[u8]) -> String {
    let level_numbers: &[&str] = &["01", "02", "03", "05", "10", "15", "49", "66", "77", "88"];
    let pic_types: &[&str] = &[
        "PIC X",
        "PIC 9",
        "PIC S9",
        "PIC S9 COMP-3",
        "PIC S9 COMP",
        "PIC Z",
        "PIC $",
        "PIC 9V9",
        "PIC S9V99",
    ];
    let name_parts: &[&str] = &[
        "FLD", "REC", "GRP", "HDR", "DTL", "TRL", "AMT", "CNT", "ID", "NAME",
        "ADDR", "CITY", "ZIP", "ACCT", "BAL", "QTY", "ITEM", "CODE", "FLAG", "FILLER",
    ];

    let mut lines = Vec::new();
    let mut pos = 0;

    // Always start with a level-01 record
    lines.push("       01  TEST-RECORD.".to_string());

    while pos + 2 < data.len() {
        let level_byte = data[pos];
        let name_byte = data[pos + 1];
        let pic_byte = if pos + 2 < data.len() { data[pos + 2] } else { 0 };
        pos += 3;

        let level = level_numbers[(level_byte as usize) % level_numbers.len()];

        // Skip level 01 (already emitted) and special levels that need specific syntax
        if level == "01" || level == "66" || level == "88" {
            // For level 88, emit a condition value
            if level == "88" {
                let name = name_parts[(name_byte as usize) % name_parts.len()];
                let suffix = name_byte % 100;
                lines.push(format!(
                    "           88  {name}-{suffix:02}     VALUE '{suffix}'.",
                ));
            }
            continue;
        }

        let name = name_parts[(name_byte as usize) % name_parts.len()];
        let suffix = name_byte;

        // Decide whether this is a group (no PIC) or elementary (with PIC)
        if pic_byte % 5 == 0 {
            // Group item
            lines.push(format!("           {level}  {name}-{suffix:03}."));
        } else {
            let pic = pic_types[(pic_byte as usize) % pic_types.len()];
            let size = ((pic_byte as usize) % 18) + 1;
            lines.push(format!(
                "           {level}  {name}-{suffix:03}     {pic}({size}).",
            ));
        }

        // Add OCCURS clause occasionally
        if pos < data.len() && data[pos - 1] % 7 == 0 && level != "77" {
            let count = ((pic_byte as usize) % 10) + 1;
            if let Some(last) = lines.last_mut() {
                // Remove trailing period, add OCCURS, re-add period
                if last.ends_with('.') {
                    last.pop();
                    *last = format!("{last}\n                              OCCURS {count} TIMES.");
                }
            }
        }
    }

    // Ensure we have at least one elementary field
    if lines.len() < 2 {
        lines.push("           05  FILLER        PIC X(10).".to_string());
    }

    lines.join("\n")
}
