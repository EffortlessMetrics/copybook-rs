// SPDX-License-Identifier: AGPL-3.0-or-later
//! Pattern analysis for edited PIC encoding.
//!
//! Computes structural facts about a `&[PicToken]` pattern that are independent
//! of any specific input value: how many integer and fractional digit positions
//! exist, and the total character width of the rendered output.

use super::PicToken;

/// Structural metrics extracted from an edited PIC pattern.
#[derive(Debug, Clone, Copy)]
pub(super) struct PatternMetrics {
    /// Count of digit-bearing positions before the decimal point.
    pub int_positions: usize,
    /// Count of digit-bearing positions after the decimal point.
    pub frac_positions: usize,
    /// Total character width of the rendered output (CR/DB count as 2).
    pub output_len: usize,
}

/// Analyze a pattern to extract structural metrics needed for encoding.
pub(super) fn analyze(pattern: &[PicToken]) -> PatternMetrics {
    let mut int_positions = 0;
    let mut frac_positions = 0;
    let mut output_len = 0;
    let mut after_decimal = false;

    for token in pattern {
        output_len += token_width(token);
        match token {
            PicToken::Digit
            | PicToken::ZeroSuppress
            | PicToken::ZeroInsert
            | PicToken::AsteriskFill => {
                if after_decimal {
                    frac_positions += 1;
                } else {
                    int_positions += 1;
                }
            }
            PicToken::DecimalPoint => after_decimal = true,
            _ => {}
        }
    }

    PatternMetrics {
        int_positions,
        frac_positions,
        output_len,
    }
}

/// Number of characters a single token contributes to rendered output.
pub(super) fn token_width(token: &PicToken) -> usize {
    match token {
        PicToken::Credit | PicToken::Debit => 2,
        _ => 1,
    }
}

/// True if the token at `idx` sits after the (first) decimal point in `pattern`.
pub(super) fn is_after_decimal_at(pattern: &[PicToken], idx: usize) -> bool {
    pattern[..idx].contains(&PicToken::DecimalPoint)
}
