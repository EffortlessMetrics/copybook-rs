// SPDX-License-Identifier: AGPL-3.0-or-later
//! Output rendering for edited PIC encoding.
//!
//! Walks the pattern from right to left, dispatching each token to the
//! appropriate character(s) in the output buffer. Digits are consumed from
//! the scaled digit vector; sign editing tokens consult the effective sign;
//! fill characters (commas, zero suppression) are applied based on whether
//! significant digits have already been emitted to the right.

use super::encode_pattern::{is_after_decimal_at, token_width};
use super::{PicToken, Sign};

/// Render the final output string for `encode_edited_numeric`.
///
/// `digits` is the scale-adjusted digit vector. `int_digits` is the number of
/// digits in `digits` that fall before the decimal point. `decimal_places` is
/// the target scale (digits after the decimal point). `output_len` is the
/// pre-computed character width of the result. `is_zero` reflects whether the
/// *original parsed* value is all zeros — comma suppression and sign editing
/// both depend on this, and scale adjustment can flip the answer.
pub(super) fn render(
    pattern: &[PicToken],
    digits: &[u8],
    int_digits: usize,
    decimal_places: usize,
    effective_sign: Sign,
    is_zero: bool,
    output_len: usize,
) -> String {
    let mut buf: Vec<char> = vec![' '; output_len];
    let mut int_cursor = int_digits;
    let mut frac_cursor = decimal_places;
    let mut char_pos = output_len;

    for (token_idx, token) in pattern.iter().enumerate().rev() {
        char_pos -= token_width(token);
        let after_dec = is_after_decimal_at(pattern, token_idx);

        match token {
            PicToken::Digit | PicToken::ZeroInsert => {
                buf[char_pos] = next_digit_char(
                    digits,
                    int_digits,
                    &mut int_cursor,
                    &mut frac_cursor,
                    after_dec,
                )
                .unwrap_or('0');
            }
            PicToken::ZeroSuppress => {
                buf[char_pos] = next_digit_char(
                    digits,
                    int_digits,
                    &mut int_cursor,
                    &mut frac_cursor,
                    after_dec,
                )
                .unwrap_or(' ');
            }
            PicToken::AsteriskFill => {
                buf[char_pos] = next_digit_char(
                    digits,
                    int_digits,
                    &mut int_cursor,
                    &mut frac_cursor,
                    after_dec,
                )
                .unwrap_or('*');
            }
            PicToken::DecimalPoint => buf[char_pos] = '.',
            PicToken::Comma => {
                buf[char_pos] = comma_or_space(&buf, char_pos, is_zero);
            }
            PicToken::Slash => buf[char_pos] = '/',
            PicToken::Currency => buf[char_pos] = '$',
            PicToken::LeadingPlus | PicToken::TrailingPlus => {
                buf[char_pos] = match effective_sign {
                    Sign::Positive => '+',
                    Sign::Negative => '-',
                };
            }
            PicToken::LeadingMinus | PicToken::TrailingMinus => {
                buf[char_pos] = match effective_sign {
                    Sign::Positive => ' ',
                    Sign::Negative => '-',
                };
            }
            PicToken::Credit => write_pair(&mut buf, char_pos, effective_sign, ('C', 'R')),
            PicToken::Debit => write_pair(&mut buf, char_pos, effective_sign, ('D', 'B')),
            PicToken::Space => buf[char_pos] = ' ',
        }
    }

    buf.into_iter().collect()
}

/// Consume the next digit from the appropriate cursor (integer or fractional)
/// and return it as a `char`. Returns `None` once that cursor is exhausted.
fn next_digit_char(
    digits: &[u8],
    int_digits: usize,
    int_cursor: &mut usize,
    frac_cursor: &mut usize,
    after_decimal: bool,
) -> Option<char> {
    if after_decimal {
        if *frac_cursor == 0 {
            return None;
        }
        *frac_cursor -= 1;
        char::from_digit(u32::from(digits[int_digits + *frac_cursor]), 10)
    } else {
        if *int_cursor == 0 {
            return None;
        }
        *int_cursor -= 1;
        char::from_digit(u32::from(digits[*int_cursor]), 10)
    }
}

/// Decide whether a comma position should render as `,` or be suppressed to ` `.
/// Commas are suppressed when the value is zero and no significant digit has
/// already been emitted to the right of this position.
fn comma_or_space(buf: &[char], pos: usize, is_zero: bool) -> char {
    let has_significant_right = buf[pos + 1..]
        .iter()
        .any(|&ch| ch != ' ' && ch != '0' && ch != ',' && ch != '.');
    if !is_zero || has_significant_right {
        ','
    } else {
        ' '
    }
}

/// Write a two-character credit/debit marker (`CR` or `DB`) for negatives,
/// or spaces for positives.
fn write_pair(buf: &mut [char], pos: usize, sign: Sign, marker: (char, char)) {
    match sign {
        Sign::Positive => {
            buf[pos] = ' ';
            buf[pos + 1] = ' ';
        }
        Sign::Negative => {
            buf[pos] = marker.0;
            buf[pos + 1] = marker.1;
        }
    }
}
