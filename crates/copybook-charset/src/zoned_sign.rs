// SPDX-License-Identifier: AGPL-3.0-or-later
//! Codepage-specific zoned-decimal sign metadata.
#![allow(clippy::missing_inline_in_public_items)]

use crate::Codepage;

// The high nibble identifies the sign for EBCDIC zoned decimal data.
static EBCDIC_ZONED_SIGNS: [(bool, bool); 16] = [
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (false, false),
    (true, false),
    (true, true),
    (false, false),
    (true, false),
];

// ASCII overpunch uses byte-level logic rather than a zone sign table.
static ASCII_ZONED_SIGNS: [(bool, bool); 16] = [(false, false); 16];

/// Return the zoned-decimal sign table for a codepage.
#[must_use]
pub fn get_zoned_sign_table(codepage: Codepage) -> &'static [(bool, bool); 16] {
    match codepage {
        Codepage::ASCII => &ASCII_ZONED_SIGNS,
        _ => &EBCDIC_ZONED_SIGNS,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables_cover_ascii_and_ebcdic() {
        assert!(
            get_zoned_sign_table(Codepage::ASCII)
                .iter()
                .all(|entry| *entry == (false, false))
        );
        let ebcdic = get_zoned_sign_table(Codepage::CP037);
        assert_eq!(ebcdic[0xC], (true, false));
        assert_eq!(ebcdic[0xD], (true, true));
        assert_eq!(ebcdic[0xF], (true, false));
    }
}
