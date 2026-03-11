// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_dialect::Dialect;
use copybook_parse_options::ParseOptions;

#[test]
fn parse_options_default_matches_parser_contract() {
    let options = ParseOptions::default();

    assert!(!options.emit_filler);
    assert_eq!(options.codepage, "cp037");
    assert!(options.allow_inline_comments);
    assert!(!options.strict);
    assert!(!options.strict_comments);
    assert_eq!(options.dialect, Dialect::Normative);
}

#[test]
fn parse_options_can_enable_strict_comment_modes() {
    let options = ParseOptions {
        strict: true,
        strict_comments: true,
        allow_inline_comments: false,
        ..ParseOptions::default()
    };

    assert!(options.strict);
    assert!(options.strict_comments);
    assert!(!options.allow_inline_comments);
}
