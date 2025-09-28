#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_core::parse_copybook;

#[test]
fn period_terminator_ignores_tail_on_same_line() {
    // Columns 8–72 contain junk after '.', ensure parser stops at '.'
    let src = "123456 01 REC.                                              12345678\n\
               123456    05 F PIC X(10).                                   12345678\n";
    let schema = parse_copybook(src).expect("should parse");
    assert_eq!(schema.fields[0].name, "REC");
    assert_eq!(schema.fields[0].children.len(), 1);
    assert_eq!(schema.fields[0].children[0].name, "F");
}
