#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_core::parse_copybook;

#[test]
fn invalid_level_reports_line_number() {
    let src = "01 REC.\n\
               99 F PIC X(3).\n";
    let err = parse_copybook(src).unwrap_err();
    // exact code may vary, but we at least want a line number
    assert!(err.context.as_ref().and_then(|c| c.line_number).is_some());
    assert_eq!(err.context.unwrap().line_number.unwrap(), 2);
}
