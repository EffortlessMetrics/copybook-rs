#[test]
fn panic_is_unwind() {
    assert!(
        cfg!(panic = "unwind"),
        "panic=abort disables catch_unwind safeguards"
    );
}
