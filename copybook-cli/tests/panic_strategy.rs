#[cfg(not(panic = "unwind"))]
compile_error!("panic=abort disables catch_unwind safeguards");

#[test]
fn panic_strategy_is_unwind() {
    // If this compiled, we're on panic=unwind.
}
