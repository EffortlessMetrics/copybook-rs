//! Property-based tests for copybook-rs
//!
//! This is the entry point for all property tests. Property tests verify
//! that certain invariants hold true for a wide range of randomly
//! generated inputs.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod config;
mod generators;
mod roundtrip;
mod parsing;
mod numeric;
mod arrays;
mod pic_clauses;
mod redefines;

fn main() {
    // Property tests are run via cargo test, not as a binary
    println!("Run property tests with: cargo test -p copybook-proptest");
    println!("Or run with: cargo test --test proptest");
}
