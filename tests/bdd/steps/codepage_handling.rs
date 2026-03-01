// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::then;

use crate::world::CopybookWorld;

#[then(expr = "the encoded output should not be all ASCII")]
async fn then_encoded_output_not_all_ascii(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    let has_non_ascii = output.iter().any(|&b| b > 127);
    assert!(
        has_non_ascii,
        "Expected encoded output to contain non-ASCII bytes (EBCDIC), but all bytes were <= 127"
    );
}

#[then(expr = "the encoded output should be saved as baseline")]
async fn then_encoded_output_saved_as_baseline(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    let mut hex = String::with_capacity(output.len() * 2);
    for b in output {
        use std::fmt::Write;
        let _ = write!(hex, "{b:02X}");
    }
    world.baseline_output = Some(hex);
}
