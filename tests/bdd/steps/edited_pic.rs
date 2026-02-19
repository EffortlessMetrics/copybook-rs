// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::given;

use crate::world::CopybookWorld;

#[given(expr = "a copybook with edited PIC:")]
async fn given_copybook_with_edited_pic(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with invalid SIGN SEPARATE")]
async fn given_invalid_sign_separate(world: &mut CopybookWorld) {
    world.copybook_text = Some(
        "01  RECORD.\n\
             05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE INVALID."
            .to_string(),
    );
}
