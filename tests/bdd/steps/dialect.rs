use cucumber::{given, then};

use crate::world::CopybookWorld;

#[given(expr = "Normative dialect")]
async fn given_normative_dialect(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.dialect = copybook_core::dialect::Dialect::Normative;
    world.parse_options = Some(options);
}

#[given(expr = "Zero-Tolerant dialect")]
async fn given_zero_tolerant_dialect(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.dialect = copybook_core::dialect::Dialect::ZeroTolerant;
    world.parse_options = Some(options);
}

#[given(expr = "One-Tolerant dialect")]
async fn given_one_tolerant_dialect(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.dialect = copybook_core::dialect::Dialect::OneTolerant;
    world.parse_options = Some(options);
}

#[then(expr = "the schema should use {string} dialect")]
async fn then_schema_uses_dialect(world: &mut CopybookWorld, expected_dialect: String) {
    let options = world.parse_options.as_ref().expect("Parse options not set");
    let actual = match options.dialect {
        copybook_core::dialect::Dialect::Normative => "Normative",
        copybook_core::dialect::Dialect::ZeroTolerant => "ZeroTolerant",
        copybook_core::dialect::Dialect::OneTolerant => "OneTolerant",
    };
    assert_eq!(
        actual, expected_dialect,
        "Expected dialect '{}', got '{}'",
        expected_dialect, actual
    );
}
