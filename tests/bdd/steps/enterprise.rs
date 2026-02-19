use cucumber::given;

use crate::world::CopybookWorld;

// ========================================================================
// Enterprise Given Steps (docstring-based copybook definitions)
// ========================================================================

#[given(expr = "a copybook with COMP-3 field:")]
async fn given_copybook_comp3(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with COMP field:")]
async fn given_copybook_comp(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with COMP-4 field:")]
async fn given_copybook_comp4(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with COMP-5 field:")]
async fn given_copybook_comp5(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SIGN SEPARATE LEADING:")]
async fn given_copybook_sign_sep_leading(
    world: &mut CopybookWorld,
    step: &cucumber::gherkin::Step,
) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SIGN SEPARATE TRAILING:")]
async fn given_copybook_sign_sep_trailing(
    world: &mut CopybookWorld,
    step: &cucumber::gherkin::Step,
) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with BLANK WHEN ZERO:")]
async fn given_copybook_bwz(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with SYNCHRONIZED:")]
async fn given_copybook_synchronized(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with RENAMES:")]
async fn given_copybook_renames_docstring(
    world: &mut CopybookWorld,
    step: &cucumber::gherkin::Step,
) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with multiple RENAMES:")]
async fn given_copybook_multiple_renames(
    world: &mut CopybookWorld,
    step: &cucumber::gherkin::Step,
) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with ODO:")]
async fn given_copybook_odo_docstring(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with nested ODO:")]
async fn given_copybook_nested_odo(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with Level-88:")]
async fn given_copybook_level88_docstring(
    world: &mut CopybookWorld,
    step: &cucumber::gherkin::Step,
) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with Level-88 VALUE THROUGH:")]
async fn given_copybook_level88_through(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with nested groups:")]
async fn given_copybook_nested_groups(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with diverse field types:")]
async fn given_copybook_diverse(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with text field:")]
async fn given_copybook_text_field(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with large OCCURS:")]
async fn given_copybook_large_occurs(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with various levels:")]
async fn given_copybook_various_levels(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with FILLER:")]
async fn given_copybook_filler(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "a copybook with multiple edited PICs:")]
async fn given_copybook_multiple_edited(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
    world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
}

#[given(expr = "lenient mode")]
async fn given_lenient_mode(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_strict_mode(false));
    }
}

#[given(expr = "strict mode")]
async fn given_strict_mode(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_strict_mode(true));
    }
}

// emit_filler steps moved to steps/filler.rs (generic `emit_filler is {word}` handler)
