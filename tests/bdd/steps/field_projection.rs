use copybook_core::project_schema;
use cucumber::{given, then, when};

use crate::world::CopybookWorld;

#[given(expr = "field selection for projection: {string}")]
async fn given_field_selection(world: &mut CopybookWorld, fields: String) {
    let field_list: Vec<String> = fields.split(',').map(|s| s.trim().to_string()).collect();
    world.field_selection = Some(field_list);
}

#[given(expr = "field selection: {string}")]
async fn given_field_selection_alias(world: &mut CopybookWorld, fields: String) {
    let field_list: Vec<String> = if fields.trim().is_empty() {
        vec![]
    } else {
        fields.split(',').map(|s| s.trim().to_string()).collect()
    };
    world.field_selection = Some(field_list);
}

#[given(regex = r"^field selection is \[(.+)\]$")]
async fn given_field_selection_bracket(world: &mut CopybookWorld, fields: String) {
    let field_list: Vec<String> = fields
        .split(',')
        .map(|s| {
            s.trim()
                .trim_matches('"')
                .trim_matches('\'')
                .trim()
                .to_string()
        })
        .filter(|s| !s.is_empty())
        .collect();
    world.field_selection = Some(field_list);
}

#[when(expr = "field projection is applied")]
async fn when_field_projection_applied(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let fields = world
        .field_selection
        .as_ref()
        .expect("Field selection not set")
        .clone();

    match project_schema(world.schema(), &fields) {
        Ok(projected) => {
            world.projected_schema = Some(projected);
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "the schema is projected with selected fields")]
async fn when_schema_projected(world: &mut CopybookWorld) {
    when_field_projection_applied(world).await;
}

#[then(expr = "the field {string} should be included in projection")]
async fn then_field_included_in_projection(world: &mut CopybookWorld, field_name: String) {
    assert!(
        world.field_in_projection(&field_name),
        "Field '{}' should be included in projection",
        field_name
    );
}

#[then(expr = "the field {string} should not be included in projection")]
async fn then_field_not_included_in_projection(world: &mut CopybookWorld, field_name: String) {
    assert!(
        !world.field_in_projection(&field_name),
        "Field '{}' should not be included in projection",
        field_name
    );
}

#[then(expr = "the projection should succeed")]
async fn then_projection_should_succeed(world: &mut CopybookWorld) {
    assert!(
        world.projected_schema.is_some(),
        "Projection should succeed"
    );
    assert!(
        world.error.is_none(),
        "No error should occur during projection"
    );
}

#[then(expr = "the projection should fail")]
async fn then_projection_should_fail(world: &mut CopybookWorld) {
    assert!(world.error.is_some(), "Projection should fail");
}

#[then(expr = "the projected schema should contain {int} top-level field(s)")]
async fn then_projected_schema_top_level_fields(world: &mut CopybookWorld, count: usize) {
    let schema = world
        .projected_schema
        .as_ref()
        .expect("Projected schema not set");
    assert_eq!(
        schema.fields.len(),
        count,
        "Expected {} top-level fields in projected schema, got {}",
        count,
        schema.fields.len()
    );
}
