//! Example: Generate comprehensive test suite
//!
//! This example demonstrates how to use copybook-gen to create
//! comprehensive test suites for validation and regression testing.

use copybook_gen::{
    CopybookTemplate, GeneratorConfig, TestSuiteBuilder, generate_complete_test_suite,
    test_generation::calculate_suite_stats,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Generating comprehensive test suite...");

    // Generate the complete test suite
    let suites = generate_complete_test_suite();
    let stats = calculate_suite_stats(&suites);

    println!(
        "Generated {} test suites with {} total tests",
        stats.total_suites, stats.total_tests
    );

    println!("\nTest breakdown by tag:");
    for (tag, count) in &stats.tests_by_tag {
        println!("  {}: {} tests", tag, count);
    }

    // Example: Create a custom test suite
    println!("\nCreating custom test suite...");

    let config = GeneratorConfig {
        seed: 12345,
        record_count: 500,
        include_edge_cases: true,
        include_invalid_data: false,
    };

    let custom_suite = TestSuiteBuilder::new("custom_validation", "Custom validation suite")
        .with_config(config)
        .add_simple_test("basic_fields")
        .add_redefines_test("redefines_validation")
        .add_odo_test("variable_arrays")
        .add_performance_test("perf_display", CopybookTemplate::DisplayHeavy)
        .add_performance_test("perf_comp3", CopybookTemplate::Comp3Heavy)
        .build();

    println!(
        "Custom suite '{}' created with {} tests",
        custom_suite.name,
        custom_suite.tests.len()
    );

    // Export suite to JSON
    let _json_output = custom_suite.to_json()?;
    println!("\nSample test from custom suite:");
    if let Some(test) = custom_suite.tests.first() {
        println!("Test name: {}", test.name);
        println!("Tags: {:?}", test.metadata.tags);
        println!(
            "Copybook preview: {}",
            test.copybook.lines().take(5).collect::<Vec<_>>().join("\n")
        );
    }

    // Demonstrate invalid copybook generation
    println!("\nGenerating invalid copybooks for negative testing...");
    let invalid_copybooks = copybook_gen::generate_invalid_copybooks(&GeneratorConfig::default());

    for (name, copybook) in invalid_copybooks.iter().take(3) {
        println!(
            "\nInvalid case '{}': {}",
            name,
            copybook.lines().next().unwrap_or("").trim()
        );
    }

    println!("\nTest suite generation complete!");
    Ok(())
}
