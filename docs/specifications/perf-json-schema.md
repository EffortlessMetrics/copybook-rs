# Performance JSON Schema Specification

## Overview

The `perf.json` schema provides a simplified, machine-readable format for copybook-rs benchmark reporting. This schema supports enterprise performance monitoring, CI/CD integration, and regulatory audit requirements while maintaining compatibility with existing Criterion.rs benchmark infrastructure.

## Schema Definition

### Core Structure

```json
{
  "display_gibs": 4.22,
  "comp3_mibs": 571.0,
  "warnings": [],
  "errors": []
}
```

### Field Specifications

#### display_gibs (Required)
- **Type**: `number` (IEEE 754 double precision)
- **Description**: DISPLAY data type throughput in Gibibytes per second
- **Range**: `0.0` to `100.0` (reasonable enterprise bounds)
- **Precision**: Minimum 2 decimal places for accurate measurement
- **Example**: `4.22` (representing 4.22 GiB/s throughput)

#### comp3_mibs (Required)
- **Type**: `number` (IEEE 754 double precision)
- **Description**: COMP-3 (packed decimal) throughput in Mebibytes per second
- **Range**: `0.0` to `10000.0` (reasonable enterprise bounds)
- **Precision**: Minimum 1 decimal place for accurate measurement
- **Example**: `571.0` (representing 571 MiB/s throughput)

#### warnings (Required)
- **Type**: `array` of `string`
- **Description**: Performance warnings that don't constitute failures
- **Can be empty**: Yes (successful benchmark runs)
- **Example**: `["DISPLAY throughput approaching minimum threshold"]`

#### errors (Required)
- **Type**: `array` of `string`
- **Description**: Performance errors indicating benchmark failures
- **Can be empty**: Yes (successful benchmark runs)
- **Example**: `["DISPLAY throughput below 80 MB/s floor"]`

## JSON Schema Validation

### JSON Schema Document

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://github.com/copybook-rs/perf-report-schema",
  "title": "copybook-rs Performance Report",
  "description": "Machine-readable performance benchmark report for copybook-rs enterprise mainframe data processing",
  "type": "object",
  "properties": {
    "display_gibs": {
      "type": "number",
      "description": "DISPLAY data type throughput in Gibibytes per second",
      "minimum": 0.0,
      "maximum": 100.0,
      "multipleOf": 0.01
    },
    "comp3_mibs": {
      "type": "number",
      "description": "COMP-3 packed decimal throughput in Mebibytes per second",
      "minimum": 0.0,
      "maximum": 10000.0,
      "multipleOf": 0.1
    },
    "warnings": {
      "type": "array",
      "description": "Performance warnings that don't constitute failures",
      "items": {
        "type": "string",
        "minLength": 1,
        "maxLength": 500
      },
      "maxItems": 50
    },
    "errors": {
      "type": "array",
      "description": "Performance errors indicating benchmark failures",
      "items": {
        "type": "string",
        "minLength": 1,
        "maxLength": 500
      },
      "maxItems": 50
    }
  },
  "required": ["display_gibs", "comp3_mibs", "warnings", "errors"],
  "additionalProperties": false
}
```

## Performance Value Specifications

### Enterprise Performance Context

#### Current Achievement Levels
- **DISPLAY**: 4.1-4.2 GiB/s (exceeds enterprise targets by 52x)
- **COMP-3**: 560-580 MiB/s (exceeds enterprise targets by 15x)
- **Performance Stability**: <5% variance across benchmark runs
- **Safety Margins**: Substantial headroom above enterprise requirements

#### Enterprise Performance Floors
- **DISPLAY Minimum**: 80 MB/s (~0.074 GiB/s)
- **COMP-3 Minimum**: 40 MB/s
- **Regression Threshold**: 5% performance decrease
- **SLO Compliance**: Automatic validation against floors

### Unit Conversion Reference

#### DISPLAY Throughput Units
```
1 GiB/s = 1,073,741,824 bytes/second
1 GB/s = 1,000,000,000 bytes/second
80 MB/s ≈ 0.0745 GiB/s (enterprise floor)
4.22 GiB/s ≈ 4,531 MB/s (current achievement)
```

#### COMP-3 Throughput Units
```
1 MiB/s = 1,048,576 bytes/second
1 MB/s = 1,000,000 bytes/second
40 MB/s ≈ 38.1 MiB/s (enterprise floor)
571 MiB/s ≈ 599 MB/s (current achievement)
```

## Error and Warning Taxonomy

### Warning Message Categories

#### Performance Degradation Warnings
- `"DISPLAY throughput approaching minimum threshold"`
- `"COMP-3 performance variance above 2% tolerance"`
- `"Benchmark execution time exceeded typical duration"`
- `"Memory usage approaching configured limits"`

#### System Environment Warnings
- `"CPU frequency scaling detected during benchmark"`
- `"System load above recommended threshold"`
- `"Available memory below optimal benchmark conditions"`
- `"Disk I/O contention detected"`

### Error Message Categories

#### Performance Floor Violations
- `"DISPLAY throughput below 80 MB/s floor"`
- `"COMP-3 throughput below 40 MB/s floor"`
- `"Performance regression exceeds 5% threshold"`
- `"SLO compliance validation failed"`

#### Execution Failures
- `"Benchmark execution timeout exceeded"`
- `"Cargo benchmark command failed"`
- `"Performance data parsing failed"`
- `"JSON schema validation failed"`

#### System Failures
- `"Insufficient system resources for benchmark"`
- `"Criterion.rs benchmark harness error"`
- `"Memory allocation failure during benchmark"`
- `"File I/O error during performance measurement"`

## Implementation Guidelines

### JSON Generation Process

1. **Performance Data Collection**
   - Execute `PERF=1 cargo bench -p copybook-bench`
   - Parse Criterion.rs output for DISPLAY and COMP-3 metrics
   - Apply unit conversions (MB/s → GiB/s for DISPLAY, MB/s → MiB/s for COMP-3)
   - Round to appropriate decimal precision

2. **Validation and Warning Generation**
   - Compare against enterprise performance floors
   - Detect performance regressions vs. baseline
   - Generate appropriate warning/error messages
   - Validate against performance variance thresholds

3. **JSON Schema Compliance**
   - Validate all required fields present
   - Verify numeric ranges and precision
   - Ensure array constraints met
   - Validate against JSON Schema document

### File Output Specification

#### Output Location
```
scripts/bench/perf.json
```

#### File Format
- **Encoding**: UTF-8
- **Indentation**: 2 spaces (pretty-printed for human readability)
- **Line Endings**: LF (Unix-style)
- **File Permissions**: 644 (readable by owner/group/others, writable by owner)

#### Example Output
```json
{
  "display_gibs": 4.22,
  "comp3_mibs": 571.0,
  "warnings": [
    "Performance variance above 2% tolerance"
  ],
  "errors": []
}
```

## Integration Points

### Criterion.rs Integration

The JSON schema integrates with existing Criterion.rs benchmark infrastructure:

- **Benchmark Harness**: `copybook-bench/benches/` directory
- **Performance Regression**: `copybook-bench/src/regression.rs` module
- **Benchmark Execution**: `PERF=1 cargo bench -p copybook-bench` command
- **Output Parsing**: Extract performance metrics from Criterion console output

### Python Utilities Integration

The schema supports validation and processing through Python utilities:

- **json_processor.py**: Schema validation and metric extraction
- **bench_runner.py**: JSON generation from benchmark output
- **slo_validator.py**: Performance floor validation
- **pr_automation.py**: One-liner summary generation

### CI/CD Integration

The schema enables automated workflow integration:

- **GitHub Actions**: Automated performance validation on PRs
- **PR Comments**: Machine-readable data for automated summaries
- **Baseline Promotion**: Performance data for baseline updates
- **Audit Reports**: Historical performance tracking data

## Error Handling and Validation

### JSON Schema Validation Errors

#### Required Field Missing
```json
{
  "error": "Required field missing",
  "field": "display_gibs",
  "message": "display_gibs field is required for performance report"
}
```

#### Invalid Value Range
```json
{
  "error": "Value out of range",
  "field": "comp3_mibs",
  "value": -5.0,
  "message": "comp3_mibs must be non-negative"
}
```

#### Invalid Precision
```json
{
  "error": "Invalid precision",
  "field": "display_gibs",
  "value": 4.123456789,
  "message": "display_gibs precision exceeds maximum allowed decimal places"
}
```

### Performance Validation Errors

#### Performance Floor Violation
```json
{
  "display_gibs": 0.05,
  "comp3_mibs": 25.0,
  "warnings": [],
  "errors": [
    "DISPLAY throughput below 80 MB/s floor (current: 53.7 MB/s)",
    "COMP-3 throughput below 40 MB/s floor (current: 25.0 MB/s)"
  ]
}
```

#### Regression Detection
```json
{
  "display_gibs": 3.85,
  "comp3_mibs": 520.0,
  "warnings": [
    "DISPLAY performance regression detected: 8.8% decrease from baseline",
    "COMP-3 performance regression detected: 8.9% decrease from baseline"
  ],
  "errors": []
}
```

## Testing and Validation

### Schema Compliance Testing

The schema supports comprehensive validation through test scaffolding:

- **AC2 Tests**: JSON schema validation in `copybook-bench/tests/json_schema_validation_ac2.rs`
- **Decimal Precision**: Verify 2+ decimal place precision for performance values
- **Array Handling**: Validate empty and populated warnings/errors arrays
- **Round-trip Testing**: JSON serialization/deserialization validation

### Performance Value Testing

- **Enterprise Ranges**: Validate values within reasonable enterprise bounds
- **Floor Scenarios**: Test at performance floor thresholds
- **High Performance**: Test with current achievement levels
- **Edge Cases**: Validate boundary conditions and error scenarios

### Integration Testing

- **File Generation**: Validate JSON output to `scripts/bench/perf.json`
- **Python Processing**: Verify JSON can be processed by Python utilities
- **CI/CD Integration**: Validate automated workflow consumption
- **Error Propagation**: Test error handling and validation failure scenarios

## Future Extensions

### Potential Schema Enhancements

While maintaining backward compatibility, future versions may include:

- **Metadata**: Benchmark execution timestamp, system information
- **Detailed Metrics**: Memory usage, CPU utilization, I/O statistics
- **Historical Context**: Baseline comparison data, trend information
- **Quality Metrics**: Test coverage, code quality indicators

### Versioning Strategy

- **Schema Version**: Include `$schema` field for version identification
- **Backward Compatibility**: Maintain compatibility with existing tooling
- **Migration Path**: Provide clear upgrade paths for schema changes
- **Documentation**: Version-specific documentation and examples

This specification ensures that the `perf.json` schema provides robust, enterprise-grade performance reporting while maintaining simplicity and machine-readability for automated CI/CD integration.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
