# How-To Guides

This directory holds **task-oriented** guides.

Use these documents when you need to solve a specific problem or perform an explicit operation.

## Available How-To Guides

- **[Handle Errors in Production](error-handling-production.md)**  
  Implement structured handling for parse, schema, and data errors in enterprise pipelines.
- **[Benchmark Regression Testing](benchmark-regression-testing.md)**  
  Set up and run performance regression tests from issue-49 workflows.
- **[Optimize Performance](performance-optimization.md)**  
  Tune decode/encode paths, scratch buffers, and throughput diagnostics.
- **[Configure Security Scanning](configure-security-scanning.md)**  
  Set up CI scanning controls for dependencies and container images.

## Typical Use Cases

### Running into production errors

- Start with [Error Handling in Production](error-handling-production.md).
- Then confirm behavior using [Troubleshooting Matrix](../TROUBLESHOOTING_MATRIX.md).
- For recurring incidents, capture recurring scenarios in issue/PR gate receipts (see process docs in root).

### Improving performance behavior

- Start with [Optimize Performance](performance-optimization.md).
- Add benchmarking checks using [Benchmark Regression Testing](benchmark-regression-testing.md).
- Validate with governance docs:
  - [Performance Governance](../PERFORMANCE_GOVERNANCE.md)
  - [Operator Runbook](../perf/OPERATOR_RUNBOOK.md) (where applicable)

### Security hardening in CI

- Start with [Configure Security Scanning](configure-security-scanning.md).
- Reconcile with repository policy docs:
  - [SECURITY.md](../../SECURITY.md)
  - [RELEASE_RUNBOOK.md](../RELEASE_RUNBOOK.md)

## How to Add a New How-To

1. Define a concrete goal and success criteria.
2. Provide prerequisites and preconditions.
3. Provide numbered operational steps.
4. Add validation commands and expected outcomes.
5. Add links to related references and explanations.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
