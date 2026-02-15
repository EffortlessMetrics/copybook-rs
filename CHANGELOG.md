# Changelog

All notable changes to copybook-rs are documented here. This root file is the canonical source used by release workflows; `docs/CHANGELOG.md` now defers to it.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.3] — 2026-02-11

### Added

- V0.4.2 improvements - edited PIC, memory optimization, K8s deployment, stdout support ([#204](https://github.com/effortlessmetrics/copybook-rs/issues/204))
- Add Kubernetes jobs for copybook-rs operations including decode, encode, verify, and determinism validation
- **encode**: Support writing output to stdout with summary suppression
- **memory**: Implement SequenceRing and StreamingProcessor for ordered processing
- Support all edited PIC patterns including Space (B) insertion
- **tests**: Add custom generators for property testing in copybook format
- Add schema invariants tests and coverage workflow
- Update project description and topics in settings.yml; add GEMINI.md for project overview and structure

### Changed

- **decode**: Streamline stdout writing by removing unnecessary line breaks
- **memory**: Optimize chunk processing and buffer growth logic

### Documentation

- Update USER_GUIDE.md status and test counts
- Update README and documentation for comprehensive testing methodologies
- Add Copilot instructions for project overview, build, and testing guidelines

### Fixed

- **codec**: Fix iterator tests placed outside `mod tests` block causing compilation failures
- **codec**: Fix memory tests with incorrect blocking channel operations and threshold calculations
- **audit**: Fix formatting in compliance validation code

### Miscellaneous Tasks

- Add PR title validation workflow
- Update dependencies and MSRV to Rust 1.92
- Update dependencies across multiple packages to latest versions
- Fix clippy and tests for release prep

## [0.4.2] — 2025-12-31

### 0.4.2

- Phase 1 quality gates + RDW safety fixes ([#181](https://github.com/effortlessmetrics/copybook-rs/issues/181))

### Added

- Dialect Lever (D0) + Edited PIC Encode (E3) + CLI Utilities ([#197](https://github.com/effortlessmetrics/copybook-rs/issues/197))

### Fixed

- **codec**: Remove last production panic
- **fixtures**: Repair simple.bin EBCDIC padding (0x20 → 0x40) ([#200](https://github.com/effortlessmetrics/copybook-rs/issues/200))

### Miscellaneous Tasks

- Add SBOM workflow + clarify private distribution

### Release

- Dialect wiring + EBCDIC padding + E3 truth pass + error-code tests ([#198](https://github.com/effortlessmetrics/copybook-rs/issues/198))

### Testing

- Ignore perf test that requires --release

## [0.4.1] — 2025-12-23

### Added

- Implement binary round-trip fidelity for COBOL zoned decimal fields ([#55](https://github.com/effortlessmetrics/copybook-rs/issues/55))
- Implement Issue #50 Comprehensive Test Suite Enhancements for Enterprise Production ([#57](https://github.com/effortlessmetrics/copybook-rs/issues/57))
- Implement Issue #53 - Comprehensive Golden Fixtures Framework with Level-88 Support ([#58](https://github.com/effortlessmetrics/copybook-rs/issues/58))
- Comprehensive Enterprise Audit System for Regulatory Compliance ([#61](https://github.com/effortlessmetrics/copybook-rs/issues/61))
- Enhance documentation structure by adding pre-analysis step and clarifying comment guidelines
- Eliminate .unwrap() panics for enterprise production safety ([#33](https://github.com/effortlessmetrics/copybook-rs/issues/33)) ([#64](https://github.com/effortlessmetrics/copybook-rs/issues/64))
- Eliminate .unwrap() panics for enterprise production safety ([#63](https://github.com/effortlessmetrics/copybook-rs/issues/63)) ([#65](https://github.com/effortlessmetrics/copybook-rs/issues/65))
- **bench**: Machine-readable benchmark reporting ([#52](https://github.com/effortlessmetrics/copybook-rs/issues/52))
- Update dependencies to latest versions for improved stability and performance
- **bench**: Implement performance regression monitoring system (Issue #49 - Core System) ([#76](https://github.com/effortlessmetrics/copybook-rs/issues/76))
- **security**: Implement comprehensive dependency & security scanning infrastructure ([#35](https://github.com/effortlessmetrics/copybook-rs/issues/35))
- **codec**: Lock numeric hot paths (scratch emission), SLO receipts, and CI gates
- **core**: RENAMES resolver (Slice-2 PR A) — hierarchy fix + QNAME + positive tests ([#129](https://github.com/effortlessmetrics/copybook-rs/issues/129))
- **cli**: Add support matrix command ([#111](https://github.com/effortlessmetrics/copybook-rs/issues/111)) ([#154](https://github.com/effortlessmetrics/copybook-rs/issues/154))
- **infra**: Docs-truth guard + perf summarization ([#155](https://github.com/effortlessmetrics/copybook-rs/issues/155))
- **bench**: Add CI perf receipts infrastructure ([#66](https://github.com/effortlessmetrics/copybook-rs/issues/66)) ([#152](https://github.com/effortlessmetrics/copybook-rs/issues/152))
- **core/error**: Modernize error handling with thiserror best practices ([#151](https://github.com/effortlessmetrics/copybook-rs/issues/151))
- **codec**: Add determinism validation module (Issue #112 Phase 1)
- **cli**: Add determinism validation subcommand (Issue #112 Phase 2)
- **ci**: Add determinism smoke test workflow template (Issue #112 Phase 3)
- **renames**: Implement R2/R3 group resolver + alias API (Issue #133 Phase R2) ([#163](https://github.com/effortlessmetrics/copybook-rs/issues/163))
- **odo**: Reject nested ODO and ODO-in-REDEFINES (Issue #164 Phase N2) ([#172](https://github.com/effortlessmetrics/copybook-rs/issues/172))

### Documentation

- **license**: Clarify AGPLv3 licensing and add CLA
- Update test counts to reflect current reality (615 passing, 54 skipped)
- Replace unlinked performance claims with baseline values
- Perf claims → baseline receipts; remove unlinked GiB/s (refs #74, #52)
- Align status to 'Engineering Preview' across 4 docs (refs #74, #75)
- COBOL Support Matrix; resolve Level-88 contradiction (refs #74, #44)
- MSRV/edition/benchmark path normalization ([#74](https://github.com/effortlessmetrics/copybook-rs/issues/74))
- Add comprehensive documentation & claims audit, test/benchmark analyses, and 5-PR closure plan
- Fix broken links, update CI test assertions, fix doctest
- Maintenance updates + determinism CLI specification

### Fixed

- Improve throughput test robustness for release mode optimizations
- Correct rust-version to match compatibility with dependencies
- **codec**: RDW field naming and COMP-3 decoding fixes ([#105](https://github.com/effortlessmetrics/copybook-rs/issues/105))
- Expose zoned overpunch module
- **ci**: Comprehensive CI pipeline fixes for 5 push failures ([#134](https://github.com/effortlessmetrics/copybook-rs/issues/134))
- **ci**: Resolve Cargo Deny, coverage, and perf test CI failures ([#135](https://github.com/effortlessmetrics/copybook-rs/issues/135))
- **ci**: Move comprehensive tests to non-blocking nightly workflow ([#136](https://github.com/effortlessmetrics/copybook-rs/issues/136))
- **ci**: Resolve Codecov upload and nextest report infra issues ([#137](https://github.com/effortlessmetrics/copybook-rs/issues/137))
- **ci**: Remove unsupported nextest JUnit report generation ([#138](https://github.com/effortlessmetrics/copybook-rs/issues/138))
- **ci**: Exclude copybook-bench from nextest and cargo test runs ([#139](https://github.com/effortlessmetrics/copybook-rs/issues/139))
- **ci**: Harden nextest pin and bench exclusion guards ([#140](https://github.com/effortlessmetrics/copybook-rs/issues/140))
- **ci**: Make nextest.toml compatible with v0.9.11 ([#141](https://github.com/effortlessmetrics/copybook-rs/issues/141))
- **lexer**: 88 VALUE list comma support ([#86](https://github.com/effortlessmetrics/copybook-rs/issues/86))
- **ci**: Remove unsupported nextest archive command
- **ci**: Configure nextest JUnit output for xtask docs sync
- **ci**: Correct nextest JUnit configuration syntax
- **ci**: Make xtask docs sync steps non-blocking
- **test**: Make audit error handling test cross-platform
- **test**: Cross-platform CLI path resolution for Windows compatibility
- **core**: Remove unnecessary serde rename_all attribute ([#157](https://github.com/effortlessmetrics/copybook-rs/issues/157))
- **parser**: Correct group structure around level-66 (Issue #133 Phase R2a) ([#162](https://github.com/effortlessmetrics/copybook-rs/issues/162))
- V0.4.1 release blockers - receipt integrity and cleanup

### Miscellaneous Tasks

- Remove obsolete binary test files
- Make historic perf SLOs advisory; add nextest CI steps, xtask sync, support cmd, and diagnostics/test hardening
- Implement local-first, cost-controlled CI infrastructure ([#117](https://github.com/effortlessmetrics/copybook-rs/issues/117))
- **ci/docs**: Cancel superseded runs + CI overview + align quick.sh
- **ci/docs**: CI Quick concurrency + Level-88 VALUE clause doc
- Bump version to 0.4.1

### Release

- **release**: V0.4.0 release hygiene ([#175](https://github.com/effortlessmetrics/copybook-rs/issues/175))

### Styling

- Apply cargo fmt to merged code ([#156](https://github.com/effortlessmetrics/copybook-rs/issues/156))

### Testing

- Fix LRECL validation; sync test counts across docs ([#74](https://github.com/effortlessmetrics/copybook-rs/issues/74))

### Cli

- Centralize exit codes and propagate ExitCode across CLI, tests, docs, and CI
- Structured diagnostics, strict-policy controls, resilient IO, and test/CI plumbing
- Add stage-aware structured diagnostics, resilient stderr, and strict-policy plumbing
- Normalize help/version diagnostic ops; default to warn when not verbose

### Codec

- Tidy docs & must_use hints, inline zoned-resolution, add tests, CLI/metrics tweaks

### Rdw

- Pedantic RDW iterator hardening; metrics exporter robustness, tests, and docs

### Telemetry

- Finalize Phase 6 rollout — CI, docs, and staging deployment

## [mantle/gen/gen-20250923-084655-e065a36-6101/003-spec-finalizer-pending-e065a36] — 2025-09-23

### Added

- Add comprehensive pr-integration-validator for pre-merge validation
- Add agent customizers for generative, integrative, and review flows to align with MergeCode standards
- Adapt doc-fixer agent for copybook-rs Generative flow
- Adapt test-creator agent for copybook-rs Generative flow with COBOL domain expertise
- Add comprehensive security scanner agent for Rust codebases
- Adapt review-hygiene-sweeper for copybook-rs enterprise standards

### Documentation

- Update roadmap status to reflect v0.3.1 shipment and release details
- Adapt review-docs-fixer agent for copybook-rs enterprise documentation standards
- Adapt review-docs-and-adr agent for copybook-rs enterprise standards

## [0.3.1] — 2025-09-23

### Added

- Port key improvements from feature branch to main ([#54](https://github.com/effortlessmetrics/copybook-rs/issues/54))

### Changed

- Replace hint_black_box with black_box for performance consistency in benchmarks

### Documentation

- Update project documentation for production readiness and performance metrics
- Enhance documentation with performance specifications and add golden fixture tests for ODO validation

### Release

- **release**: Prepare v0.3.1 for crates.io publishing with updated benchmarks and fixtures

## [0.3.0] — 2025-09-22

### Added

- Add zoned overpunch module and integrate into numeric codec
- Add COMP-3 fast path optimizations and benchmarks
- Implement COMP-3 fast path optimizations and add comprehensive property tests
- Enable COMP-3 fast path by default and update documentation
- Update inline comment handling to support COBOL-2002 specifications
- Implement inline comment handling and parser options for COBOL-2002 compliance
- Add strict comments mode to enforce COBOL-85 compatibility and update CLI options
- Add strict comments mode and finalize COBOL-2002 inline comment support
- **schema**: Schema-aware ODO counters across numeric encodings

### Changed

- Improve code formatting and readability across multiple files

### Documentation

- Add strict_comments field to verify report JSON schema

### Miscellaneous Tasks

- Update repository URLs to EffortlessMetrics organization

### Performance

- Optimize ASCII overpunch encoding with O(1) lookup tables

### Release

- ODO decode/verify parity + structural tail validation

## [0.2.0] — 2025-09-19

### Added

- Add new agents for PR management and validation
- Update architecture-validator, context-scout, and docs-updater agents for copybook-rs
- Update dependency-resolver agent for copybook-rs with improved context and examples
- Revise performance-analyzer agent to align with copybook-rs throughput targets and enhance benchmarking examples
- Implement record decoder
- Comprehensive parser stabilization and CLI enhancements

### Changed

- Improve COBOL lexer and parser handling for continuations and comments

### Documentation

- Update documentation for comprehensive field decoding implementation
- Update documentation for PR #12 JSON decoder improvements
- Update documentation for parser stability improvements
- Update documentation for code quality improvements and SmallDecimal Display trait
- Finalize documentation for ODO/REDEFINES implementation and PR completion
- Update documentation for binary field alignment and code quality improvements
- Update documentation for PR #15 project status report
- Update documentation for PR #14 finalization
- Update documentation with accurate clippy compliance status
- Finalize documentation for PR #5 safety improvements
- Update documentation to reflect current state post-PR #20
- Update documentation for PR #25 - JSON encoder schema threading and REDEFINES support
- Update Diátaxis documentation for PR #21 record decoding functionality
- Finalize performance evaluation documentation and fix compilation issues ([#30](https://github.com/effortlessmetrics/copybook-rs/issues/30))

### Fixed

- Resolve critical JSON null handling and enhance COBOL data type processing
- Resolve major clippy pedantic violations - cast issues, unnecessary wraps, match arms
- Complete clippy pedantic compliance - major violations resolved
- Final clippy pedantic compliance - safe cast operations
- Resolve clippy violations and async function cleanup
- Resolve remaining clippy pedantic violations
- Apply cargo fmt to roundtrip_repro.rs
- Remove needless borrow in copybook-gen test generation
- Comprehensive clippy pedantic fixes - remove needless raw string hashes in tests and benchmarks
- Post-merge clippy and formatting cleanup
- Finalize safety improvements for PR #5 - complete clippy pedantic compliance
- Resolve clippy pedantic violations in post-merge finalization
- Apply automatic formatting to benchmark code
- Pass schema through encoder helpers
- Add missing backticks in documentation for clippy compliance
- Skip offset-based filler fields
- Reorder imports for consistency and improve readability
- Add PartialEq derive to Error and ErrorContext for improved comparison
- Resolve clippy pedantic violations after PR #27 merge
- Resolve merge conflicts and apply formatting fixes
- Resolve clippy pedantic compliance in lib_api optimizations

### Miscellaneous Tasks

- Drop unused import in iterator
- **core**: Remove ignored tests
- Comprehensive code quality improvements and clippy pedantic compliance

### Performance

- Optimize decode_record with scratch buffers

### Release

- Bump to v0.2.0

### Styling

- Apply automatic code formatting
- Apply cargo fmt to comprehensive_parser_tests.rs

### Testing

- Use assert for ODO validation ([#27](https://github.com/effortlessmetrics/copybook-rs/issues/27))
- Enhance parser fixture tests with improved string formatting

### Checkpoint

- Intermediate clippy fixes before final automated pass

<!-- generated by git-cliff -->
