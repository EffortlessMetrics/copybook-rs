# Changelog

All notable changes to copybook-rs will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-09-19

### Added

- `--strict` flag for `inspect`/`parse` commands: normative ODO bounds/ordering; REDEFINES ambiguity becomes error.
- Comprehensive CLI integration tests for ODO lenient/strict modes, REDEFINES, edited PIC, and fixed-form parsing.
- Enhanced documentation with validation modes, binary widths clarification, and strict mode examples.

### Fixed

- Edited PIC clauses now properly generate `CBKP051_UNSUPPORTED_EDITED_PIC` error (decimal point, `CR/DB`, blanks `B`, trailing sign).
- Column-7 continuation lines and inline `*>` comment stripping in fixed-form parsing.
- Parser now ignores sequence-area tail content after terminating `.` on the same line.
- SYNC binary field widths clarified: `≤4→16b`, `5–9→32b`, `10–18→64b`.
- Error messages now include line numbers with proper context information.

### Tests

- Added core regression tests for terminator tail handling and error line reporting.
- CLI end-to-end tests for ODO lenient/strict validation, REDEFINES processing, edited PIC handling, and fixed-form parsing.
- Stream-agnostic test assertions for improved reliability.

## [0.1.0] - 2025-09-02

### Added

- Initial release of copybook-rs workspace
- COBOL copybook parser with comprehensive field type support
- High-performance codec for encoding/decoding mainframe data formats
- CLI tool with parse, inspect, decode, encode, and verify commands
- Support for fixed-length and RDW record formats
- Multiple EBCDIC codepage support (CP037, CP273, CP500, CP1047, CP1140)
- Performance benchmarks achieving 4+ GiB/s for DISPLAY data and 500+ MiB/s for COMP-3 data