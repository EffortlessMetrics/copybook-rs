# Implementation Plan

- [ ] 1. Project Foundation & Workspace Setup
  - Create Cargo workspace with crates: copybook-core, copybook-codec, copybook-cli, copybook-gen
  - Configure MSRV 1.89, edition 2024, dual license Apache-2.0 OR MIT
  - Set up CI/CD with cargo fmt, clippy pedantic, and deny.toml
  - Implement error taxonomy with stable codes (CBKP*, CBKS*, CBKR*, CBKC*, CBKD*, CBKE*, CBKF*)
  - Configure tracing and run summary structures
  - _Requirements: R8.7, R9.1, R9.2, R9.3_

- [ ] 2. Core Parsing Infrastructure (copybook-core)
- [ ] 2.1 Lexical Analysis and Grammar Foundation
  - Implement fixed-form vs free-form detection (≥70% lines with cols 7-72 content)
  - Create tokenizer using logos for COBOL tokens (levels, PIC, USAGE, keywords)
  - Handle column-7 continuation (- indicator only), comments (* and *>), sequence areas
  - Parse PIC clauses: X(n), 9(n)[V9(m)], optional S for signed
  - Detect and reject edited PICs with CBKP051_UNSUPPORTED_EDITED_PIC
  - _Requirements: R1.1, R1.2, R1.8_

- [ ] 2.2 AST Construction and Validation
  - Build hierarchical AST from tokens (levels 01-49, groups vs elements)
  - Parse USAGE clauses (DISPLAY, COMP, COMP-3, BINARY)
  - Handle REDEFINES relationships with target validation
  - Parse OCCURS clauses (fixed and DEPENDING ON)
  - Validate ODO constraints: driver precedes array, tail-position only, not in REDEFINES
  - Implement duplicate sibling name disambiguation (NAME__dup2, NAME__dup3)
  - _Requirements: R1.1, R1.3, R1.4, R3.6, R4.1, R4.7_

- [ ] 2.3 Layout Resolution Engine
  - Compute field byte offsets with SYNCHRONIZED alignment (2/4/8 byte boundaries)
  - Calculate REDEFINES cluster sizes as max of all variants
  - Handle OCCURS fixed arrays and ODO space allocation (use max count)
  - Insert alignment padding bytes and track for encoding
  - Validate total record length and detect overlaps
  - Generate schema JSON with canonical field ordering
  - _Requirements: R1.1, R3.1, R3.4, R4.2, R4.3, R5.1, R5.5_

- [ ] 3. Data Type Codecs (copybook-codec)
- [ ] 3.1 Character Encoding and EBCDIC Support
  - Implement static lookup tables for cp037, cp273, cp500, cp1047, cp1140
  - Add ASCII pass-through mode
  - Handle unmappable characters with configurable policy (error/replace/skip)
  - Implement UTF-8 output for all text fields
  - Add EBCDIC to ASCII conversion for zoned decimal sign detection
  - _Requirements: R2.3, R12.1, R12.2, R12.3, R12.4, R12.5, R12.6_

- [ ] 3.2 Numeric Type Decoding
  - Implement zoned decimal decoder with EBCDIC/ASCII overpunch sign tables
  - Handle BLANK WHEN ZERO: all spaces decode to 0 with warning CBKD412_ZONED_BLANK_IS_ZERO
  - Implement packed decimal (COMP-3) decoder with nibble validation
  - Create binary integer decoder (big-endian, 2/4/8 bytes by PIC digits)
  - Add canonical decimal string formatting (fixed scale, -0 normalization)
  - _Requirements: R1.2, R2.4, R2.5, R2.6, R2.7_

- [ ] 3.3 Numeric Type Encoding
  - Implement zoned decimal encoder with proper sign overpunch
  - Add packed decimal encoder with correct nibble packing and sign
  - Create binary integer encoder with range validation
  - Implement BWZ encoding policy (optional --bwz-encode flag)
  - Add input validation for scale matching on encode
  - _Requirements: R7.2, R7.3, R7.4, R7.5_

- [ ] 4. Record Framing and I/O (copybook-codec)
- [ ] 4.1 Fixed Record Processing
  - Implement fixed-length record reader with LRECL validation
  - Handle ODO tail records with variable length
  - Add record length validation against schema
  - Implement fixed record writer with proper padding
  - _Requirements: R2.1, R10.6_

- [ ] 4.2 RDW Variable Record Processing
  - Implement RDW header parsing (4-byte big-endian length + reserved)
  - Add reserved byte validation with warning CBKR211_RDW_RESERVED_NONZERO
  - Detect ASCII transfer corruption with heuristic CBKF104_RDW_SUSPECT_ASCII
  - Handle zero-length records (valid only when schema prefix == 0)
  - Implement RDW writer with proper header generation
  - Add raw RDW preservation for round-trip fidelity
  - _Requirements: R2.2, R9.8_

- [ ] 5. JSON Processing and Determinism
- [ ] 5.1 Deterministic JSON Output
  - Implement streaming JSON writer with schema-ordered keys (pre-order traversal)
  - Handle REDEFINES: emit all views in declaration order
  - Process OCCURS as JSON arrays with actual length only
  - Add raw byte capture (--emit-raw=record|field|record+rdw)
  - Ensure parallel processing maintains deterministic output ordering
  - _Requirements: R6.1, R6.2, R6.4, R6.5, R6.6_

- [ ] 5.2 Round-Trip Encoding Support
  - Implement --use-raw mode for byte-identical round-trips
  - Add REDEFINES encode precedence: raw > single view > error
  - Handle ODO array encoding with counter field updates
  - Validate JSON types match copybook field expectations
  - Add comprehensive round-trip testing
  - _Requirements: R7.1, R7.6, R7.7_

- [ ] 6. Command Line Interface (copybook-cli)
- [ ] 6.1 Core CLI Commands
  - Implement parse command with schema JSON output
  - Create inspect command with human-readable layout table
  - Build decode command with streaming JSONL output
  - Add encode command for JSON to binary conversion
  - Implement verify command with structural validation
  - _Requirements: R8.1, R8.2, R8.3, R8.4_

- [ ] 6.2 CLI Options and Configuration
  - Add codepage selection (--codepage cp037|ascii|cp273|cp500|cp1047|cp1140)
  - Implement record format options (--format fixed|rdw)
  - Add JSON number policy (--json-number lossless|native)
  - Create strict/lenient modes (--strict, --max-errors)
  - Add parallel processing (--threads with ordered output)
  - Implement comprehensive error reporting and exit codes
  - _Requirements: R8.5, R8.6, R8.7, R9.3, R9.4, R9.5, R9.6_

- [ ] 7. Error Handling and Resilience
- [ ] 7.1 Structured Error Reporting
  - Implement error context with record numbers, field paths, byte offsets
  - Add configurable error handling (strict vs lenient modes)
  - Create detailed error logging with field-level context
  - Implement error counting and summary reporting
  - Add transfer corruption detection and warnings
  - _Requirements: R9.1, R9.2, R9.3, R9.4, R9.7, R9.8_

- [ ] 7.2 ODO and REDEFINES Error Handling
  - Implement ODO bounds checking with clamp vs fail behavior
  - Add REDEFINES ambiguity detection on encode
  - Handle missing counter fields and invalid references
  - Create comprehensive validation for tail-position ODO
  - _Requirements: R4.4, R4.5, R4.6, R3.6_

- [ ] 8. Performance Optimization and Streaming
- [ ] 8.1 Memory Management
  - Implement streaming record processing with bounded memory
  - Add reusable buffers for record processing
  - Create efficient parallel pipeline with ordered output
  - Optimize EBCDIC conversion with static lookup tables
  - Minimize allocations in hot codec paths
  - _Requirements: R10.1, R10.2, R10.4, R10.5, R10.6_

- [ ] 8.2 Throughput Optimization
  - Optimize zoned and packed decimal codecs for performance
  - Implement efficient JSON streaming without intermediate maps
  - Add buffered I/O with appropriate chunk sizes
  - Create performance benchmarks and SLO validation
  - Target ≥80 MB/s DISPLAY-heavy, ≥40 MB/s COMP-3-heavy throughput
  - _Requirements: R10.1, R10.2, R10.3_

- [ ] 9. Library API Implementation
- [ ] 9.1 Core Library Functions
  - Implement parse_copybook with comprehensive error handling
  - Create decode_file_to_jsonl with streaming support
  - Add encode_jsonl_to_file with validation
  - Build record iterator for programmatic access
  - Expose structured error types with context
  - _Requirements: R11.1, R11.2, R11.3, R11.4, R11.5_

- [ ] 9.2 Configuration and Options
  - Create DecodeOptions and EncodeOptions structs
  - Implement RecordFormat, Codepage, and JsonNumberMode enums
  - Add RunSummary with comprehensive statistics
  - Follow Rust conventions for error handling and API design
  - _Requirements: R11.6, R11.7_

- [ ] 10. Comprehensive Testing Suite
- [ ] 10.1 Unit and Integration Tests
  - Create parser tests for all grammar constructs and edge cases
  - Add codec tests with property testing for numeric round-trips
  - Implement layout resolution tests for alignment and REDEFINES
  - Build end-to-end CLI tests with golden file validation
  - Add error scenario tests for all error codes
  - _Requirements: All requirements need comprehensive test coverage_

- [ ] 10.2 Synthetic Test Generation (copybook-gen)
  - Create deterministic copybook and data generators
  - Generate test cases for all field type combinations
  - Add negative test cases (invalid data, corruption scenarios)
  - Implement golden test validation with SHA-256 hashes
  - Create performance test datasets (DISPLAY-heavy, COMP-3-heavy)
  - _Requirements: Performance and correctness validation_

- [ ] 11. Documentation and Release Preparation
- [ ] 11.1 User Documentation
  - Write comprehensive README with usage examples
  - Create CLI help text and man pages
  - Document error codes and troubleshooting guide
  - Add library API documentation with examples
  - Create migration guide from other COBOL tools
  - _Requirements: R8.8, usability requirements_

- [ ] 11.2 Schema Documentation and Validation
  - Create JSON meta-schemas for copybook schema and record format
  - Add schema validation and documentation
  - Implement schema fingerprinting for provenance tracking
  - Document deterministic output guarantees
  - Create examples and test cases for common scenarios
  - _Requirements: R6.1, R6.4, audit and compliance needs_