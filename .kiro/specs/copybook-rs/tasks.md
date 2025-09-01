# Implementation Plan

- [x] 1. Project Foundation & Workspace Setup






  - Create Cargo workspace with crates: copybook-core, copybook-codec, copybook-cli, copybook-gen
  - Configure MSRV 1.89, edition 2024, AGPLv3
  - Set up CI/CD with cargo fmt, clippy pedantic, and deny.toml
  - Implement error taxonomy with stable codes (CBKP*, CBKS*, CBKR*, CBKC*, CBKD*, CBKE*, CBKF*)
  - Configure tracing and run summary structures
  - Create initial fixtures: sample copybook + data + golden JSONL/binary hashes
  - Add JSON meta-schemas for schema validation and fingerprinting
  - Set up criterion benchmarks (gated by PERF=1 env var)
  - _Requirements: R8.7, R9.1, R9.2, R9.3_

- [ ] 2. Core Parsing Infrastructure (copybook-core)




- [x] 2.1 Lexical Analysis and Grammar Foundation



  - Implement fixed-form vs free-form detection (≥70% lines with cols 7-72 content)
  - Create tokenizer using logos for COBOL tokens (levels, PIC, USAGE, keywords)
  - Handle column-7 continuation (- indicator only), comments (* and *>), sequence areas
  - Parse PIC clauses: X(n), 9(n)[V9(m)], optional S for signed
  - Detect and reject edited PICs with CBKP051_UNSUPPORTED_EDITED_PIC
  - Reject SIGN LEADING/TRAILING [SEPARATE] as edited PIC → CBKP051_UNSUPPORTED_EDITED_PIC - NORMATIVE
  - Implement continuation join: strip trailing/leading spaces, preserve interior whitespace - NORMATIVE
  - _Requirements: R1.1, R1.2, R1.8_

- [x] 2.2 AST Construction and Validation




  - Build hierarchical AST from tokens (levels 01-49, groups vs elements)
  - Parse USAGE clauses (DISPLAY, COMP, COMP-3, BINARY)
  - Handle REDEFINES relationships with target validation
  - Parse OCCURS clauses (fixed and DEPENDING ON)
  - Validate ODO constraints: driver precedes array, tail-position only, not in REDEFINES
  - Support nested fixed OCCURS; ODO only at tail, not nested under another ODO - NORMATIVE
  - Implement duplicate sibling name disambiguation (NAME__dup2, NAME__dup3) - NORMATIVE
  - Add FILLER emission policy: _filler_<offset> when --emit-filler is set - NORMATIVE
  - Compute schema fingerprint: SHA-256 over canonical schema JSON + codepage + options
  - _Requirements: R1.1, R1.3, R1.4, R3.6, R4.1, R4.7_

- [x] 2.3 Layout Resolution Engine









  - Compute field byte offsets with SYNCHRONIZED alignment (2/4/8 byte boundaries)
  - Calculate REDEFINES cluster sizes as max of all variants (deterministic)
  - Handle OCCURS fixed arrays and ODO space allocation (use max count)
  - Insert alignment padding bytes and track for encoding
  - Implement overflow defense: compute offsets/lengths in u64 with explicit checks
  - Add error CBKS141_RECORD_TOO_LARGE if theoretical max record size exceeds cap
  - SYNCH and REDEFINES: alignment applied to declared field before cluster max selection
  - Generate schema JSON with canonical field ordering
  - _Requirements: R1.1, R3.1, R3.4, R4.2, R4.3, R5.1, R5.5_

- [x] 3. Data Type Codecs (copybook-codec)






- [x] 3.1 Character Encoding and EBCDIC Support


  - Implement static lookup tables for cp037, cp273, cp500, cp1047, cp1140
  - Add ASCII pass-through mode (transparent 8-bit, not Windows-1252) - NORMATIVE
  - Add ASCII zoned sign table in addition to EBCDIC; select by codepage at runtime - NORMATIVE
  - Implement --on-decode-unmappable=error|replace|skip (default error) - NORMATIVE
  - Replace mode uses U+FFFD and logs CBKC301_INVALID_EBCDIC_BYTE warning
  - Implement UTF-8 output for all text fields
  - Ensure binary/packed fields never undergo character conversion
  - Alphanumeric decode: preserve all spaces (no trimming) - NORMATIVE
  - _Requirements: R2.3, R12.1, R12.2, R12.3, R12.4, R12.5, R12.6_

- [x] 3.2 Numeric Type Decoding


  - Implement zoned decimal decoder with EBCDIC/ASCII overpunch sign tables
  - Handle BLANK WHEN ZERO: all spaces decode to 0 with warning CBKD412_ZONED_BLANK_IS_ZERO
  - Implement packed decimal (COMP-3) decoder with nibble validation
  - Create binary integer decoder (big-endian, 2/4/8 bytes by PIC digits)
  - Add fixed-scale rendering for zoned/packed: exactly scale digits after decimal - NORMATIVE
  - Normalize -0 → 0 in all output - NORMATIVE
  - Implement small decimal struct for parsing/formatting without floats
  - _Requirements: R1.2, R2.4, R2.5, R2.6, R2.7_

- [x] 3.3 Numeric Type Encoding


  - Implement zoned decimal encoder with proper sign overpunch
  - Add packed decimal encoder with correct nibble packing and sign
  - Create binary integer encoder with range validation
  - Support binary width mapping: digits → width (≤4→2B, 5-9→4B, 10-18→8B) - NORMATIVE
  - Accept explicit USAGE BINARY(n) for n ∈ {1,2,4,8} - NORMATIVE
  - Implement BWZ encoding policy (optional --bwz-encode flag)
  - Add encode validation: enforce scale match, reject otherwise with CBKE501_JSON_TYPE_MISMATCH - NORMATIVE
  - Alphanumeric encode: pad with spaces, over-length → CBKE501_JSON_TYPE_MISMATCH - NORMATIVE
  - _Requirements: R7.2, R7.3, R7.4, R7.5_

- [x] 4. Record Framing and I/O (copybook-codec)






- [x] 4.1 Fixed Record Processing


  - Implement fixed-length record reader with LRECL validation
  - Handle ODO tail records with variable length
  - Add record length validation against schema
  - Implement fixed record writer with proper padding
  - _Requirements: R2.1, R10.6_

- [x] 4.2 RDW Variable Record Processing


  - Implement RDW header parsing (4-byte big-endian length + reserved)
  - Add reserved byte validation with warning CBKR211_RDW_RESERVED_NONZERO (fatal in strict)
  - Detect ASCII transfer corruption with heuristic CBKF104_RDW_SUSPECT_ASCII
  - Implement --emit-raw=record+rdw and --use-raw preserve reserved bytes - NORMATIVE
  - Handle zero-length records: valid only when schema fixed prefix == 0 - NORMATIVE
  - Recompute length if payload changed during round-trip
  - Implement RDW writer with proper header generation
  - _Requirements: R2.2, R9.8_

- [-] 5. JSON Processing and Determinism





- [x] 5.1 Deterministic JSON Output


  - Implement streaming JSON writer in schema order; avoid intermediate maps - NORMATIVE
  - Handle REDEFINES: emit all views in declaration order (primary then redefiners)
  - Process OCCURS as JSON arrays with actual length only (no placeholders)
  - Add raw byte capture (--emit-raw=record|field|record+rdw)
  - Optional --emit-meta: append "__schema_id", "__record_index", "__offset", "__length" per record
  - Ensure parallel processing maintains deterministic output ordering
  - Writer emits by sequence ID with bounded reordering window - NORMATIVE
  - _Requirements: R6.1, R6.2, R6.4, R6.5, R6.6_


- [ ] 5.2 Round-Trip Encoding Support

  - Implement --use-raw mode for byte-identical round-trips
  - Add REDEFINES encode precedence: raw > single view > error - NORMATIVE
  - Error code CBKE501_JSON_TYPE_MISMATCH with cluster path in context for ambiguity
  - Handle ODO array encoding with counter field updates
  - Validate JSON types match copybook field expectations
  - Add comprehensive round-trip testing with golden SHA-256 validation
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
  - Implement record format options (--format fixed|rdw, explicit by default)
  - Add JSON number policy (--json-number lossless|native, default lossless)
  - Create strict/lenient modes (--strict=false default, --max-errors N)
  - Add --emit-filler, --emit-meta, --on-decode-unmappable flags
  - Add parallel processing (--threads with ordered output)
  - Implement exit semantics: warnings → 0; any errors → 1; fatal → 2 - NORMATIVE
  - Add atomic output write (tmp + rename)
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
  - Escalate ODO out-of-bounds to fatal in strict mode; clamp + warn in lenient - NORMATIVE
  - Add REDEFINES ambiguity detection on encode with proper error context
  - Handle missing counter fields and invalid references
  - Create comprehensive validation for tail-position ODO
  - Include record_index, field_path, byte_offset in context for all CBKD/CBKE errors
  - _Requirements: R4.4, R4.5, R4.6, R3.6_

- [ ] 8. Performance Optimization and Streaming

- [ ] 8.1 Memory Management
  - Implement streaming record processing with bounded memory
  - Add reusable scratch buffers per worker; digit SmallVec for packed/zoned
  - Create bounded channel + sequence ring for ordered emission
  - Test determinism: --threads 1 vs --threads 8 identical outputs
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
  - Parser fixtures: fixed/free form, col-7 continuation vs literal, inline *>, edited PIC error
  - REDEFINES: shorter/equal/longer overlays; encode ambiguity; raw-preserved record
  - Zoned: EBCDIC & ASCII sign zones; BWZ zeros; invalid zone; -0 normalization
  - Packed: odd/even digits; C/D/F signs; invalid nibble path
  - Binary: width by digits; signed/unsigned edges; alignment padding in/out
  - ODO: driver in redefine/after array reject; clamp vs strict; payload length correctness
  - RDW: reserved non-zero preserved via raw; suspect ASCII heuristic; zero-length record
  - Determinism: --threads 1 vs --threads 8 byte-identical JSONL; golden SHA-256 for re-encoded binaries
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
  - Document deterministic output guarantees and normative decisions
  - Create SPEC addendum with normative decisions (numeric rendering, REDEFINES precedence, ODO strictness, RDW raw)
  - Add CLI manpage examples for strict vs lenient, BWZ, unmappable policies
  - Create troubleshooting matrix keyed by error codes
  - _Requirements: R6.1, R6.4, audit and compliance needs_