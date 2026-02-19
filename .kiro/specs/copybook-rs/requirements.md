# Requirements Document

## Introduction

copybook-rs is a modern, memory-safe parser/codec for COBOL copybooks and fixed-record data. It provides deterministic, corpus-driven functionality through both CLI and library interfaces, targeting mainframe data modernization and ETL integration scenarios.

The tool bridges legacy COBOL data formats with modern systems by providing deterministic, reproducible conversion of mainframe-encoded records into accessible formats like JSON. This enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

## Requirements

### Requirement 1: Copybook Parsing and Schema Generation

**User Story:** As a mainframe data engineer, I want to provide a COBOL copybook file to the tool and have it parsed into an internal schema (understanding level numbers, PIC field definitions, REDEFINES, OCCURS, etc.), so that the record layout is fully defined for subsequent data parsing with no ambiguity.

#### Acceptance Criteria

1. WHEN a valid COBOL copybook is provided THEN the system SHALL parse level numbers 01-49 into a hierarchical structure
2. WHEN PIC clauses are encountered THEN the system SHALL interpret X(n) for alphanumeric, 9(n) for numeric display, and S9(n) for signed numeric fields
3. WHEN USAGE clauses specify COMP, COMP-3, or BINARY THEN the system SHALL correctly identify binary and packed decimal field types
4. WHEN REDEFINES clauses are present THEN the system SHALL create multiple views over the same storage area
5. WHEN OCCURS clauses are specified THEN the system SHALL support both fixed arrays and OCCURS DEPENDING ON (ODO) for variable arrays
6. WHEN SYNCHRONIZED clauses are present THEN the system SHALL calculate proper alignment padding for binary fields
7. WHEN unsupported features (COMP-1, COMP-2, P scaling) are encountered THEN the system SHALL fail with descriptive error messages
8. WHEN the copybook contains syntax errors THEN the system SHALL report the error with line number and context

### Requirement 2: Binary Data Decoding

**User Story:** As an ETL integrator, I want to decode mainframe data files using the parsed copybook schema, so that I can accurately extract each field from every record into a structured JSON format for downstream processing.

#### Acceptance Criteria

1. WHEN processing fixed-length records THEN the system SHALL read records of constant byte length as defined by the copybook
2. WHEN processing variable-length records with RDW THEN the system SHALL read the 4-byte Record Descriptor Word and extract the correct record data
3. WHEN encountering EBCDIC text fields THEN the system SHALL convert to UTF-8 using the specified code page (default CP037)
4. WHEN decoding zoned decimal fields THEN the system SHALL interpret sign overpunch in the last character correctly
5. WHEN decoding packed decimal (COMP-3) fields THEN the system SHALL unpack two digits per byte with proper sign nibble handling
6. WHEN decoding binary (COMP) fields THEN the system SHALL interpret as big-endian integers of appropriate width (2/4/8 bytes)
7. WHEN BLANK WHEN ZERO fields contain all spaces THEN the system SHALL decode as numeric zero with a warning
8. WHEN invalid data is encountered THEN the system SHALL report field-level errors with record context

### Requirement 3: REDEFINES and Variant Field Support

**User Story:** As a mainframe data engineer, I want the parser to handle REDEFINES clauses in the copybook (where multiple fields share the same storage area) and allow me to access each redefined view, so that data files with variant record formats or overlapping fields can be decoded without losing information.

#### Acceptance Criteria

1. WHEN a field REDEFINES another field THEN the system SHALL assign both fields the same byte offset
2. WHEN multiple fields redefine the same storage THEN the system SHALL calculate the storage size as the maximum of all variants
3. WHEN outputting JSON THEN the system SHALL include all redefined views as separate fields
4. WHEN a redefining field is larger than the original THEN the system SHALL extend the containing group size accordingly
5. WHEN encoding data with REDEFINES THEN the system SHALL require explicit selection of which variant to use
6. WHEN REDEFINES targets are missing THEN the system SHALL report semantic errors during parsing

### Requirement 4: Variable Arrays with OCCURS DEPENDING ON

**User Story:** As a mainframe data engineer, I want the parser to support OCCURS DEPENDING ON clauses (variable-length arrays whose length is determined by another field's value) when the array is at the end of its containing group, so that records with variable-length arrays are parsed to include only the actual number of elements present.

#### Acceptance Criteria

1. WHEN OCCURS DEPENDING ON is specified THEN the system SHALL require the counter field to precede the array in byte order
2. WHEN the ODO array is not at the end of its group THEN the system SHALL reject the copybook with error CBKP021_ODO_NOT_TAIL
3. WHEN decoding ODO arrays THEN the system SHALL read the counter field value to determine actual array length
4. WHEN the counter value exceeds the maximum THEN the system SHALL clamp to maximum with warning CBKS301_ODO_CLIPPED
5. WHEN the counter value is below the minimum THEN the system SHALL clamp to minimum with warning
6. WHEN encoding ODO arrays THEN the system SHALL set the counter field to match the JSON array length
7. WHEN the counter field is inside a REDEFINES or ODO region THEN the system SHALL reject with error CBKS121_COUNTER_NOT_FOUND

### Requirement 5: Field Alignment and Synchronization

**User Story:** As a mainframe data engineer, I want the parser to respect SYNCHRONIZED alignment specifications in the copybook by automatically accounting for padding bytes needed to align fields on natural boundaries, so that binary fields are read correctly without misinterpreting adjacent data.

#### Acceptance Criteria

1. WHEN SYNCHRONIZED is specified on binary fields THEN the system SHALL align to natural boundaries (2/4/8 bytes)
2. WHEN alignment requires padding THEN the system SHALL insert slack bytes and skip them during decoding
3. WHEN encoding aligned fields THEN the system SHALL emit padding bytes as 0x00
4. WHEN SYNCHRONIZED is specified on non-binary fields THEN the system SHALL ignore the alignment directive
5. WHEN calculating group lengths THEN the system SHALL include alignment padding in the total size

### Requirement 6: Deterministic JSON Output

**User Story:** As an audit/compliance user, I want the JSON output to have a deterministic schema and field order derived from the copybook (consistent field names, types, and ordering every run), so that each conversion produces the same structured output format for reliable downstream validation and audit trails.

#### Acceptance Criteria

1. WHEN generating JSON output THEN the system SHALL emit fields in schema order (pre-order traversal)
2. WHEN outputting numeric fields THEN the system SHALL use canonical string format for packed/zoned decimals to preserve precision
3. WHEN outputting binary integers THEN the system SHALL use JSON numbers for values up to 64-bit, strings for larger values
4. WHEN processing the same input multiple times THEN the system SHALL produce byte-identical JSON output
5. WHEN using parallel processing THEN the system SHALL maintain deterministic record ordering
6. WHEN REDEFINES are present THEN the system SHALL output all views in declaration order

### Requirement 7: Binary Data Encoding

**User Story:** As a mainframe data engineer, I want to take structured JSON data and encode it back into the original COBOL binary format using the same copybook, so that I can create mainframe-compatible files and ensure round-trip fidelity where unchanged data produces byte-identical output.

#### Acceptance Criteria

1. WHEN encoding JSON to binary THEN the system SHALL validate field types match copybook expectations
2. WHEN encoding numeric fields THEN the system SHALL apply proper sign encoding (overpunch for zoned, sign nibble for packed)
3. WHEN encoding text fields THEN the system SHALL convert UTF-8 to the target codepage and pad with spaces as needed
4. WHEN encoding binary fields THEN the system SHALL use big-endian byte order and proper width
5. WHEN BLANK WHEN ZERO fields have zero values THEN the system SHALL optionally emit spaces based on --bwz-encode flag
6. WHEN round-trip encoding unchanged data THEN the system SHALL produce byte-identical output to the original
7. WHEN REDEFINES are present THEN the system SHALL require explicit variant selection for encoding

### Requirement 8: Command Line Interface

**User Story:** As a cloud pipeline builder, I want a convenient command-line tool that converts mainframe data files into newline-delimited JSON records (JSONL), with flags to specify input/output files, record format, and text encoding, so that I can integrate this conversion into data pipelines.

#### Acceptance Criteria

1. WHEN running the parse command THEN the system SHALL output the copybook schema as JSON
2. WHEN running the inspect command THEN the system SHALL display a human-readable layout table with offsets and types
3. WHEN running the decode command THEN the system SHALL convert binary data to JSONL format
4. WHEN running the encode command THEN the system SHALL convert JSONL back to binary format
5. WHEN specifying --codepage THEN the system SHALL use the specified EBCDIC code page or ASCII
6. WHEN specifying --record-format THEN the system SHALL use fixed or RDW format as specified
7. WHEN errors occur THEN the system SHALL exit with appropriate status codes (0=success, 1=warnings, 2=fatal)
8. WHEN processing completes THEN the system SHALL display summary statistics including record counts and error counts

### Requirement 9: Error Handling and Resilience

**User Story:** As a platform team member, I want the tool to clearly report record-level parsing errors with details and provide configurable modes to either stop on first error or continue processing, so that data quality issues are visible while allowing batch processing to continue.

#### Acceptance Criteria

1. WHEN copybook parsing fails THEN the system SHALL report syntax errors with line numbers and stop processing
2. WHEN record decoding fails THEN the system SHALL log the error with record number and field context
3. WHEN running in strict mode THEN the system SHALL stop on the first data error
4. WHEN running in lenient mode THEN the system SHALL skip bad records and continue processing
5. WHEN --max-errors is specified THEN the system SHALL stop after reaching the error limit
6. WHEN processing completes THEN the system SHALL report total records processed and error counts
7. WHEN file transfer corruption is detected THEN the system SHALL warn about suspected ASCII conversion of binary data
8. WHEN invalid numeric data is found THEN the system SHALL report the specific field and problematic bytes

### Requirement 10: Performance and Scalability

**User Story:** As a platform team member, I want the tool to handle very large data files efficiently (multi-gigabyte mainframe dumps) by streaming through records and using bounded memory, so that it can run in production pipelines within time and hardware constraints.

#### Acceptance Criteria

1. WHEN processing large files THEN the system SHALL maintain steady-state memory usage under 256 MiB
2. WHEN decoding DISPLAY-heavy files THEN the system SHALL achieve at least 80 MB/s throughput
3. WHEN decoding COMP-3-heavy files THEN the system SHALL achieve at least 40 MB/s throughput
4. WHEN using streaming I/O THEN the system SHALL process records one at a time without loading entire files
5. WHEN parallel processing is enabled THEN the system SHALL use bounded queues to prevent memory growth
6. WHEN writing output THEN the system SHALL flush incrementally rather than buffering all results
7. WHEN processing variable-length records THEN the system SHALL efficiently handle RDW parsing without excessive allocations

### Requirement 11: Library API Integration

**User Story:** As an ETL integrator, I want to call the copybook parsing and record decoding/encoding functionality from Rust code via a stable library API, so that I can embed COBOL data translation directly into applications without invoking external processes.

#### Acceptance Criteria

1. WHEN calling parse_copybook THEN the system SHALL return a Schema object or detailed parse errors
2. WHEN calling decode_record THEN the system SHALL convert byte slices to serde_json::Value objects
3. WHEN calling encode_record THEN the system SHALL convert JSON values to byte vectors
4. WHEN using the record iterator THEN the system SHALL provide streaming access to decoded records
5. WHEN errors occur THEN the system SHALL return structured Error types with context information
6. WHEN configuring options THEN the system SHALL accept DecodeOptions and EncodeOptions structs
7. WHEN integrating with other Rust code THEN the system SHALL follow standard Rust conventions and error handling patterns

### Requirement 12: Code Page and Character Encoding Support

**User Story:** As an ETL integrator, I want to specify the source character encoding of the data (EBCDIC code page vs ASCII) and have the parser automatically convert text fields to UTF-8 while preserving binary fields, so that output is human-readable regardless of the original encoding.

#### Acceptance Criteria

1. WHEN --codepage=cp037 is specified THEN the system SHALL use EBCDIC Code Page 037 for text conversion
2. WHEN --codepage=ascii is specified THEN the system SHALL treat text fields as ASCII
3. WHEN supported code pages are used THEN the system SHALL support cp037, cp273, cp500, cp1047, cp1140
4. WHEN converting EBCDIC to UTF-8 THEN the system SHALL use static lookup tables for performance
5. WHEN invalid EBCDIC bytes are encountered THEN the system SHALL report errors with hex context
6. WHEN encoding UTF-8 to EBCDIC THEN the system SHALL handle unmappable characters with errors or replacement
7. WHEN binary fields are processed THEN the system SHALL never apply character encoding conversion