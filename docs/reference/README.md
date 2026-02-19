# Reference

This directory holds **information-oriented** references.

Use these documents as canonical technical references and interface definitions.

## Available Reference Docs

- **[Library API](LIBRARY_API.md)**  
  Rust API references and primary types.
- **[CLI Examples](CLI_EXAMPLES.md)**  
  Command interface and usage examples.
- **[Error Codes](ERROR_CODES.md)**  
  Structured error taxonomy and operational meanings.
- **[COBOL Support Matrix](COBOL_SUPPORT_MATRIX.md)**  
  Feature-level support and dialect behavior matrix.
- **[Benchmarks](BENCHMARKS.md)**  
  Reference details for benchmark behavior and outputs.
- **[Benchmark API Contracts](benchmark-api-contracts.md)**  
  Contract definitions used by issue-49 and performance reporting flows.
- **[Grammar Renames](GRAMMAR_RENAMES.md)**  
  Rename behavior and aliasing reference for schema fields.
- **[Security Receipt Schema](security-receipt-schema.md)** and **[security-receipt-schema.json](security-receipt-schema.json)**  
  JSON schema definition and schema-serialization details.

## Standards and Usage

- Use references as the first source for API/CLI behavior.
- Pair each reference with the related explanation and how-to when implementing changes.
- Prefer this directory for signatures, structures, and explicit constraints before editing runtime behavior.
