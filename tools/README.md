# Tools Directory

Development and debugging tools for copybook-rs COBOL data processing.

## Debug Executables
Binary tools for debugging and development:
- **debug_invalid_level** - Debug tool for invalid COBOL level analysis
- **debug_pic** - PIC clause parsing debug utility
- **debug_tokens** - Token stream analysis for lexer debugging
- **test_actual_invalid** - Test runner for invalid data scenarios
- **test_zoned_encoding_integration** - Zoned decimal encoding integration tests

## Shell Scripts
Maintenance and automation scripts:
- **clean_merge_conflicts.sh** - Automated merge conflict resolution
- **fix_comp3_tests.sh** - COMP-3 test suite repair utility

## Source Files
- **debug_test.rs** - Debug test implementation source

## Usage

Debug tools are typically run during development:
```bash
# Run debug tool
./tools/debug_pic

# Execute maintenance script
./tools/clean_merge_conflicts.sh
```

These tools are built as part of the development workflow and are not included in release distributions.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
