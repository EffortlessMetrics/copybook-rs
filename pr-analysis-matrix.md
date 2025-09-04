# PR Cross-Analysis Matrix

## File Conflict Analysis

| File/Directory | PR#2 | PR#3 | PR#4 | PR#5 | PR#7 | PR#8 | PR#9 | PR#10 | PR#17 |
|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Core Files** |  |  |  |  |  |  |  |  |  |
| copybook-core/src/parser.rs | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | |
| copybook-core/src/lexer.rs | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | |
| copybook-core/src/layout.rs | ✓ |  |  | ✓ | ✓ | ✓ |  | ✓ | |
| copybook-core/src/schema.rs | ✓ |  |  | ✓ | ✓ | ✓ |  | ✓ | |
| copybook-core/src/error_reporter.rs | ✓ |  |  | ✓ | ✓ | ✓ |  | ✓ | |
| **Codec Files** |  |  |  |  |  |  |  |  |  |
| copybook-codec/src/lib_api.rs | ✓ |  | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | |
| copybook-codec/src/json.rs | ✓ |  | ✓ |  | ✓ |  | ✓ | ✓ | |
| copybook-codec/src/numeric.rs | ✓ |  |  |  |  |  | ✓ |  | |
| copybook-codec/src/memory.rs | ✓ |  |  |  |  |  | ✓ |  | |
| copybook-codec/src/record.rs | ✓ |  |  |  |  | ✓ |  |  | |
| **CLI Files** |  |  |  |  |  |  |  |  |  |
| copybook-cli/src/utils.rs | ✓ |  | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | |
| copybook-cli/src/commands/decode.rs | ✓ |  |  | ✓ | ✓ |  |  | ✓ | |
| copybook-cli/src/commands/encode.rs | ✓ |  |  | ✓ | ✓ |  |  | ✓ | |
| **Test Files** |  |  |  |  |  |  |  |  |  |
| comprehensive_numeric_tests.rs | ✓ | ✓ | ✓ | ✓ |  |  |  |  | |
| **Benchmark Files** |  |  |  |  |  |  |  |  |  |
| decode_performance.rs | ✓ |  |  |  |  |  |  |  | ✓ |

## Test Failure Patterns

| PR | Failed Tests | Pattern |
|---|---|---|
| PR#10 | 13 tests in comprehensive_numeric_tests | Core numeric processing |
| PR#9 | 7 tests in comprehensive_numeric_tests | Record decoder + numeric |
| PR#8 | 13 tests in comprehensive_numeric_tests | Fixed record handling + numeric |
| PR#7 | 14 tests in comprehensive_numeric_tests | Filler naming + numeric |
| PR#17 | None (benchmark only) | Clean |
| PR#5 | Many gated behind feature flags | Error handling improvements |
| PR#4 | Many gated behind feature flags | Schema fingerprint |
| PR#3 | Many gated behind feature flags | Verification command |
| PR#2 | None mentioned | Performance optimizations |

## Change Size Analysis

| PR | Lines Added | Lines Deleted | Risk Level | Scope |
|---|:---:|:---:|---|---|
| PR#2 | 5305 | 2652 | **VERY HIGH** | Massive performance rewrite |
| PR#7 | 1947 | 1232 | **HIGH** | Major refactoring |
| PR#5 | 1275 | 705 | **MEDIUM** | Error handling |
| PR#4 | 861 | 330 | **MEDIUM** | Schema fingerprint |
| PR#8 | 818 | 515 | **MEDIUM** | Record handling |
| PR#10 | 280 | 209 | **LOW** | Schema exposure |
| PR#9 | 275 | 114 | **LOW** | Record decoder |
| PR#3 | 202 | 48 | **LOW** | Verification command |
| PR#17 | 6 | 7 | **MINIMAL** | Benchmark fix |