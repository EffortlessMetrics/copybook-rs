<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Investigation Report: `copybook-rs` Goals vs. Reality

This report summarizes an investigation into the `copybook-rs` project, comparing its documented goals against its actual, observable behavior.

## Overall Assessment

At its core, `copybook-rs` is a robust and well-engineered project with an extensive, passing unit test suite that covers a wide array of COBOL features. The underlying libraries appear to be of high quality. However, when assessing the project's end-to-end functionality through its command-line interface, there are significant discrepancies between its stated goals and its actual behavior, particularly concerning **round-trip data fidelity** and **performance**.

---

## Goals Met: The Strengths

*   **Code Health and Core Logic:** The project builds cleanly and has a comprehensive suite of 94 unit and integration tests that all pass. This indicates that the individual components of the `copybook-core` and `copybook-codec` libraries are functioning as the developers intend. The test suite covers complex features like `REDEFINES`, `OCCURS`, `COMP-3`, and `ZONED` decimals, which is a very positive sign.
*   **Text Data Performance:** The project significantly **exceeds** its performance target for text-heavy data. The goal was `≥80 MB/s`, but the benchmarks show performance in the **~250-290 MB/s** range, which is excellent.
*   **Basic CLI Usability:** The `copybook-cli` tool is functional for its primary commands (`inspect`, `decode`, `encode`). It successfully parses copybooks and processes data without crashing.

---

## Goals Not Met: The Discrepancies

*   **Critical Failure: Round-Trip Fidelity:** The `README.md` states that "Unchanged JSON data re-encodes to identical binary." Our testing shows this is **false** for the CLI tool.
    *   A valid binary file was generated using the project's own libraries.
    *   This file was decoded to JSON and immediately re-encoded back to binary.
    *   A byte-level comparison of the original and round-tripped files **failed**, proving the conversion is not byte-identical. This failure occurred for both "normal" data and for all-blank records.
    *   A reproducible test case for this bug has been added in `copybook-gen/examples/roundtrip_repro.rs`.

*   **Missed Performance Target:** The project **fails to meet** its performance goal for `COMP-3` (packed decimal) data. The goal was `≥40 MB/s`, but the benchmarks consistently show performance in the **~25-28 MB/s** range.

*   **Incomplete User-Facing Examples:** The sample data file provided in the repository (`fixtures/data/simple.bin`) is a non-functional placeholder text file. This prevents a new user from being able to run the examples in the `README.md` and successfully verify the tool's functionality, leading to a confusing initial experience.

*   **Bugs in Data Generation:** The `Performance` strategy within the `copybook-gen` test data generator appears to be buggy, as it only produces all-blank records instead of the predictable, patterned data that would be expected for performance testing.

## Conclusion

The project has a strong but incomplete foundation. It is closer to being a "solid parsing/decoding library" than it is to being the "production-ready, high-fidelity, high-performance CLI tool" that the `README.md` describes. The failure to provide round-trip fidelity is a critical bug that should be addressed.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
