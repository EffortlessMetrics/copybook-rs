<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Coverage

Coverage is execution-surface evidence.

It answers:

> Did tests execute this Rust parser/data-conversion surface?

It does not answer:

- whether COBOL feature support is complete,
- whether parser behavior is correct,
- whether EBCDIC/ASCII conversion is correct,
- whether COMP-3, packed decimal, RDW, ODO, or REDEFINES behavior is correct,
- whether mutation adequacy is strong,
- whether fuzzing is sufficient,
- whether release packaging is valid.

Those are separate proof lanes.

The Coverage workflow runs on:

- push to `main`,
- `workflow_dispatch`,
- PRs labeled `coverage` or `full-ci`.

Codecov comments are disabled. Durable receipts are:

- `coverage.json`,
- `coverage.txt`,
- `lcov.info`,
- the GitHub Actions coverage artifact,
- the Codecov dashboard.
