<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-codec Guidance

This file extends [`crates/AGENTS.md`](../AGENTS.md). `copybook-codec` owns
deterministic binary/JSON encode and decode, character conversion, numeric
formats, record framing, projection, and codec memory patterns. It consumes
schemas from core and must not absorb CLI orchestration.

Preserve typed `CBKD*`, `CBKE*`, and `CBKR*` failures. Check truncation,
overflow, invalid encodings, RDW bounds, variable record lengths, and malformed
JSON at the closest seam. Reuse established scratch buffers in hot loops and
benchmark before making throughput claims.

Raw capture follows the root contract: record mode emits payload-level
`raw_b64` plus compatibility `__raw_b64`; record-RDW includes header and
payload; field mode emits only `<FIELD_NAME>_raw_b64`; off emits none. Add a
regression test whenever that output shape changes and keep the library API,
CLI reference, and JSONL schema synchronized.

Run focused codec tests first, then package fmt and pedantic Clippy. Exercise
CLI or BDD coverage when framing, streaming, or public JSON output changes.
