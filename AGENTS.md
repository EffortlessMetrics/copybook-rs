<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Repository Agent Guide

This is the tool-neutral maintenance contract for `copybook-rs`. The closest
`AGENTS.md` applies to work in its directory and descendants.

## Authority and source truth

Use current repository evidence in this order:

1. Canonical behavior documents, code, tests, policy, and generated receipts.
2. This file and the closest scoped `AGENTS.md` for workflow and ownership.
3. Nested `claude.md` files as topology maps only.
4. Chat history and prior agent notes.

Do not copy volatile facts such as crate, test, dependency, or error-code counts
into guidance. Verify them from the workspace when they matter.

Canonical references include:

| Concern | Source |
| --- | --- |
| Documentation index | `docs/README.md` |
| Feature support | `docs/reference/COBOL_SUPPORT_MATRIX.md` |
| Error contracts | `docs/reference/ERROR_CODES.md` |
| CLI behavior | `docs/CLI_REFERENCE.md` |
| Library API | `docs/reference/LIBRARY_API.md` |
| JSONL schema | `docs/jsonl-schema.md` |
| Performance policy and receipts | `docs/PERFORMANCE_GOVERNANCE.md`, `scripts/bench/perf.json` |
| Project status | `docs/ROADMAP.md`, `docs/REPORT.md` |
| Dialect and edited PIC contracts | `docs/internal/features/d0_dialect_lever_contract.md`, `docs/internal/features/e3_edited_pic_encode_contract.md` |
| ODO and RENAMES behavior | `docs/design/NESTED_ODO_BEHAVIOR.md`, `docs/design/RENAMES_NESTED_GROUPS.md` |
| Stability and support | `docs/STABILITY_GUARANTEES.md`, `docs/SUPPORT_POLICY.md` |
| Release procedure | `docs/RELEASE_RUNBOOK.md` |

## Architecture and ownership

The principal flow is `copybook-core` (parse and schema) -> `copybook-codec`
(binary/JSON transformation) -> `copybook-cli` (orchestration). Put domain
logic in libraries rather than the CLI. Prefer an internal module before adding
a workspace crate; a public crate creates a support obligation.

Before editing, inspect the branch, status, relevant canonical documents,
nearby tests, and any existing aligned PR. Keep one review-forward concern per
PR. Update the source-truth artifact when behavior, public API, policy, support,
or release claims change. Do not tag, publish, deploy, force-push, or mutate
`origin/main` directly without explicit authorization.

## Rust and data rules

- The workspace uses Rust 2024 with MSRV 1.92. Preserve MSRV unless the change
  explicitly updates the compatibility contract.
- Keep shipped code safe and fallible: no new `unsafe`, panic-family calls, or
  unchecked indexing without a documented, governed exception.
- Preserve typed errors, causal context, and the stable error-code taxonomy.
- Treat copybooks, binary records, JSON, paths, and process input as hostile.
- Keep encode/decode output deterministic. Add focused success and failure
  coverage beside the changed seam.
- Use existing scratch-buffer and streaming patterns in hot paths; benchmark
  performance claims against the canonical receipt rather than intuition.

### Raw capture contract

- `RawMode::Record` emits the record payload as canonical `raw_b64`; legacy
  `__raw_b64` is also emitted/accepted for compatibility.
- `RawMode::RecordRDW` captures the RDW header plus record payload.
- `RawMode::Field` emits only `<FIELD_NAME>_raw_b64` values and no whole-record
  raw payload.
- `RawMode::Off` emits no raw payload.

The library API, CLI reference, and JSONL schema listed above are authoritative.

## Validation

Run the narrowest proof first, then the repository gate appropriate to risk:

```text
just test-crate <crate>
just bdd-smoke
just fmt-check
just lint
just deny
just check-msrv
just ci-quick
just pr
```

`just pr` (also exposed as `just ci`) is the canonical local PR-parity gate.
Use `just deny` for dependency or lockfile changes, `just check-msrv` for
compatibility work, and `just bdd-smoke` when governance or BDD behavior moves.
On Windows, workspace-wide rustfmt can hit OS error 206; a scoped
`cargo fmt -p <crate> --check` is useful local evidence, but report the gap and
use Linux CI for full-workspace proof.

Report checks as pass, fail, or not run. A skipped policy lane is not a pass,
and a green CI run proves only the checks that ran against that commit.

## Review, merge, and cleanup

Use Conventional Commit titles and the repository PR template. Reuse and
improve aligned inactive PRs when practical. Before committing, inspect status,
the full diff, and the staged diff; stage only the selected lane. Merge only
when the diff is reviewed, required/current checks are green, and actionable
feedback is resolved. After merge, sync the target branch and remove only the
branch, worktree, and temporary artifacts created by the lane. Capture genuine
remaining work as a focused issue instead of widening the merged concern.
