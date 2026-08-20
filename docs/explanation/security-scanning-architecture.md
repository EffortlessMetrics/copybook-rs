<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Security-scanning architecture

This document explains the security-scanning evidence flow currently shipped
by copybook-rs. It is not a security certification, compliance attestation, or
remediation guarantee.

## Scope and ownership

The canonical implementation is
[`.github/workflows/security-scan.yml`](../../.github/workflows/security-scan.yml).
It keeps four concerns separate:

1. dependency policy (`cargo deny check`);
2. raw scanner evidence (`cargo audit -q --json`);
3. deterministic normalization and validation (receipt v2); and
4. an optional GitHub issue roll-up lifecycle for eligible findings.

Raw scanner output remains diagnostic evidence. The normalized receipt is a
closed, schema-valid representation of the same scan. The issue lifecycle is a
separate operational projection and does not replace either artifact.

```text
checkout (credentials removed)
  -> advisory-db-v2 preparation -> cargo-deny check
  -> advisory-db-v2 preparation -> cargo audit -q --json
       |-> classify state and eligibility
       |-> upload cargo-audit-raw-{run_id}
       |-> generate receipt v2 from exact raw bytes
           -> semantic validation -> JSON Schema validation
           -> upload security-receipt-v2-{run_id}
                |-> eligible findings: plan/execute issue lifecycle
```

The final enforcement step runs with `if: always()` and reports policy,
scanner, classification, artifact-upload, receipt-publication, and eligible
lifecycle failures even when an earlier step fails.

## Triggers and permissions

- **Scheduled**: Mondays at `09:00 UTC` (`0 9 * * 1`). Scheduled runs pass
  `dry-run=false` to the lifecycle adapter.
- **Manual**: `workflow_dispatch` exposes a boolean `dry_run` input defaulting
  to `true`. Manual runs discover and print a plan without mutating issues
  unless an operator explicitly selects `false`.
- **Concurrency**: runs use `security-weekly-lifecycle` and do not cancel an
  in-progress run.

The workflow requests `contents: read` and `issues: write`. Checkout sets
`persist-credentials: false`; the checkout token is not retained in local Git
configuration. Issue-write permission is used only by the live lifecycle
adapter.

## Advisory database preparation

The workflow uses cache key `advisory-db-v2` and caches
`~/.cargo/advisory-db`. Before both policy and audit checks,
[`prepare_advisory_db.py`](../../scripts/ci/prepare_advisory_db.py) removes an
unusable cached checkout. It has no arbitrary production cleanup path and
leaves a valid committed advisory database available for reuse.

Both preparation steps are `continue-on-error`; later gates record whether
they were usable. `cargo-audit` is installed locked, and the weekly command is:

```text
cargo audit -q --json > target/security.audit.json
```

The audit exit code is retained. A parsed findings report and a tool/database
failure are not treated as the same state.

## Classification and raw evidence

[`classify_security_audit.py`](../../scripts/ci/classify_security_audit.py)
requires valid JSON with `vulnerabilities.list` and checks any declared count
against that list. It emits:

- `clean` when the list is empty and audit exits `0`;
- `findings` when the list is non-empty; lifecycle eligibility requires audit
  exit `1`; or
- a non-eligible tool/error condition when the report or exit status is
  inconsistent.

For findings, the lifecycle fingerprint is a SHA-256 digest of sorted
`(advisory.id, package.name, package.version)` tuples. It is independent from
the v2 `receipt_id`.

The exact raw file is uploaded as `cargo-audit-raw-{github.run_id}` with
90-day retention. Missing raw output or an upload failure fails the workflow;
raw evidence remains available if a later receipt step fails.

## Normalized receipt v2

The v2 generator is
[`generate_security_receipt.py`](../../scripts/ci/generate_security_receipt.py)
and its closed shape is
[`security-receipt-schema-v2.json`](../reference/security-receipt-schema-v2.json).
After raw upload, the workflow:

1. generates `target/security-receipt-v2.json` from the exact raw bytes;
2. runs the generator's semantic validator;
3. runs `check-jsonschema` against the v2 schema; and
4. uploads `security-receipt-v2-{github.run_id}` with 90-day retention.

The receipt contains `schema_version: "2.0"`, a deterministic `receipt_id`,
explicit commit/scan/run identity, `identity.raw_audit_sha256`, scanner
name/version/exit code, outcome state (`clean`, `findings`, or `tool_error`),
severity counts including `unknown`, and normalized findings. The generator
rejects malformed reports, inconsistent counts, unsupported values, and output
paths that alias the raw input.

JSON Schema validity alone does not prove aggregate parity; consumers should
run the semantic validator too. The v2 artifact is evidence of the executed
scanner and bytes, not authentication or proof that all vulnerabilities were
found.

The older [v1 reference](../reference/security-receipt-schema.md) remains for
compatibility. V2 does not generate deprecated v1 `compliance_metadata` or
`*_compliant` fields. Those producer-supplied fields must not be interpreted as
certification or a regulatory determination.

## Lifecycle projection

The adapter
[`security_issue_lifecycle_adapter.mjs`](../../scripts/ci/security_issue_lifecycle_adapter.mjs)
runs only after raw upload, normalized generation, both validations, and
normalized upload succeed, and only for eligible findings. It receives the raw
artifact name and independent findings fingerprint; it does not use
`receipt_id` as a finding identity.

The adapter discovers only `security`-labeled issues, paginates issues and
comments in pages of 100, rejects duplicate IDs, and trusts findings markers
only from the hardcoded `github-actions[bot]` author. The pure planner fails
closed on malformed or duplicate markers and returns one closed action:
`create`, `adopt`, `update`, `no-op`, `close`, or `reopen`. The adapter executes
writes in deterministic order. Manual dry runs return the plan with zero
writes; scheduled runs are the default live-mutation path.

The roll-up is an operational projection of findings and a link to raw
evidence. It is not a vulnerability database, remediation guarantee, or proof
of issue resolution.

## Evidence interpretation

| Evidence | Establishes | Does not establish |
| --- | --- | --- |
| Raw artifact | Exact `cargo-audit` JSON bytes uploaded by one run | Complete coverage, authenticity, or remediation |
| V2 receipt | Deterministic, semantically and schema-validated normalization linked to those bytes | Certification, absence of undiscovered vulnerabilities, or immutability |
| Lifecycle plan | Deterministic issue action for a supplied snapshot | That a write occurred |
| Lifecycle execution | Adapter-issued GitHub API writes for that run | Remediation, deployment safety, or compliance |
| Final workflow result | Configured gates and enforcement outcome | Any claim beyond the executed scanner scope |

No receipt, green workflow, or issue state is a regulatory certification,
compliance attestation, or substitute for independent security review. A
hosted witness is required for claims about a particular run; a manual witness
must not be presented as a scheduled run.

## Source references

- [Configuration how-to](../how-to/configure-security-scanning.md)
- [V2 schema](../reference/security-receipt-schema-v2.json)
- [V1 compatibility reference](../reference/security-receipt-schema.md)
- [Security policy](../../SECURITY.md)
- [Receipt generator tests](../../scripts/ci/test_generate_security_receipt.py)
- [Classifier tests](../../scripts/ci/test_classify_security_audit.py)
- [Lifecycle tests](../../scripts/ci/test_security_issue_lifecycle.mjs)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
