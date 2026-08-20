<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Configure and inspect security scanning

This guide describes the security-scanning workflow already present in
copybook-rs. It does not add a workflow, create a receipt by hand, or establish
compliance. For architecture and claim boundaries, see the
[security-scanning architecture](../explanation/security-scanning-architecture.md).

## Before you start

You need a repository checkout, GitHub Actions access, and (for local proof)
Python 3 and Node.js. The workflow installs `cargo-deny`, `cargo-audit`, and
`check-jsonschema` itself. Do not add a second security job to `ci.yml`; the
weekly workflow is owned by
[`.github/workflows/security-scan.yml`](../../.github/workflows/security-scan.yml).

## Trigger a run

The workflow runs Mondays at `09:00 UTC`.

Dispatch a non-mutating manual run with the default dry-run behavior:

```bash
gh workflow run security-scan.yml --ref main -f dry_run=true
```

To explicitly request live issue lifecycle writes, use `-f dry_run=false` only
when that external mutation is intended. Scheduled runs pass `dry-run=false`;
manual runs do not mutate issues unless the input is explicitly false. A
manual dry run is not evidence that a scheduled run completed.

The workflow has `contents: read` and `issues: write` permissions. Checkout
uses `persist-credentials: false`. Do not broaden these permissions for local
inspection or artifact download.

## Run the offline contracts

From the repository root, run the focused tests:

```bash
python3 -m unittest \
  scripts.ci.test_generate_security_receipt \
  scripts.ci.test_classify_security_audit
node --test scripts/ci/test_security_issue_lifecycle.mjs
```

These tests cover clean, findings, and tool-error states; malformed and
inconsistent raw reports; v2 identity and hash linkage; schema/semantic
validation; output alias rejection; lifecycle pagination, trust, markers,
duplicate IDs, dry-run behavior, and deterministic actions.

The generator CLI can be exercised with a checked-in raw fixture:

```bash
python3 scripts/ci/generate_security_receipt.py generate \
  tests/fixtures/security-scanning/raw-audit/clean.json \
  --commit-sha 0000000000000000000000000000000000000000 \
  --scan-type weekly-scan \
  --workflow-run-id local-example \
  --cargo-audit-version 0.21.2 \
  --audit-exit-code 0 \
  --output target/security-receipt-v2-local.json

python3 scripts/ci/generate_security_receipt.py validate \
  target/security-receipt-v2-local.json
check-jsonschema \
  --schemafile docs/reference/security-receipt-schema-v2.json \
  target/security-receipt-v2-local.json
```

The fixture path and tool version are illustrative local inputs. A local
receipt is not a hosted workflow artifact. The generator reads raw bytes and
rejects an output path that aliases the raw input.

## Understand the workflow stages

The workflow uses cache key `advisory-db-v2` and `~/.cargo/advisory-db`. Before
both policy and audit checks it runs
[`prepare_advisory_db.py`](../../scripts/ci/prepare_advisory_db.py), which
removes only an unusable advisory database checkout. The audit command is:

```text
cargo audit -q --json > target/security.audit.json
```

The classifier
[`classify_security_audit.py`](../../scripts/ci/classify_security_audit.py)
requires valid JSON and a consistent `vulnerabilities.list`:

- `clean`: no findings and audit exit code `0`;
- `findings`: one or more findings; lifecycle eligibility requires audit exit
  code `1`; and
- tool/error or inconsistent output: not eligible for lifecycle mutation.

The workflow uploads `cargo-audit-raw-{workflow-run-id}` with 90-day retention,
then generates, semantically validates, schema-validates, and uploads
`security-receipt-v2-{workflow-run-id}`, also with 90-day retention. The raw
artifact is never overwritten by normalized output.

## Download and verify a hosted run

Replace `<run-id>` with the actual workflow run ID:

```bash
gh run view <run-id> --workflow security-scan.yml
gh run download <run-id> \
  --name cargo-audit-raw-<run-id> \
  --name security-receipt-v2-<run-id> \
  --dir evidence
```

Validate the normalized receipt with both validators:

```bash
python3 scripts/ci/generate_security_receipt.py validate \
  evidence/security-receipt-v2-<run-id>/security-receipt-v2.json
check-jsonschema \
  --schemafile docs/reference/security-receipt-schema-v2.json \
  evidence/security-receipt-v2-<run-id>/security-receipt-v2.json
```

Compute SHA-256 over the exact downloaded raw JSON bytes and compare it with
`identity.raw_audit_sha256` in the normalized receipt. Preserve the exact run
ID, commit SHA, artifact names, and commands in any evidence note. A checkout
or platform conversion that changes line endings changes the raw-byte hash;
use bytes downloaded from the workflow for this check.

The receipt's `receipt_id` is a deterministic identity for schema version,
explicit scan identity, and scanner tuple. It is not a signature or a digest
of findings. The lifecycle fingerprint is independent and uses sorted
advisory/package/version identities.

## Lifecycle dry-run and mutation

Lifecycle execution is gated on successful raw upload, receipt generation,
semantic validation, schema validation, normalized upload, and eligible
classification. The adapter then:

1. lists only `security`-labeled issues;
2. fetches all issue/comment pages (100 items per page);
3. rejects duplicate IDs and malformed snapshots;
4. trusts findings markers only from `github-actions[bot]`; and
5. asks the pure planner for `create`, `adopt`, `update`, `no-op`, `close`, or
   `reopen`.

Manual dry runs perform discovery and planning but zero writes. Live execution
writes only the closed plan action and an optional findings comment. The
adapter fails closed when the scan is not eligible or the snapshot/markers are
malformed. A roll-up issue records operational state; it does not prove
remediation or compliance.

## Troubleshooting

### Advisory database preparation fails

Inspect the `advisory_db` and `advisory_db_before_deny` outcomes and the final
enforcement step. The helper intentionally has no arbitrary path option. Re-run
after the cache is rebuilt; do not add a second cleanup implementation.

### Raw artifact is missing

Final enforcement fails if raw upload is not successful. Check that
`target/security.audit.json` was produced and that the name is
`cargo-audit-raw-<run-id>`. Missing raw evidence prevents lifecycle execution.

### Receipt validation fails

Keep the raw artifact. Run semantic validation first, then `check-jsonschema`
against the v2 schema. Common causes are malformed `vulnerabilities.list`, a
count mismatch, unsupported severity, invalid identity, or output aliasing.
Receipt publication failure prevents lifecycle mutation.

### Lifecycle does not run

This is expected for a clean scan, an ineligible tool/error state, or any run
where raw/normalized evidence gates did not succeed. For a manual run, verify
that `dry_run` was explicitly set to `false` before expecting writes. Inspect
classifier outputs and the lifecycle step rather than inferring issue state
from a receipt.

## V1 and V2 transition

The v1 reference and fixtures remain for compatibility. V1 contains deprecated
compliance-shaped producer fields for compatibility only; they are not
generated by the v2 workflow and must not be treated as attestations. New
weekly evidence uses the closed v2 schema and paired raw artifact. See the
[v1 reference](../reference/security-receipt-schema.md) and
[v2 schema](../reference/security-receipt-schema-v2.json).

## Claim boundary

The workflow and artifacts prove only what the configured commands executed for
the observed run and inputs. They do not establish absence of unknown
vulnerabilities, complete severity classification, remediation, artifact
immutability or authentication, regulatory certification, compliance
attestation, or release readiness. A green run is not a substitute for
independent security review.

## Related documentation

- [Security-scanning architecture](../explanation/security-scanning-architecture.md)
- [V1 receipt reference](../reference/security-receipt-schema.md)
- [V2 receipt schema](../reference/security-receipt-schema-v2.json)
- [Security policy](../../SECURITY.md)
- [Release runbook](../RELEASE_RUNBOOK.md)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
