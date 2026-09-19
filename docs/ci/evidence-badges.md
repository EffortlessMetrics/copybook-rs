# Evidence badge endpoints

The README's first row reports three different things:

- **CI** — canonical `main` CI status.
- **ripr+** — repo-scoped RIPR actionable repair count under RIPR's badge policy.
- **unsafe-review+** — repo-scoped missing-or-weak unsafe-review evidence count.

The last two are generated evidence counters. They are not coverage percentages,
security certifications, UB-free claims, or substitutes for the repository's
other CI and security lanes.

## Public endpoint contract

The checked-in public surface is:

```text
badges/ripr.json
badges/ripr-plus.json
badges/unsafe-review.json
badges/unsafe-review-plus.json
```

Each file must be exactly a four-field Shields endpoint object:

```json
{
  "schemaVersion": 1,
  "label": "ripr+",
  "message": "123",
  "color": "orange"
}
```

The endpoint controller rejects missing files, duplicate JSON keys, non-JSON
numeric constants, unexpected fields, label drift, empty colors, and
non-numeric messages. A partial/skipped/error analyzer result therefore cannot
silently become a public count.

The README publishes only the two `+` endpoints. The corresponding base
endpoints are kept beside them because they are useful audit companions and are
produced by the same tools.

## Tool ownership

The workflow pins:

- `ripr 0.10.0`
- `unsafe-review 0.5.0`

`unsafe-review+` is generated directly by the released CLI:

```bash
unsafe-review badges --root . --out <dir>
```

That is the provider-owned repo projection.

### RIPR test-efficiency portability boundary

`ripr+` requires `target/ripr/reports/test-efficiency.json`. In RIPR 0.10.0,
the richer producer for that report is still a RIPR-repository-private xtask;
the public RIPR CLI does not expose a portable downstream producer.

copybook-rs therefore uses
`scripts/ci/generate_ripr_test_efficiency_inventory.py` as a deliberately
conservative bridge. It inventories real Rust test declarations under
`crates/`, `tools/`, and `tests/`, then marks every entry `opaque`.
It does **not** copy RIPR's private heuristics or infer discriminator strength,
reached owners, activation values, or strong-test status.

That choice is fail-closed. It gives RIPR the real downstream test inventory
required to validate the `ripr+` fact source without manufacturing evidence.
Under RIPR 0.10.0's canonical repo-badge basis, raw test-efficiency inventory
does not move the public headline until a producer lifts those items into the
same repair / verify / receipt model as canonical actionable gaps.

When RIPR exposes a portable public test-efficiency producer, replace this
bridge rather than evolving it into a second RIPR analyzer.

## Generation and refresh

`.github/workflows/evidence-badges.yml` owns the lifecycle.

On relevant pull requests it:

1. runs the Python contract tests;
2. installs the pinned evidence tools;
3. generates the conservative RIPR test inventory;
4. generates native RIPR audit artifacts plus all four public endpoint
   candidates;
5. validates the endpoint contract;
6. compares generated values with committed `badges/*.json` when the
   committed endpoints exist;
7. uploads the full generation packet as an Actions artifact.

On the weekly schedule or a manual dispatch, the same generation job runs from
`main`. If endpoint values changed, the refresh job writes only
`badges/*.json` to the dedicated `automation/evidence-badges` branch and
creates or updates a narrow refresh pull request.

Ordinary product pull requests do not silently rewrite badge counts.

## Local commands

After installing the pinned tools:

```bash
python3 scripts/ci/test_generate_ripr_test_efficiency_inventory.py
python3 scripts/ci/test_evidence_badges.py

python3 scripts/ci/evidence_badges.py generate \
  --root . \
  --output target/evidence-badges

python3 scripts/ci/evidence_badges.py check \
  target/evidence-badges badges
```

To intentionally refresh checked-in endpoints:

```bash
python3 scripts/ci/evidence_badges.py publish \
  target/evidence-badges badges
```

Review the four-file diff before committing it.

## Trust boundaries

The badges keep their provider meanings:

- RIPR badge policy: <https://github.com/EffortlessMetrics/ripr/blob/main/docs/BADGE_POLICY.md>
- unsafe-review badge policy: <https://github.com/EffortlessMetrics/unsafe-review/blob/main/docs/BADGE_POLICY.md>

The existing Codecov integration and weekly security scanning remain in the
repository. Removing their README badges did not remove those evidence planes.
