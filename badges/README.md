# Public evidence badge endpoints

These files are generated repo-scoped Shields endpoint snapshots.

| Endpoint | Provider meaning | README |
| --- | --- | --- |
| `ripr.json` | RIPR canonical actionable repair gaps | audit companion |
| `ripr-plus.json` | RIPR `ripr+` projection on the same public canonical repair basis | yes |
| `unsafe-review.json` | unsafe-review open actionable review gaps | audit companion |
| `unsafe-review-plus.json` | unsafe-review missing-or-weak evidence findings | yes |

Do not edit the counts by hand. Generate them through
`.github/workflows/evidence-badges.yml` or the local sequence in
[`docs/ci/evidence-badges.md`](../docs/ci/evidence-badges.md).

The endpoint JSON is intentionally small and public. Richer provider output stays
in the workflow artifact; the badges are projections, not certifications.

- RIPR policy: <https://github.com/EffortlessMetrics/ripr/blob/main/docs/BADGE_POLICY.md>
- unsafe-review policy: <https://github.com/EffortlessMetrics/unsafe-review/blob/main/docs/BADGE_POLICY.md>
