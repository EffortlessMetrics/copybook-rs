<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CI Labels

Labels control which deep validation lanes are activated on a PR.
They do not replace the ordinary PR gate; they add lanes on top of it.

## Label Definitions

### Cost-Override Labels

| Label | Semantics |
|-------|-----------|
| `full-ci` | Run all lanes including every deep validation lane. LEM ceiling suspended. |
| `ci:full` | Alias for `full-ci`. |
| `ci-budget-ack` | Acknowledges elevated LEM spend; suppresses soft warning. |
| `ci-budget-override` | Overrides hard LEM ceiling (>125 LEM). PR body must justify. |

### Platform Labels

| Label | Lanes Activated |
|-------|----------------|
| `platform-matrix` | macOS, Windows, beta toolchain, full OS/toolchain matrix |
| `wasm` | WASM build and wasm-pack tests |
| `arrow` | Arrow integration tests and Arrow schema validation |

### Verification Depth Labels

| Label | Lanes Activated |
|-------|----------------|
| `coverage` | tarpaulin or llvm-cov coverage run; uploads to coverage service |
| `mutation` | cargo-mutants mutation testing |
| `fuzz` | libfuzzer/cargo-fuzz integration |
| `property-tests` | Heavy proptest runs with increased case counts |

### Domain Labels

| Label | Lanes Activated |
|-------|----------------|
| `security-audit` | Full cargo-deny, cargo-audit, SBOM, and security-scan workflow |
| `release-check` | Publish dry-run, API freeze check, changelog validation |
| `perf` | Benchmark suite, enterprise perf, perf-validation workflow |
| `ripr` | ripr static exposure analysis (also runs on parser/codec risk packs by default) |
| `ripr-waive` | Suppresses ripr advisory finding for this PR |

## Label Semantics Rules

1. Labels are additive. Adding `coverage` does not remove any other lane.
2. `full-ci` is the superset. No other label is needed when `full-ci` is present.
3. `ripr-waive` suppresses ripr; it does not suppress any other lane.
4. `ci-budget-override` requires justification in the PR body. The PR
   description should explain why the spend is warranted.
5. Labels applied by bots (e.g., auto-labelers) do not count as owner
   acknowledgement for cost-override labels. A human must apply
   `ci-budget-ack` and `ci-budget-override`.

## Labels and Branch Protection

After PR 17 (branch protection migration), only `PR Gate Success` is a
required check. Labels control whether deep lanes run; they do not change
what is required for merge.

Exception: `release-check` lanes may become required checks on the
`release/*` branch pattern in a future phase.

## Label Reference in Policy TOML

Labels are defined in `policy/ci-budget.toml` under `[labels]`.
Each lane in `policy/ci-lane-whitelist.toml` references the labels that
activate it via the `labels` array field.
