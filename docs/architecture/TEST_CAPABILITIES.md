<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Test capability ownership

Repository validation selects real capabilities deliberately. It does not add no-op Cargo features merely so one command can be applied to every package.

| Capability | Owner | Normal lane | Contract |
| --- | --- | --- | --- |
| Default stable tests | whole workspace | blocking PR/main | Advertised stable behavior and normal package defaults |
| Comprehensive parser scenarios | `copybook-core` | `CI Comprehensive` parser matrix entry | Aspirational and extended parser scenarios behind the real `comprehensive-tests` feature |
| Comprehensive codec scenarios | `copybook-codec` | `CI Comprehensive` codec matrix entry | Extended ODO, REDEFINES, RDW, and numeric scenarios behind the real `comprehensive-tests` feature |
| BDD | `copybook-bdd` | dedicated BDD lane | Cucumber harness, not a nextest-compatible ordinary test target |
| Property tests | `copybook-proptest` plus owner integration tests | dedicated property lane | Governed seeds, case counts, and committed regressions |
| Fuzzing | standalone `fuzz/` workspace | fuzz integration/dispatch | Owner-targeted fuzz binaries, not runtime feature flags |
| RIPR | repository control plane | advisory PR/weekly | Diff-scoped test-oracle work orders |
| cargo-mutants | local deliberate tooling | `just mutants` | Not a routine hosted-CI capability |
| Arrow, audit, and metrics | owning adapter/application | dedicated feature lanes | Optional product or operational surfaces, not stable-default proof |

Only `copybook-core` and `copybook-codec` own the `comprehensive-tests` Cargo feature because those packages contain tests gated by it. Coverage and the normal PR gate use default product features; comprehensive and experimental surfaces run in explicitly named lanes.

The owner-specific proof commands are:

```bash
cargo nextest run -p copybook-core \
  --features comprehensive-tests \
  --profile ci \
  --no-fail-fast

cargo nextest run -p copybook-codec \
  --features comprehensive-tests \
  --profile ci \
  --no-fail-fast
```

`.github/workflows/ci-comprehensive.yml` runs those packages as separate, non-fail-fast matrix jobs on nightly schedule, manual dispatch, or a pull request carrying the `comprehensive` label. Each job records its advisory outcome in the step summary and uploads its own nextest receipt; parser and codec failures cannot cancel one another.

Removing a no-op feature from a publishable package is an intentional next-release surface cleanup. It must not be represented as a 0.5 patch-release compatibility promise.
