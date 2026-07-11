# API Freeze Audit Checklist

This checklist is used during the pre-v1.0.0 stability freeze window for documentation-led work only.

## Purpose

Before freeze closure, confirm these items are completed and evidenced in repository artifacts:

1. Public API audit completed.
2. Any deprecations have migration paths documented.
3. Stability guarantees are published and easy to find.
4. API changelog entries include migration notes where needed.
5. Support policy documents a 6-month previous-minor support window.

## 1) Public API audit

- Run API compatibility check against the active freeze baseline:
  - `just api-freeze-status`
  - `just api-check`
- Confirm no API shape changes are present while freeze is active:
  - `copybook-core/`, `copybook-codec/`, `copybook-cli/` are lock-scoped
  - docs, bench, tests, and CI/tooling are the only allowed changed domains
- Capture or link the CI run output showing freeze PASS/FAIL status.

## 2) Deprecation and migration path

- Confirm every deprecated public API has:
  - `#[deprecated(...)]` annotation (or explicit rationale)
  - replacement guidance in CHANGELOG and/or API docs
- Run a quick declaration scan for deprecated markers:

```bash
rg "#\\[deprecated" copybook-* -g "*.rs"
```

- Keep the canonical machine-readable deprecation inventory in:

```bash
docs/reports/deprecation-audit.json
```

- Validate the file is complete and consistent by running:

```bash
rtk cargo run -p xtask -- docs verify-all
```

- For each deprecation, ensure migration notes include a version and a sunset expectation.

## 3) Stability guarantees visibility

- Ensure these documents remain linked from README and the docs index:
  - `docs/STABILITY_GUARANTEES.md`
  - `docs/API_FREEZE.md`
  - `docs/SUPPORT_POLICY.md`
- Confirm that public claims match what these documents state.

## 4) API changelog and migration notes

- Update `CHANGELOG.md` for any API-impacting behavior change, including:
  - removed fields, signatures, trait changes
  - newly deprecated items
  - replacement flow before API removals
- Mark version scope clearly for each item (e.g. `v0.5.0`, `v1.0.0`).

## 5) Support policy check

- Confirm support policy states:
  - latest minor support window
  - previous-minor 6-month security window
  - security/critical bug patch expectations
- For v1.0+, align any policy changes with issue owner and release owner before freeze close.

## Exit evidence

- Attach to the freeze-tracking issue:
  - API check output (pass)
  - changelog diff links
  - README/doc index diff
  - freeze diff summary showing docs/bench/tests-only scope

## Scope reminder

During freeze, PRs that change public API shapes must lift the freeze before merge unless they are explicitly scoped as planned release exceptions.
