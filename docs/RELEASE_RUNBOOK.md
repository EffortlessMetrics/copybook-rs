# Release Runbook

**Purpose**: Repeatable release process for publishing the copybook-rs crates to crates.io.

**Canonical status**: `docs/ROADMAP.md`

---

## 1) Choose the Tag Commit

- Land on `main` (no local-only commits).
- Ensure a clean working tree: `git status`.

---

## 2) Prove the Commit (Required)

Run the same gates CI expects on the exact commit you intend to tag:

```bash
# Lint → clippy → build → nextest → doctests
bash scripts/ci/quick.sh

# Security sweep (deny always; audit only on Cargo.lock diffs)
BASE_SHA=origin/main HEAD_SHA=HEAD bash scripts/ci/security.sh

# Docs-truth
cargo run -p xtask -- docs verify-support-matrix
```

If any of these fail on the tag commit, do not ship.

---

## 3) Preflight crates.io Packaging (Required)

On the tag commit:

```bash
cargo publish -p copybook-core --dry-run
```

**Note (workspace dependency ordering)**: `copybook-codec` depends on `copybook-core`, and `copybook-cli` depends on both. `cargo publish --dry-run` for downstream crates requires their dependencies to exist on crates.io, so run these dry-runs **just-in-time** during the publish sequence:

1. Publish `copybook-core`
2. Wait for crates.io index propagation
3. Dry-run (then publish) `copybook-codec`
4. Wait for crates.io index propagation
5. Dry-run (then publish) `copybook-cli`

The repository publish workflow (`.github/workflows/publish.yml`) follows this order.

---

## 4) Tag + Publish

- Ensure `CHANGELOG.md` matches the tag date (or keep it `Unreleased` until you tag).
- Create the tag:
  ```bash
  git tag -a v0.4.0 -m "v0.4.0"
  git push origin main --tags
  ```
- Publish in order:
  1. `copybook-core`
  2. `copybook-codec`
  3. `copybook-cli`

---

## 5) Post-Release Verification

- docs.rs builds complete for:
  - `copybook-core`
  - `copybook-codec`
  - `copybook-cli`
- Install works on a clean machine:
  ```bash
  cargo install copybook-cli@0.4.0
  ```

---

## Rollback (If Needed)

If you published a bad release:

```bash
# Yank in reverse dependency order
cargo yank copybook-cli@0.4.0
cargo yank copybook-codec@0.4.0
cargo yank copybook-core@0.4.0
```

Then cut a patch release with fixes and re-run this runbook.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
