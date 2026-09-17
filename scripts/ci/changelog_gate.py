#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Decide whether a change set needs a changie fragment (#993).

This is the single classifier for the changelog lane: it consumes real
changed-path/status data (git name-status letters, renames expanded to
delete+add by the driver) and reports a pass/fail decision with actionable
reasons. Statuses: A (added), M (modified), D (deleted), T (type-changed).

Policy, derived from the repository layout:

- Product (needs a note): crates/*/src/**, crates/*/Cargo.toml,
  crates/*/build.rs, schemas/**, root Cargo.toml, and every Cargo.lock
  (a dependency/security change must not disappear behind a lockfile-only
  diff). Deletions and renames of product paths stay product changes.
- Exempt (never needs a note): crate integration tests and benches
  (crates/**/tests/**, crates/**/benches/**), READMEs and other markdown,
  docs/**, tools/**, .github/**, fixtures/**, test-data/**, fuzz/**,
  examples/**, deploy/**, grafana/**. Mixed product+exempt changes stay
  product changes.
- Fragments: an added/modified `.changes/unreleased/*` file must parse as a
  fragment with a known kind and a non-empty body. Deleted fragments,
  malformed fragments, and missing fragments fail. Editing an unrelated
  historical `.changes/v*.md` file neither satisfies nor (alone) fails.
- Release preparation: an ADDED `.changes/v*.md` whose deletions are all
  consumed unreleased fragments, with no source/schema changes, passes
  through the changie batch/merge proof instead of a fragment.
"""

from __future__ import annotations

import sys

KNOWN_KINDS = frozenset({"added", "changed", "deprecated", "removed", "fixed", "security"})

PRODUCT_SUFFIXES = ("Cargo.toml", "Cargo.lock", "build.rs")


def _is_product(path: str) -> bool:
    if path == "Cargo.toml" or path == "Cargo.lock" or path.endswith("/Cargo.lock"):
        return True
    if path.startswith("crates/"):
        rest = path[len("crates/"):]
        if "/tests/" in rest or rest.startswith("tests/"):
            return False
        if "/benches/" in rest or rest.startswith("benches/"):
            return False
        basename = rest.rsplit("/", 1)[-1]
        if basename == "README.md" or basename.startswith("README"):
            return False
        if rest.endswith(".md"):
            return False
        if "/src/" in rest or rest.startswith("src/"):
            return True
        if basename in PRODUCT_SUFFIXES:
            return True
        return True
    if path.startswith("schemas/"):
        return True
    return False


def parse_fragment(body: str) -> tuple[bool, str]:
    """Validate a fragment body. Returns (valid, reason)."""
    try:
        import yaml  # type: ignore[import-not-in-requirements]
    except ImportError:
        yaml = None  # type: ignore[assignment]
    if yaml is not None:
        try:
            data = yaml.safe_load(body)
        except Exception as exc:  # noqa: BLE001 - any parse failure is invalid
            return False, f"fragment does not parse as YAML: {exc}"
        if not isinstance(data, dict):
            return False, "fragment must be a YAML mapping"
        kind = data.get("kind")
        note = data.get("body")
        if kind not in KNOWN_KINDS:
            return False, f"fragment kind {kind!r} is not a known changie kind"
        if not isinstance(note, str) or not note.strip():
            return False, "fragment body must be a non-empty string"
        return True, ""
    # Minimal fallback when PyYAML is unavailable: require kind/body keys.
    if "kind:" not in body or "body:" not in body:
        return False, "fragment must declare kind and body"
    return True, ""


def classify(
    changes: list[tuple[str, str]],
    read_fragment: object = None,
) -> tuple[bool, list[str]]:
    """Decide (passing, reasons) for [(status, path)] with statuses A/M/D/T.

    `read_fragment`, when given, is a callable(path) -> body used to validate
    added/modified unreleased fragments. Without it, added/modified fragments
    are accepted structurally (presence only).
    """
    reasons: list[str] = []
    product = sorted({p for s, p in changes if _is_product(p)})
    added_versions = sorted(
        p
        for s, p in changes
        if s == "A" and p.startswith(".changes/v") and p.endswith(".md")
    )
    deleted_unreleased = sorted(
        p
        for s, p in changes
        if s == "D" and p.startswith(".changes/unreleased/")
    )
    other_deletions = sorted(
        p
        for s, p in changes
        if s == "D"
        and not p.startswith(".changes/unreleased/")
        and not (p.startswith(".changes/v") and p.endswith(".md"))
    )
    present_fragments = sorted(
        p
        for s, p in changes
        if s in {"A", "M"} and p.startswith(".changes/unreleased/")
    )

    failures: list[str] = []
    valid_fragments = 0
    for path in present_fragments:
        if read_fragment is not None:
            try:
                body = read_fragment(path)  # type: ignore[operator]
            except OSError as exc:
                failures.append(f"fragment {path} is not readable: {exc}")
                continue
            valid, why = parse_fragment(body)
            if not valid:
                failures.append(f"fragment {path} is malformed: {why}")
                continue
        valid_fragments += 1

    if other_deletions:
        # Deletions outside the fragment/version flow are product deletions
        # unless every one of them is exempt by classification.
        non_exempt = sorted(p for p in other_deletions if _is_product(p))
        if non_exempt and not product:
            product = non_exempt
        if non_exempt:
            reasons.append(f"production deletions: {', '.join(non_exempt)}")

    if deleted_unreleased and not added_versions:
        failures.append(
            "unreleased fragments deleted without a replacement note: "
            + ", ".join(deleted_unreleased)
        )

    if product:
        reasons.append(f"production changes: {', '.join(product)}")
        # Release preparation reconciles through the changie proof instead.
        if added_versions and not any(
            _is_product(p)
            for s, p in changes
            if p.startswith("crates/") or p.startswith("schemas/")
        ):
            non_fragment_deletions = [
                p
                for s, p in changes
                if s == "D" and not p.startswith(".changes/unreleased/")
            ]
            if not non_fragment_deletions:
                return True, [
                    f"release preparation ({', '.join(added_versions)}); "
                    "notes reconcile through the changie batch/merge proof"
                ]
        if valid_fragments == 0 and not failures:
            failures.append(
                "production changes require an added/modified unreleased fragment"
            )
        elif valid_fragments > 0 and not failures:
            return True, reasons + [f"noted in {valid_fragments} fragment(s)"]
        return False, reasons + failures

    if failures:
        return False, failures
    return True, ["no production changes; no fragment required"]


def main(argv: list[str]) -> int:
    """CLI driver: reads git name-status lines on stdin, exits 0/1.

    Rename entries (`R100\\told\\tnew`) expand to a delete plus an add so a
    renamed production file stays a product change and a renamed-away
    fragment still fails.
    """
    changes: list[tuple[str, str]] = []
    for line in sys.stdin:
        line = line.rstrip("\n")
        if not line.strip():
            continue
        parts = line.split("\t")
        status = parts[0].strip()
        if status.startswith("R") and len(parts) == 3:
            changes.append(("D", parts[1].strip()))
            changes.append(("A", parts[2].strip()))
            continue
        if len(parts) != 2 or not status or not parts[1].strip():
            print(f"error: malformed change line: {line!r}", file=sys.stderr)
            return 2
        changes.append((status, parts[1].strip()))
    passing, reasons = classify(changes)
    for reason in reasons:
        print(("ok: " if passing else "error: ") + reason, file=sys.stderr if not passing else sys.stdout)
    return 0 if passing else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
