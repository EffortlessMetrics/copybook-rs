#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the changelog classifier (scripts/ci/changelog_gate.py).

Each case mirrors one row of the #993 acceptance matrix.
"""

from __future__ import annotations

import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

from changelog_gate import classify  # noqa: E402


VALID = "kind: fixed\nbody: some user-facing note\n"
BAD_KIND = "kind: frobnicate\nbody: some note\n"
NO_BODY = "kind: fixed\n"


def files(*pairs: str) -> list[tuple[str, str]]:
    return [("A" if p[0] == "+" else "D", p[1:]) for p in pairs]


class ChangelogGateTests(unittest.TestCase):
    def test_prod_source_without_note_fails(self) -> None:
        passing, reasons = classify(files("+crates/copybook-core/src/lib.rs"))
        self.assertFalse(passing)
        self.assertTrue(any("production changes" in r for r in reasons))

    def test_prod_source_with_valid_note_passes(self) -> None:
        passing, _ = classify(
            files(
                "+crates/copybook-core/src/lib.rs",
                "+.changes/unreleased/fixed-thing.yaml",
            ),
            read_fragment=lambda p: VALID,
        )
        self.assertTrue(passing)

    def test_lockfile_only_without_note_fails(self) -> None:
        passing, _ = classify(files("+Cargo.lock"))
        self.assertFalse(passing)

    def test_readme_only_passes(self) -> None:
        passing, _ = classify(files("+crates/copybook-core/README.md"))
        self.assertTrue(passing)

    def test_integration_test_only_passes(self) -> None:
        passing, _ = classify(files("+crates/copybook-core/tests/deep.rs"))
        self.assertTrue(passing)

    def test_mixed_prod_and_test_without_note_fails(self) -> None:
        passing, _ = classify(
            files("+crates/copybook-core/src/lib.rs", "+crates/copybook-core/tests/deep.rs")
        )
        self.assertFalse(passing)

    def test_prod_deletion_without_note_fails(self) -> None:
        passing, reasons = classify(files("-crates/copybook-core/src/old.rs"))
        self.assertFalse(passing)
        self.assertTrue(any("deletion" in r for r in reasons))

    def test_prod_rename_without_note_fails(self) -> None:
        # Renames arrive expanded as delete+add by the driver.
        passing, _ = classify(
            files("-crates/copybook-core/src/old.rs", "+crates/copybook-core/src/new.rs")
        )
        self.assertFalse(passing)

    def test_deleted_fragment_fails(self) -> None:
        passing, _ = classify(files("-.changes/unreleased/fixed-thing.yaml"))
        self.assertFalse(passing)

    def test_malformed_fragment_fails(self) -> None:
        passing, reasons = classify(
            files(
                "+crates/copybook-core/src/lib.rs",
                "+.changes/unreleased/fixed-thing.yaml",
            ),
            read_fragment=lambda p: BAD_KIND,
        )
        self.assertFalse(passing)
        self.assertTrue(any("malformed" in r for r in reasons))

    def test_bodyless_fragment_fails(self) -> None:
        passing, _ = classify(
            files("+.changes/unreleased/fixed-thing.yaml"),
            read_fragment=lambda p: NO_BODY,
        )
        # No production changes, but the present fragment is malformed.
        self.assertFalse(passing)

    def test_historical_version_edit_alone_is_exempt(self) -> None:
        passing, _ = classify(files("+.changes/v0.8.0.md"))
        self.assertTrue(passing)

    def test_release_prep_passes_without_fragment(self) -> None:
        passing, reasons = classify(
            files(
                "+.changes/v0.8.1.md",
                "-.changes/unreleased/fixed-a.yaml",
                "-.changes/unreleased/added-b.yaml",
                "+CHANGELOG.md",
                "+Cargo.toml",
                "+Cargo.lock",
                "+README.md",
            )
        )
        self.assertTrue(passing, reasons)
        self.assertTrue(any("release preparation" in r for r in reasons))

    def test_release_prep_with_source_changes_still_needs_note(self) -> None:
        passing, _ = classify(
            files(
                "+.changes/v0.8.1.md",
                "-.changes/unreleased/fixed-a.yaml",
                "+crates/copybook-core/src/lib.rs",
            )
        )
        self.assertFalse(passing)

    def test_docs_only_passes(self) -> None:
        passing, _ = classify(files("+docs/foo.md", "+tools/x.sh", "+.github/w.yml"))
        self.assertTrue(passing)

    def test_schema_change_without_note_fails(self) -> None:
        passing, _ = classify(files("+schemas/x.json"))
        self.assertFalse(passing)


if __name__ == "__main__":
    unittest.main()
