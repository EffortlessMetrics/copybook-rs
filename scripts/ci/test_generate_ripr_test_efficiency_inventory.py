#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

import generate_ripr_test_efficiency_inventory as subject


class RiprTestEfficiencyInventoryTests(unittest.TestCase):
    def test_discovers_supported_test_attributes_and_marks_every_entry_opaque(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "crates/demo/src/lib.rs"
            source.parent.mkdir(parents=True)
            source.write_text(
                """
#[test]
fn plain_test() {
    assert_eq!(1, 1);
}

#[tokio::test]
async fn async_test() {
    do_work().await;
}

#[rstest]
#[case(1)]
fn parameterized(#[case] value: usize) {
    assert!(value > 0);
}

// #[test]
// fn commented_out() {}

fn ordinary_function() {
    let _text = "#[test] fn fake() {}";
}
""".lstrip(),
                encoding="utf-8",
            )

            report = subject.build_report(root)

            self.assertEqual(report["schema_version"], "0.1")
            self.assertEqual(report["metrics"]["tests_scanned"], 3)
            self.assertEqual(report["metrics"]["class_counts"]["opaque"], 3)
            self.assertEqual(
                report["metrics"]["reason_counts"][
                    "opaque_helper_or_fixture_boundary"
                ],
                3,
            )
            self.assertEqual(
                [entry["name"] for entry in report["tests"]],
                ["plain_test", "async_test", "parameterized"],
            )
            self.assertTrue(
                all(entry["class"] == "opaque" for entry in report["tests"])
            )
            self.assertTrue(
                all(entry["reached_owners"] == [] for entry in report["tests"])
            )

    def test_refuses_empty_inventory(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "crates/empty/src").mkdir(parents=True)
            (root / "crates/empty/src/lib.rs").write_text(
                "pub fn ordinary() {}\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(RuntimeError, "no Rust test declarations"):
                subject.build_report(root)


if __name__ == "__main__":
    unittest.main()
