# SPDX-License-Identifier: AGPL-3.0-or-later
"""Remove an unusable cargo-audit advisory database before audit initializes it."""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
from pathlib import Path


def _is_valid_git_checkout(path: Path) -> bool:
    """Return whether *path* is a usable, committed Git checkout."""
    if not path.is_dir():
        return False

    inside_worktree = subprocess.run(
        ["git", "-C", str(path), "rev-parse", "--is-inside-work-tree"],
        capture_output=True,
        check=False,
        text=True,
    )
    if inside_worktree.returncode != 0 or inside_worktree.stdout.strip() != "true":
        return False

    head = subprocess.run(
        ["git", "-C", str(path), "rev-parse", "--verify", "HEAD^{commit}"],
        capture_output=True,
        check=False,
        text=True,
    )
    return head.returncode == 0 and bool(head.stdout.strip())


def remove_if_unusable(path: Path) -> bool:
    """Remove *path* when it is not a valid advisory-db checkout.

    The path is deliberately caller-supplied and narrowly scoped to the
    cargo-audit database. A valid checkout is left untouched for cache reuse.
    """
    if not path.exists() and not path.is_symlink():
        return False
    if _is_valid_git_checkout(path):
        return False

    if path.is_symlink() or path.is_file():
        path.unlink()
    else:
        shutil.rmtree(path)
    return True


def default_advisory_db() -> Path:
    """Return cargo-audit's default advisory database path."""
    cargo_home = Path(os.environ.get("CARGO_HOME", Path.home() / ".cargo"))
    return cargo_home / "advisory-db"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--path", type=Path, default=default_advisory_db())
    args = parser.parse_args()

    removed = remove_if_unusable(args.path)
    if removed:
        print(f"Removed unusable cargo-audit advisory database: {args.path}")
    else:
        print(f"Cargo-audit advisory database is reusable or absent: {args.path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
