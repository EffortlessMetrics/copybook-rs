# SPDX-License-Identifier: AGPL-3.0-or-later
"""Remove an unusable cargo-audit advisory database before audit initializes it.

The command intentionally has no path override: production cleanup is limited
to ``$CARGO_HOME/advisory-db``, cargo-audit's own database location. The
``remove_if_unusable`` helper accepts a path only so fixture tests can exercise
the cleanup behavior without touching a real cargo home.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
from pathlib import Path


def _is_valid_git_checkout(path: Path) -> bool:
    """Return whether *path* is a usable, committed Git checkout."""
    # Git discovers repositories in parent directories. An incomplete cache
    # below a project checkout must not borrow that project's HEAD. A .git
    # file is valid here too: linked worktrees do not have a .git directory.
    if not path.is_dir() or not (path / ".git").exists():
        return False

    top_level = subprocess.run(
        ["git", "-C", str(path), "rev-parse", "--show-toplevel"],
        capture_output=True,
        check=False,
        text=True,
    )
    if top_level.returncode != 0 or not top_level.stdout.strip():
        return False
    try:
        if not path.samefile(top_level.stdout.strip()):
            return False
    except OSError:
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
    # Do not let validation follow a symlink into an unrelated repository.
    # The cache path itself is the only object this helper may remove.
    if path.is_symlink():
        path.unlink()
        return True
    if not path.exists():
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
    parser.parse_args()
    path = default_advisory_db()

    removed = remove_if_unusable(path)
    if removed:
        print(f"Removed unusable cargo-audit advisory database: {path}")
    else:
        print(f"Cargo-audit advisory database is reusable or absent: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
