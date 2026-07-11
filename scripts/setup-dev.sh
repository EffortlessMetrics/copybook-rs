#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# One-shot bootstrap for local copybook-rs development.
# Installs the cargo subcommands referenced by the justfile so `just <recipe>`
# works out of the box after a fresh clone.
set -euo pipefail

if command -v cargo.exe >/dev/null 2>&1; then
  CARGO_BIN="cargo.exe"
else
  CARGO_BIN="cargo"
fi

# name -> crate name (kept separate in case they ever diverge)
TOOLS=(
  "cargo-nextest:cargo-nextest"
  "cargo-deny:cargo-deny"
  "cargo-watch:cargo-watch"
  "cargo-llvm-cov:cargo-llvm-cov"
  "cargo-mutants:cargo-mutants"
)

install_one() {
  local bin_name="$1" crate="$2"
  if command -v "$bin_name" >/dev/null 2>&1; then
    echo "==> $bin_name already installed, skipping"
    return
  fi
  echo "==> Installing $crate"
  if command -v cargo-binstall >/dev/null 2>&1; then
    "$CARGO_BIN" binstall --no-confirm "$crate"
  else
    "$CARGO_BIN" install --locked "$crate"
  fi
}

echo "==> copybook-rs dev environment setup"
for entry in "${TOOLS[@]}"; do
  install_one "${entry%%:*}" "${entry##*:}"
done

if ! command -v just >/dev/null 2>&1; then
  echo "NOTE: 'just' is not installed. Install it from https://github.com/casey/just"
  echo "      or run: cargo install just"
fi

echo "==> Done. Try 'just ci-quick' to validate your setup."
