#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"

bootstrap_rustup() {
  if [ -n "${HOME:-}" ] && [ -f "$HOME/.cargo/env" ]; then
    # Non-login bash shells may not populate Rust's shims on PATH.
    . "$HOME/.cargo/env"
  fi
}

run_cargo() {
  bootstrap_rustup
  local toolchain="${COPYBOOK_RUST_TOOLCHAIN:-stable}"

  if command -v rustup >/dev/null 2>&1 && rustup toolchain list | grep -q "^${toolchain}"; then
    rustup run "$toolchain" cargo "$@"
  elif command -v rustup.exe >/dev/null 2>&1 && rustup.exe toolchain list | grep -q "^${toolchain}"; then
    rustup.exe run "$toolchain" cargo "$@"
  else
    cargo "$@"
  fi
}

run_cargo run --quiet --manifest-path "$ROOT_DIR/tools/copybook-scripts/Cargo.toml" -- perf-annotate-host
