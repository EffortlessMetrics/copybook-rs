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
  cargo "$@"
}

run_cargo run --quiet --manifest-path "$ROOT_DIR/tools/copybook-scripts/Cargo.toml" -- check-performance-docs
