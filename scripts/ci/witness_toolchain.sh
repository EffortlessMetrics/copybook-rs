#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Fail unless the effective Cargo toolchain is governed by RUSTUP_TOOLCHAIN.
#
# Usage: witness_toolchain.sh <requested-selector>
#
# Background (#991): `rust-toolchain.toml` pins 1.98 for fmt parity, and that
# file takes precedence over the compiler a toolchain-install step selects as
# the rustup default. A job that intends stable/beta/MSRV must therefore set
# RUSTUP_TOOLCHAIN in its own scope; this witness proves — via rustup's own
# override reason, which stays distinct even when two channels resolve to the
# same release — that the compiler Cargo actually uses came from that
# selection and not from the repository pin. Run it immediately after the
# toolchain-install step and before any expensive suite.
set -euo pipefail

REQUESTED="${1:?usage: witness_toolchain.sh <requested-selector>}"

ACTIVE="$(rustup show active-toolchain -v)"
printf 'requested toolchain selector: %s\n%s\n' "$REQUESTED" "$ACTIVE"
rustc -vV
cargo --version

case "$ACTIVE" in
*"environment variable RUSTUP_TOOLCHAIN"*)
  echo "effective toolchain is governed by RUSTUP_TOOLCHAIN"
  ;;
*)
  printf 'error: effective toolchain is not governed by RUSTUP_TOOLCHAIN\n' >&2
  exit 1
  ;;
esac
