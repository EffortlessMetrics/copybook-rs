#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail
#
# Reproducible JRecord differential lanes for the breadth inventory
# (docs/evidence/differential-breadth/, issues #1007/#1021).
#
# What it does: provisions the pinned JRecord bundle once (verified by
# digest, never a mutable latest tag), runs the breadth lanes on both
# tools over committed fixtures, and classifies each lane with the closed
# #1021 outcome vocabulary. Both original outputs are preserved per lane
# under a scratch directory for inspection before any normalization.
#
# What it is not: a CI gate. Network provisioning is pinned and cached but
# still network; a missing comparator reports unavailable/inconclusive and
# the lane fails loudly instead of counting as agreement.
#
# Usage: scripts/differential-jrecord.sh [--keep DIR]
#   --keep DIR  preserve per-lane original outputs under DIR (default: tmp,
#               removed on success)

JRECORD_VERSION="0.93.2"
JRECORD_URL="https://sourceforge.net/projects/jrecord/files/jrecord/Version_${JRECORD_VERSION}/JRecord_${JRECORD_VERSION}.zip/download"
JRECORD_SHA256="74b0196d7565bc97b4f465e82176a7f8f4f50654a98b115c18992e417e28ed5c"
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/copybook-rs"
BUNDLE="$CACHE_DIR/JRecord_${JRECORD_VERSION}.zip"
LIBDIR="$CACHE_DIR/jrecord-${JRECORD_VERSION}-lib"

KEEP=""
if [[ "${1:-}" == "--keep" ]]; then
  KEEP="${2:?--keep needs a directory}"
fi
SCRATCH="$(mktemp -d)"
cleanup() {
  if [[ -z "$KEEP" ]]; then
    rm -rf "$SCRATCH"
  else
    mkdir -p "$KEEP"
    cp -r "$SCRATCH"/. "$KEEP"/
    echo "lane outputs preserved under $KEEP"
  fi
}
trap cleanup EXIT

REPO_ROOT="$(git rev-parse --show-toplevel)"
FIX="$REPO_ROOT/docs/evidence/differential-breadth"
cp "$FIX"/{vary.cpy,vbvaried.bin,lines.cpy,lines8.txt,multi01.cpy,fb-14.bin,prefix2.bin} "$SCRATCH"/

failures=0
report() { # $1 = lane, $2 = outcome, $3 = detail
  printf '%-28s %-24s %s\n' "$1" "$2" "$3"
  case "$2" in
    agreement|policy-difference|external-limitation|unsupported-nonoverlapping) ;;
    *) failures=$((failures + 1)) ;;
  esac
}

# --- Provision the pinned comparator (verified cache, never latest) ---
if [[ ! -f "$BUNDLE" ]]; then
  mkdir -p "$CACHE_DIR"
  curl -sSL -o "$BUNDLE" "$JRECORD_URL"
fi
echo "$JRECORD_SHA256  $BUNDLE" | sha256sum -c - >/dev/null
if [[ ! -f "$LIBDIR/Cobol2Csv.jar" ]]; then
  mkdir -p "$LIBDIR"
  unzip -o -q "$BUNDLE" 'lib/*.jar' -d "$LIBDIR-provision"
  mv "$LIBDIR-provision"/lib/*.jar "$LIBDIR"/
  rm -rf "$LIBDIR-provision"
fi
# Cobol2Csv resolves its manifest Class-Path (cb2xml, JRecord) relative to
# LIBDIR, so `java -jar` from there needs no explicit classpath.
cargo build -q -p copybook-cli --manifest-path "$REPO_ROOT/Cargo.toml"

JREC_COMMON=(-IC ASCII -OC ASCII -D "," -Q DoubleQuote -Dialect 0)
jrec() { # <cpy> <input> <output> <ifs...>
  local cpy="$1" input="$2" output="$3" ifs="$4"
  (cd "$LIBDIR" && java -jar Cobol2Csv.jar -C "$cpy" -I "$input" -O "$output" -IFS "$ifs" "${JREC_COMMON[@]}" >/dev/null 2>&1)
}
cbk() { # <cpy> <input> <output> <format>
  "$REPO_ROOT/target/debug/copybook" decode "$1" "$2" --output "$3" --format "$4" --codepage ascii >/dev/null 2>&1
}

# --- Lane vb-varied: multi-block VB, 6-byte then 2-byte records ---
cbk "$SCRATCH/vary.cpy" "$SCRATCH/vbvaried.bin" "$SCRATCH/vb-cbk.jsonl" vb || true
jrec "$SCRATCH/vary.cpy" "$SCRATCH/vbvaried.bin" "$SCRATCH/vb-jrec.csv" Mainframe_VB_As_RECFMU
cbk_records="$(grep -c record_index "$SCRATCH/vb-cbk.jsonl" || true)"
jrec_records="$(( $(wc -l < "$SCRATCH/vb-jrec.csv") - 1 ))"
if [[ "$cbk_records" == "1" ]] && grep -q ABCDEF "$SCRATCH/vb-cbk.jsonl" \
  && [[ "$jrec_records" == "2" ]] && grep -q GH "$SCRATCH/vb-jrec.csv"; then
  report "vb-varied" "policy-difference" \
    "copybook-rs decodes 1 record + CBKF221 underflow; JRecord accepts both rows"
else
  report "vb-varied" "unexpected" "cbk=$cbk_records jrec=$jrec_records"
fi

# --- Lane text: line-delimited fixed-width ---
if cbk "$SCRATCH/lines.cpy" "$SCRATCH/lines8.txt" "$SCRATCH/text-cbk.jsonl" fixed 2>/dev/null; then
  report "text" "unexpected" "copybook-rs decoded line input"
else
  jrec "$SCRATCH/lines.cpy" "$SCRATCH/lines8.txt" "$SCRATCH/text-jrec.csv" Text
  if grep -q "ABCD,1234" "$SCRATCH/text-jrec.csv" && grep -q "EFGH,5678" "$SCRATCH/text-jrec.csv"; then
    report "text" "unsupported-nonoverlapping" \
      "copybook-rs has no text framing (CBKR101); JRecord Text splits both rows"
  else
    report "text" "unexpected" "JRecord Text output changed"
  fi
fi

# --- Lane fb-short: 14 bytes at 4-byte stride (copybook-rs LRECL 8 concat) ---
if cbk "$SCRATCH/multi01.cpy" "$SCRATCH/fb-14.bin" "$SCRATCH/fb-cbk.jsonl" fixed 2>/dev/null; then
  report "fb-short" "unexpected" "copybook-rs accepted a short tail"
else
  jrec "$SCRATCH/multi01.cpy" "$SCRATCH/fb-14.bin" "$SCRATCH/fb-jrec.csv" Fixed_Length
  if grep -q "MN" "$SCRATCH/fb-jrec.csv"; then
    report "fb-short" "policy-difference" \
      "copybook-rs CBKR101 strict multiple; JRecord NUL-pads the short tail"
  else
    report "fb-short" "unexpected" "JRecord short-tail output changed"
  fi
fi

# --- Lane multi01: two 01s, concatenation vs overlay ---
jrec "$SCRATCH/multi01.cpy" "$SCRATCH/fb-14.bin" "$SCRATCH/m01-jrec.csv" Fixed_Length
if grep -q "A_FIELD,B_FIELD" "$SCRATCH/m01-jrec.csv"; then
  report "multi01" "policy-difference" \
    "copybook-rs concatenates 01s (LRECL 8); JRecord overlays at stride 4"
else
  report "multi01" "unexpected" "JRecord multi-01 output changed"
fi

# --- Lane prefix2: generic 2-byte prefix, negative control ---
if cbk "$SCRATCH/vary.cpy" "$SCRATCH/prefix2.bin" "$SCRATCH/p2-cbk.jsonl" rdw 2>/dev/null \
  && [[ "$(grep -c record_index "$SCRATCH/p2-cbk.jsonl")" == "2" ]]; then
  report "prefix2" "unexpected" "copybook-rs parsed generic prefixes"
else
  jrec "$SCRATCH/vary.cpy" "$SCRATCH/prefix2.bin" "$SCRATCH/p2-jrec.csv" Binary
  if grep -q "V_FIELD" "$SCRATCH/p2-jrec.csv"; then
    report "prefix2" "unsupported-nonoverlapping" \
      "neither side parses generic prefixes (JRecord Binary keeps prefix bytes in-field)"
  else
    report "prefix2" "unexpected" "JRecord Binary output changed"
  fi
fi

if [[ "$failures" -gt 0 ]]; then
  echo "FAIL: $failures lane(s) unexpected (outputs under $SCRATCH unless --keep)"
  exit 1
fi
echo "OK: all lanes match the recorded classifications"
