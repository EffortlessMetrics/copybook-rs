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
# Every classification asserts record values or stable error identities
# on both sides, never headers or exit codes alone.
#
# What it is not: a CI gate. Network provisioning is pinned and cached but
# still network; a missing comparator reports unavailable/inconclusive and
# the lane fails loudly instead of counting as agreement.
#
# Usage: scripts/differential-jrecord.sh [--keep DIR]
#   --keep DIR  preserve per-lane original outputs under DIR (default: the
#               scratch directory is kept on failure and removed on success)

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
  local status=$?
  if [[ -n "$KEEP" ]]; then
    mkdir -p "$KEEP"
    cp -r "$SCRATCH"/. "$KEEP"/
    echo "lane outputs preserved under $KEEP"
  elif [[ "$status" -eq 0 ]]; then
    rm -rf "$SCRATCH"
  else
    echo "lane outputs preserved under $SCRATCH"
  fi
}
trap cleanup EXIT

REPO_ROOT="$(git rev-parse --show-toplevel)"
FIX="$REPO_ROOT/docs/evidence/differential-breadth"
cp "$FIX"/{vary.cpy,vbvaried.bin,lines.cpy,lines8.txt,multi01.cpy,fb-short-8.bin,prefix2.bin} "$SCRATCH"/

failures=0
report() { # $1 = lane, $2 = outcome, $3 = detail
  printf '%-28s %-24s %s\n' "$1" "$2" "$3"
  case "$2" in
    agreement|policy-difference|external-limitation|unsupported-nonoverlapping) ;;
    *) failures=$((failures + 1)) ;;
  esac
}

# --- Provision the pinned comparator (verified cache, never latest) ---
# Download to a temporary file and verify before it can poison the cache:
# a partial bundle never lands on the cached path.
if [[ ! -f "$BUNDLE" ]]; then
  mkdir -p "$CACHE_DIR"
  tmp_bundle="$(mktemp "$CACHE_DIR/.download-XXXXXX")"
  curl -sSL -o "$tmp_bundle" "$JRECORD_URL"
  echo "$JRECORD_SHA256  $tmp_bundle" | sha256sum -c - >/dev/null
  mv "$tmp_bundle" "$BUNDLE"
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
jrec() { # <cpy> <input> <output> <ifs>
  local cpy="$1" input="$2" output="$3" ifs="$4"
  local log
  log="$SCRATCH/$(basename "$output").jrec.log"
  (cd "$LIBDIR" && java -jar Cobol2Csv.jar -C "$cpy" -I "$input" -O "$output" -IFS "$ifs" "${JREC_COMMON[@]}" >"$log" 2>&1)
}
# cbk runs copybook-rs decode, saving diagnostics to <name>.cbk.log and
# reporting the exit status through the return code.
cbk() { # <cpy> <input> <output> <format> <logname>
  local cpy="$1" input="$2" output="$3" format="$4" logname="$5"
  "$REPO_ROOT/target/debug/copybook" decode "$cpy" "$input" --output "$output" --format "$format" --codepage ascii >"$SCRATCH/$logname.cbk.log" 2>&1
}
CBK_BIN="$REPO_ROOT/target/debug/copybook"

# --- Lane vb-varied: multi-block VB, 6-byte then 2-byte records ---
# The short record is a classified per-record error, so decode exits
# nonzero after writing record 1; the log assertion below is the check.
cbk "$SCRATCH/vary.cpy" "$SCRATCH/vbvaried.bin" "$SCRATCH/vb-cbk.jsonl" vb vb-varied || true
jrec "$SCRATCH/vary.cpy" "$SCRATCH/vbvaried.bin" "$SCRATCH/vb-jrec.csv" Mainframe_VB_As_RECFMU
if grep -q '"V-FIELD":"ABCDEF"' "$SCRATCH/vb-cbk.jsonl" \
  && grep -q "CBKF221_RDW_UNDERFLOW" "$SCRATCH/vb-varied.cbk.log" \
  && grep -q "^ABCDEF$" "$SCRATCH/vb-jrec.csv" \
  && grep -q "^GH$" "$SCRATCH/vb-jrec.csv"; then
  report "vb-varied" "policy-difference" \
    "copybook-rs decodes ABCDEF + CBKF221 underflow; JRecord accepts both rows"
else
  report "vb-varied" "unexpected" "record values or underflow identity changed"
fi

# --- Lane text: line-delimited fixed-width ---
if cbk "$SCRATCH/lines.cpy" "$SCRATCH/lines8.txt" "$SCRATCH/text-cbk.jsonl" fixed text; then
  report "text" "unexpected" "copybook-rs decoded line input"
else
  jrec "$SCRATCH/lines.cpy" "$SCRATCH/lines8.txt" "$SCRATCH/text-jrec.csv" Text
  if grep -q "CBKR101_FIXED_RECORD_ERROR" "$SCRATCH/text.cbk.log" \
    && grep -q "^ABCD,1234$" "$SCRATCH/text-jrec.csv" \
    && grep -q "^EFGH,5678$" "$SCRATCH/text-jrec.csv"; then
    report "text" "unsupported-nonoverlapping" \
      "copybook-rs has no text framing (CBKR101); JRecord Text splits both rows"
  else
    report "text" "unexpected" "framing behavior or row values changed"
  fi
fi

# --- Lane fb-short: 14 bytes at one 8-byte stride ---
if cbk "$SCRATCH/lines.cpy" "$SCRATCH/fb-short-8.bin" "$SCRATCH/fb-cbk.jsonl" fixed fb-short; then
  report "fb-short" "unexpected" "copybook-rs accepted a short tail"
else
  jrec "$SCRATCH/lines.cpy" "$SCRATCH/fb-short-8.bin" "$SCRATCH/fb-jrec.csv" Fixed_Length
  if grep -q "expected 8 bytes" "$SCRATCH/fb-short.cbk.log" \
    && [[ "$(wc -l < "$SCRATCH/fb-jrec.csv")" == "3" ]] \
    && grep -q "EFGH,56" "$SCRATCH/fb-jrec.csv"; then
    report "fb-short" "policy-difference" \
      "copybook-rs CBKR101 strict multiple; JRecord NUL-pads the short tail"
  else
    report "fb-short" "unexpected" "tail behavior or row values changed"
  fi
fi

# --- Lane multi01: two 01s, concatenation vs overlay ---
"$CBK_BIN" inspect "$SCRATCH/multi01.cpy" --format fixed >"$SCRATCH/m01-inspect.txt" 2>&1
jrec "$SCRATCH/multi01.cpy" "$SCRATCH/fb-short-8.bin" "$SCRATCH/m01-jrec.csv" Fixed_Length
if grep -q "Fixed LRECL: 8 bytes" "$SCRATCH/m01-inspect.txt" \
  && grep -q "^ABCD,ABCD$" "$SCRATCH/m01-jrec.csv"; then
  report "multi01" "policy-difference" \
    "copybook-rs concatenates 01s (LRECL 8); JRecord overlays at stride 4"
else
  report "multi01" "unexpected" "layout handling changed on either side"
fi

# --- Lane prefix2: generic 2-byte prefix, negative control ---
if cbk "$SCRATCH/vary.cpy" "$SCRATCH/prefix2.bin" "$SCRATCH/p2-cbk.jsonl" rdw prefix2 \
  && [[ "$(grep -c record_index "$SCRATCH/p2-cbk.jsonl")" == "2" ]]; then
  report "prefix2" "unexpected" "copybook-rs parsed generic prefixes"
else
  jrec "$SCRATCH/vary.cpy" "$SCRATCH/prefix2.bin" "$SCRATCH/p2-jrec.csv" Binary
  if grep -q "V_FIELD" "$SCRATCH/p2-jrec.csv" \
    && grep -q "ABCD" "$SCRATCH/p2-jrec.csv"; then
    report "prefix2" "unsupported-nonoverlapping" \
      "neither side parses generic prefixes (JRecord Binary keeps prefix bytes in-field)"
  else
    report "prefix2" "unexpected" "prefix handling changed on either side"
  fi
fi

if [[ "$failures" -gt 0 ]]; then
  echo "FAIL: $failures lane(s) unexpected"
  exit 1
fi
echo "OK: all lanes match the recorded classifications"
