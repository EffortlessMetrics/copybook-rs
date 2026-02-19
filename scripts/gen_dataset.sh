#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/gen_dataset.sh <format> <codepage> <size> <output>

  <format>   Record format to generate (fixed|rdw)
  <codepage> Character encoding (ascii|cp037|cp273|cp500|cp1047|cp1140)
  <size>     Target dataset size (e.g. 512MiB, 1GiB, 750MB)
  <output>   Output file path

Environment:
  WORKLOAD     Workload mix (display-heavy|comp3-heavy|mixed). Default: mixed
  ZONED_POLICY Zoned formatting policy (preferred|preserved|override-ascii|override-ebcdic).
               Default: preferred
  SOAK_SEED    Deterministic RNG seed. Default: copybook-soak
USAGE
}

if [[ $# -ne 4 ]]; then
  usage
  exit 1
fi

FORMAT="$(tr '[:upper:]' '[:lower:]' <<<"$1")"
CODEPAGE="$(tr '[:upper:]' '[:lower:]' <<<"$2")"
SIZE_SPEC="$3"
OUTPUT="$4"

case "${FORMAT}" in
  fixed|rdw) ;;
  *)
    echo "Unsupported format '${FORMAT}'. Expected fixed or rdw." >&2
    exit 1
    ;;
esac

case "${CODEPAGE}" in
  ascii|cp037|cp273|cp500|cp1047|cp1140) ;;
  *)
    echo "Unsupported codepage '${CODEPAGE}'." >&2
    exit 1
    ;;
esac

WORKLOAD="${WORKLOAD:-mixed}"
ZONED_POLICY="${ZONED_POLICY:-preferred}"
SOAK_SEED="${SOAK_SEED:-copybook-soak}"

mkdir -p "$(dirname "${OUTPUT}")"

python3 - "$FORMAT" "$CODEPAGE" "$SIZE_SPEC" "$OUTPUT" <<'PY'
import math
import os
import pathlib
import random
import re
import sys

FORMAT = sys.argv[1]
CODEPAGE = sys.argv[2]
SIZE_SPEC = sys.argv[3]
OUTPUT = pathlib.Path(sys.argv[4])

WORKLOAD = os.environ.get("WORKLOAD", "mixed").lower()
ZONED_POLICY = os.environ.get("ZONED_POLICY", "preferred").lower()
SOAK_SEED = os.environ.get("SOAK_SEED", "copybook-soak")

CODEPAGE_CODECS = {
    "ascii": "ascii",
    "cp037": "cp037",
    "cp273": "cp273",
    "cp500": "cp500",
    "cp1047": "cp1047",
    "cp1140": "cp1140",
}

WORKLOAD_LAYOUT = {
    "display-heavy": (10, 2, 2),
    "comp3-heavy": (4, 2, 14),
    "mixed": (6, 4, 8),
}

SUPPORTED_POLICIES = {
    "preferred",
    "preserved",
    "override-ascii",
    "override-ebcdic",
}

if CODEPAGE not in CODEPAGE_CODECS:
    raise SystemExit(f"Unsupported codepage: {CODEPAGE}")

if WORKLOAD not in WORKLOAD_LAYOUT:
    raise SystemExit(
        f"Unsupported workload '{WORKLOAD}'. "
        f"Choose from {', '.join(sorted(WORKLOAD_LAYOUT))}."
    )

if ZONED_POLICY not in SUPPORTED_POLICIES:
    raise SystemExit(
        f"Unsupported zoned policy '{ZONED_POLICY}'. "
        f"Choose from {', '.join(sorted(SUPPORTED_POLICIES))}."
    )

codec = CODEPAGE_CODECS[CODEPAGE]


def parse_size(spec: str) -> int:
    spec = spec.strip()
    match = re.fullmatch(r"\s*(\d+(?:\.\d+)?)\s*([KMG]?i?B?)?\s*", spec, re.IGNORECASE)
    if not match:
        raise ValueError(f"Invalid size specification: {spec!r}")

    value = float(match.group(1))
    unit = (match.group(2) or "bytes").lower()

    multipliers = {
        "b": 1,
        "byte": 1,
        "bytes": 1,
        "kb": 1000,
        "mb": 1000**2,
        "gb": 1000**3,
        "kib": 1024,
        "mib": 1024**2,
        "gib": 1024**3,
    }

    if unit.endswith("ib") and unit not in multipliers:
        raise ValueError(f"Invalid binary size unit: {unit}")

    if unit not in multipliers:
        raise ValueError(f"Unsupported unit in size specification: {unit}")

    size_bytes = int(value * multipliers[unit])
    if size_bytes <= 0:
        raise ValueError("Size must be greater than zero")
    return size_bytes


def encode_display(text: str, width: int) -> bytes:
    truncated = text[:width]
    padded = truncated.ljust(width)
    return padded.encode(codec, errors="replace")


def encode_comp3(value: int, digits: int, scale: int) -> bytes:
    abs_value = abs(value)
    max_value = 10 ** digits - 1
    if abs_value > max_value:
        abs_value = max_value

    digit_str = f"{abs_value:0{digits}}"
    nibbles = [int(ch) for ch in digit_str]
    nibbles.append(0x0C if value >= 0 else 0x0D)

    if len(nibbles) % 2 != 0:
        nibbles.insert(0, 0)

    packed = bytearray()
    for i in range(0, len(nibbles), 2):
        packed.append((nibbles[i] << 4) | nibbles[i + 1])
    return bytes(packed)


def resolve_zoned_mode() -> str:
    if ZONED_POLICY == "override-ascii":
        return "ascii"
    if ZONED_POLICY == "override-ebcdic":
        return "ebcdic"
    if ZONED_POLICY == "preserved":
        # Preserve whichever mode matches the target codepage
        return "ascii" if CODEPAGE == "ascii" else "ebcdic"
    # preferred
    return "ascii" if CODEPAGE == "ascii" else "ebcdic"


def encode_zoned(value: int, digits: int, mode: str) -> bytes:
    abs_value = abs(value)
    max_value = 10 ** digits - 1
    if abs_value > max_value:
        abs_value = max_value

    digit_str = f"{abs_value:0{digits}}"
    positive_zone = 0x3 if mode == "ascii" else 0xF
    negative_zone = 0x7 if mode == "ascii" else 0xD
    zone = positive_zone if value >= 0 else negative_zone

    encoded = bytearray()
    for idx, ch in enumerate(digit_str):
        digit = ord(ch) - 0x30
        zone_high = zone if idx == len(digit_str) - 1 else positive_zone
        encoded.append((zone_high << 4) | digit)
    return bytes(encoded)


def build_record(index: int, rng: random.Random) -> bytes:
    display_fields, zoned_fields, comp3_fields = WORKLOAD_LAYOUT[WORKLOAD]
    display_width = 64
    zoned_digits = 11
    comp3_digits = 11
    comp3_scale = 2
    zoned_mode = resolve_zoned_mode()

    record = bytearray()

    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    for field in range(display_fields):
        prefix = f"REC{index:08d}-F{field:02d}-"
        remaining = display_width - len(prefix)
        body = "".join(rng.choice(alphabet) for _ in range(max(0, remaining)))
        record.extend(encode_display(prefix + body, display_width))

    zoned_max = 10 ** zoned_digits - 1
    for field in range(zoned_fields):
        value = rng.randint(-zoned_max, zoned_max)
        record.extend(encode_zoned(value, zoned_digits, zoned_mode))

    comp3_max = 10 ** comp3_digits - 1
    for field in range(comp3_fields):
        value = rng.randint(-comp3_max, comp3_max)
        record.extend(encode_comp3(value, comp3_digits, comp3_scale))

    return bytes(record)


target_bytes = parse_size(SIZE_SPEC)
rng = random.Random(SOAK_SEED)
record_example = build_record(0, rng)
record_length = len(record_example)

header_overhead = 4 if FORMAT == "rdw" else 0
per_record_bytes = record_length + header_overhead
record_count = max(1, math.ceil(target_bytes / per_record_bytes))

# Re-seed to ensure deterministic content starting from record 0
rng = random.Random(SOAK_SEED)

with OUTPUT.open("wb") as fh:
    for idx in range(record_count):
        record = build_record(idx, rng)
        if FORMAT == "rdw":
            rdw_length = len(record)
            if rdw_length > 0xFFFF:
                raise SystemExit("Record length exceeds RDW 64 KiB limit")
            fh.write(rdw_length.to_bytes(2, "big"))
            fh.write(b"\x00\x00")
        fh.write(record)

actual_bytes = OUTPUT.stat().st_size
print(
    f"Generated dataset {OUTPUT} ({FORMAT}, {CODEPAGE}, {WORKLOAD}) "
    f"records={record_count} size={actual_bytes} bytes "
    f"(target ~{target_bytes} bytes)"
)
PY
