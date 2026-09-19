#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Generate, validate, publish, and check public evidence badge endpoints."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

import generate_ripr_test_efficiency_inventory as ripr_inventory

ENDPOINTS = {
    "ripr.json": "ripr",
    "ripr-plus.json": "ripr+",
    "unsafe-review.json": "unsafe-review",
    "unsafe-review-plus.json": "unsafe-review+",
}
PUBLIC_KEYS = {"schemaVersion", "label", "message", "color"}
NUMERIC_MESSAGE = re.compile(r"^[0-9]+$")


class EndpointError(RuntimeError):
    pass


def _reject_duplicate_keys(pairs: list[tuple[str, object]]) -> dict:
    result: dict[str, object] = {}
    for key, value in pairs:
        if key in result:
            raise EndpointError(f"duplicate JSON key: {key}")
        result[key] = value
    return result


def _reject_constant(value: str) -> None:
    raise EndpointError(f"non-JSON numeric constant: {value}")


def _load_json(path: Path) -> dict:
    try:
        value = json.loads(
            path.read_text(encoding="utf-8"),
            object_pairs_hook=_reject_duplicate_keys,
            parse_constant=_reject_constant,
        )
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise EndpointError(f"{path}: invalid JSON: {exc}") from exc
    if not isinstance(value, dict):
        raise EndpointError(f"{path}: endpoint must be a JSON object")
    return value


def validate_endpoint(path: Path, expected_label: str) -> dict:
    value = _load_json(path)
    keys = set(value)
    if keys != PUBLIC_KEYS:
        raise EndpointError(
            f"{path}: public endpoint keys must be exactly "
            f"{sorted(PUBLIC_KEYS)}, got {sorted(keys)}"
        )
    if value.get("schemaVersion") != 1:
        raise EndpointError(f"{path}: schemaVersion must be 1")
    if value.get("label") != expected_label:
        raise EndpointError(
            f"{path}: label must be {expected_label!r}, got {value.get('label')!r}"
        )

    message = value.get("message")
    if not isinstance(message, str) or not NUMERIC_MESSAGE.fullmatch(message):
        raise EndpointError(
            f"{path}: message must be a decimal count, got {message!r}; "
            "refusing to publish partial, skipped, or inferred posture"
        )

    color = value.get("color")
    if not isinstance(color, str) or not color.strip():
        raise EndpointError(f"{path}: color must be a non-empty string")

    return value


def validate_directory(directory: Path) -> dict[str, dict]:
    endpoints: dict[str, dict] = {}
    for filename, label in ENDPOINTS.items():
        path = directory / filename
        if not path.is_file():
            raise EndpointError(f"missing generated endpoint: {path}")
        endpoints[filename] = validate_endpoint(path, label)
    return endpoints


def _run_json(command: list[str], *, cwd: Path, output: Path) -> dict:
    completed = subprocess.run(
        command,
        cwd=cwd,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
    )
    if completed.returncode != 0:
        raise EndpointError(
            f"command failed ({completed.returncode}): {' '.join(command)}\n"
            f"stdout:\n{completed.stdout}\n"
            f"stderr:\n{completed.stderr}"
        )
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(completed.stdout, encoding="utf-8")
    return _load_json(output)


def _run(command: list[str], *, cwd: Path) -> None:
    completed = subprocess.run(command, cwd=cwd, check=False)
    if completed.returncode != 0:
        raise EndpointError(
            f"command failed ({completed.returncode}): {' '.join(command)}"
        )


def _tool_version(tool: str, *, cwd: Path) -> str:
    completed = subprocess.run(
        [tool, "--version"],
        cwd=cwd,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        encoding="utf-8",
    )
    if completed.returncode != 0:
        raise EndpointError(f"could not read {tool} --version")
    return completed.stdout.strip()


def _write_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(
        json.dumps(value, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def generate(root: Path, output: Path) -> None:
    root = root.resolve()
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    native = output / "native"
    native.mkdir(parents=True, exist_ok=True)

    # ripr+ requires this fact source. The downstream bridge intentionally marks
    # every discovered test opaque rather than copying ripr's repo-private
    # analyzer or manufacturing discriminator-strength claims.
    ripr_inventory.write_report(
        root,
        root / "target/ripr/reports/test-efficiency.json",
    )

    ripr_commands = (
        ("repo-badge-json", native / "ripr.json"),
        ("repo-badge-shields", output / "ripr.json"),
        ("repo-badge-plus-json", native / "ripr-plus.json"),
        ("repo-badge-plus-shields", output / "ripr-plus.json"),
    )
    for output_format, path in ripr_commands:
        _run_json(
            [
                "ripr",
                "check",
                "--root",
                str(root),
                "--mode",
                "ready",
                "--format",
                output_format,
            ],
            cwd=root,
            output=path,
        )

    unsafe_dir = output / "unsafe-review-generated"
    if unsafe_dir.exists():
        shutil.rmtree(unsafe_dir)
    _run(
        [
            "unsafe-review",
            "badges",
            "--root",
            str(root),
            "--out",
            str(unsafe_dir),
        ],
        cwd=root,
    )
    for filename in ("unsafe-review.json", "unsafe-review-plus.json"):
        source = unsafe_dir / filename
        if not source.is_file():
            raise EndpointError(
                f"unsafe-review did not produce expected endpoint {source}"
            )
        shutil.copyfile(source, output / filename)

    endpoints = validate_directory(output)

    manifest = {
        "schema_version": 1,
        "source_sha": os.environ.get("GITHUB_SHA", ""),
        "tools": {
            "ripr": _tool_version("ripr", cwd=root),
            "unsafe_review": _tool_version("unsafe-review", cwd=root),
            "test_efficiency_producer": (
                "copybook-rs conservative opaque inventory bridge"
            ),
        },
        "endpoints": {
            filename: {
                "label": value["label"],
                "message": value["message"],
                "sha256": hashlib.sha256(
                    (output / filename).read_bytes()
                ).hexdigest(),
            }
            for filename, value in endpoints.items()
        },
    }
    _write_json(output / "manifest.json", manifest)


def publish(generated: Path, committed: Path) -> None:
    endpoints = validate_directory(generated)
    committed.mkdir(parents=True, exist_ok=True)
    for filename, value in endpoints.items():
        _write_json(committed / filename, value)


def check(generated: Path, committed: Path) -> None:
    generated_values = validate_directory(generated)
    committed_values = validate_directory(committed)
    drift: list[str] = []
    for filename in ENDPOINTS:
        if generated_values[filename] != committed_values[filename]:
            drift.append(
                f"{filename}: committed={committed_values[filename]!r} "
                f"generated={generated_values[filename]!r}"
            )
    if drift:
        raise EndpointError(
            "public evidence badge endpoint drift detected:\n- "
            + "\n- ".join(drift)
            + "\nrefresh with: "
            "python3 scripts/ci/evidence_badges.py publish "
            "target/evidence-badges badges"
        )


def main() -> int:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    generate_parser = subparsers.add_parser("generate")
    generate_parser.add_argument("--root", type=Path, default=Path("."))
    generate_parser.add_argument(
        "--output", type=Path, default=Path("target/evidence-badges")
    )

    validate_parser = subparsers.add_parser("validate")
    validate_parser.add_argument("directory", type=Path)

    publish_parser = subparsers.add_parser("publish")
    publish_parser.add_argument("generated", type=Path)
    publish_parser.add_argument("committed", type=Path)

    check_parser = subparsers.add_parser("check")
    check_parser.add_argument("generated", type=Path)
    check_parser.add_argument("committed", type=Path)

    args = parser.parse_args()
    try:
        if args.command == "generate":
            generate(args.root, args.output)
        elif args.command == "validate":
            validate_directory(args.directory)
        elif args.command == "publish":
            publish(args.generated, args.committed)
        elif args.command == "check":
            check(args.generated, args.committed)
        else:
            parser.error(f"unknown command: {args.command}")
    except EndpointError as exc:
        print(f"evidence-badges: {exc}", file=__import__("sys").stderr)
        return 1

    print(f"evidence-badges: {args.command} OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
