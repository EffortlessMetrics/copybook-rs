#!/usr/bin/env python3
"""
Block Bash tool calls that pass '--edition 2021' (or '--edition=2021')
to cargo/rustc/rustfmt. Allow everything else.

Why Python?
- Read the JSON reliably
- Tokenize like a shell (shlex) so quoted values are handled
"""

import json, sys, shlex, os

DENY_MSG = (
    'Drop "--edition 2021"; rely on Cargo.toml (edition 2024). '
    'If you must pass it, use "--edition 2024".'
)

def main():
    try:
        data = json.load(sys.stdin)
    except Exception:
        # If we can't parse input, fail open (do not block).
        sys.exit(0)

    cmd = (data.get("tool_input", {}) or {}).get("command", "")
    if not cmd:
        sys.exit(0)

    try:
        argv = shlex.split(cmd, posix=True)
    except Exception:
        # Fallback: crude split; still better than inspecting raw JSON.
        argv = cmd.split()

    # Detect relevant tools by basename anywhere in argv (catches 'sudo cargo …', '/usr/bin/rustc', etc.)
    basenames = {os.path.basename(a) for a in argv}
    if {"cargo", "rustc", "rustfmt"}.isdisjoint(basenames):
        sys.exit(0)

    # Look for '--edition=2021' or '--edition 2021'
    for i, a in enumerate(argv):
        if a.startswith("--edition="):
            val = a.split("=", 1)[1].strip("\"'")
            if val == "2021":
                print(DENY_MSG, file=sys.stderr)
                sys.exit(2)
        if a == "--edition" and i + 1 < len(argv):
            val = argv[i + 1].strip("\"'")
            if val == "2021":
                print(DENY_MSG, file=sys.stderr)
                sys.exit(2)

    # No match: allow
    sys.exit(0)

if __name__ == "__main__":
    main()