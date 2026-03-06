<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Start Here

This is the stable entry point for the user-facing `copybook-rs` documentation.
If `docs/README.md` is regenerated in your workflow, use this file as the hand-maintained navigation page.

## First Links To Open

| If you want to... | Open |
|---|---|
| Try the CLI against bundled fixtures | [tutorials/getting-started.md](tutorials/getting-started.md) |
| Run end-to-end CLI workflows | [USER_GUIDE.md](USER_GUIDE.md) |
| See the exact command surface | [CLI_REFERENCE.md](CLI_REFERENCE.md) |
| Copy working commands | [reference/CLI_EXAMPLES.md](reference/CLI_EXAMPLES.md) |
| Embed the library in Rust | [reference/LIBRARY_API.md](reference/LIBRARY_API.md) |
| Check supported COBOL features | [reference/COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md) |
| Understand current limits | [ROADMAP.md](ROADMAP.md) |

## Recommended Reading Order

1. [tutorials/getting-started.md](tutorials/getting-started.md)
2. [CLI_REFERENCE.md](CLI_REFERENCE.md)
3. [reference/CLI_EXAMPLES.md](reference/CLI_EXAMPLES.md)
4. [reference/COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md)
5. [ROADMAP.md](ROADMAP.md)

## Notes

- Examples assume you are running from the repository root unless stated otherwise.
- If you are not installing the binary, replace `copybook ...` with `cargo run -q -p copybook-cli -- ...`.
- CLI flags in the maintained docs are kept aligned with `copybook --help`.
