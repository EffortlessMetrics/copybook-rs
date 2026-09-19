<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<p align="center">
  <img src="docs/assets/copybook-rs-mark.svg" alt="copybook-rs logo" width="128" />
</p>

<h1 align="center">copybook-rs</h1>

<p align="center">
  <em>Inspect, convert, and round-trip COBOL-described mainframe records.</em>
</p>

<p align="center">
  <a href="https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml"><img src="https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml/badge.svg?branch=main&event=push" alt="CI" /></a>
  <a href="https://github.com/EffortlessMetrics/ripr/blob/main/docs/BADGE_POLICY.md"><img src="https://img.shields.io/badge/ripr%2B-pending-lightgrey" alt="ripr+" /></a>
  <a href="https://github.com/EffortlessMetrics/unsafe-review/blob/main/docs/BADGE_POLICY.md"><img src="https://img.shields.io/badge/unsafe--review%2B-pending-lightgrey" alt="unsafe-review+" /></a>
</p>

<p align="center">
  <a href="https://github.com/EffortlessMetrics/copybook-rs/releases/latest"><img src="https://img.shields.io/github/v/release/EffortlessMetrics/copybook-rs?sort=semver&label=release" alt="GitHub release" /></a>
  <a href="https://crates.io/crates/copybook-cli"><img src="https://img.shields.io/crates/d/copybook-cli.svg?label=crates.io%20downloads" alt="crates.io downloads" /></a>
  <a href="https://docs.rs/copybook"><img src="https://docs.rs/copybook/badge.svg" alt="docs.rs" /></a>
</p>

<p align="center">
  <a href="https://doc.rust-lang.org/cargo/reference/manifest.html#the-rust-version-field"><img src="https://img.shields.io/badge/MSRV-1.98-blue.svg" alt="MSRV" /></a>
  <a href="#license"><img src="https://img.shields.io/badge/license-AGPL--3.0--or--later-blue.svg" alt="License: AGPL-3.0-or-later" /></a>
</p>

---

`copybook-rs` reads COBOL copybooks and fixed, RDW, or VB mainframe record
files, decodes supported records to JSONL, and can encode them back against the
same layout.

**Engineering Preview · v0.8.1.** It does not run COBOL or require mainframe
access.

## The problem

A copybook describes the record, but the extract is still encoded bytes.
EBCDIC, packed decimal, record framing, overlays, and variable structures still
have to be interpreted correctly. Getting the framing, codepage, or layout
wrong can make the output wrong even when the file is readable.

`copybook-rs` resolves the layout explicitly, decodes records against it, and
provides verification and deterministic round-trip checks for supported paths.

## The first useful run

Install the CLI, fetch the bundled EBCDIC fixture, and decode it:

```bash
cargo install copybook-cli@0.8.1 --locked

curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.8.1/fixtures/copybooks/simple.cpy
curl -LO https://github.com/EffortlessMetrics/copybook-rs/raw/v0.8.1/fixtures/data/simple.bin

copybook decode simple.cpy simple.bin \
  --format fixed --codepage cp037 \
  --output demo.jsonl

cat demo.jsonl
# {"CUSTOMER-ID":"123456",...,"ACCOUNT-BALANCE":"12345.67",...}
```

Then check the supported decode/encode path against the original bytes:

```bash
copybook determinism round-trip simple.cpy simple.bin \
  --format fixed --codepage cp037
```

The fixture includes EBCDIC text, zoned numerics, and a COMP-3 packed-decimal
field.

## Can it handle my files?

| Area | Supported surface |
| --- | --- |
| **Record framing** | Fixed, RDW, VB/BDW |
| **Text** | ASCII, CP037, CP273, CP500, CP1047, CP1140 |
| **Storage** | DISPLAY, zoned decimal, COMP-3, BINARY, COMP-1/COMP-2, edited PIC |
| **Structure** | REDEFINES, fixed OCCURS, tail-position ODO, Level-88, RENAMES R1-R3 |

Deliberate boundaries include nested ODO O5/O6, ODO over REDEFINES, RENAMES
R4-R6 interactions with REDEFINES/OCCURS, and `EXTERNAL` / `GLOBAL`.

Check a copybook directly:

```bash
copybook support --advise your-copybook.cpy
```

The [COBOL support matrix](docs/reference/COBOL_SUPPORT_MATRIX.md) is the
governed construct-level contract.

## Common jobs

| Need | Start with |
| --- | --- |
| Figure out an unfamiliar extract | `copybook doctor` |
| See field offsets and storage layout | `copybook inspect` |
| Convert records to JSONL | `copybook decode` |
| Encode JSONL back to records | `copybook encode` |
| Validate records without converting them | `copybook verify` |
| Check deterministic byte fidelity | `copybook determinism round-trip` |
| Explain a stable failure | `copybook explain` |
| Check a copybook change for breakage | `copybook compat` |

See the [CLI reference](docs/CLI_REFERENCE.md) for exact arguments and output
contracts.

## Use it from Rust

Depend on the canonical facade:

```toml
[dependencies]
copybook = "=0.8.1"
```

```rust
use copybook::codec::{decode_record, DecodeOptions};
use copybook::core::parse_copybook;
```

Most users should depend on `copybook`; lower-level crates remain available
for specialized use. See the [library API](docs/reference/LIBRARY_API.md).

## Status and documentation

Engineering Preview means the CLI and library expose stable contracts while the
COBOL support envelope remains preview-level. Validate representative
production copybooks and records before unattended production adoption.

| Need | Go to |
| --- | --- |
| Walk through the CLI | [Getting Started](docs/tutorials/getting-started.md) |
| Check exact command behavior | [CLI Reference](docs/CLI_REFERENCE.md) |
| Check COBOL feature support | [Support Matrix](docs/reference/COBOL_SUPPORT_MATRIX.md) |
| Embed the library | [Library API](docs/reference/LIBRARY_API.md) |
| Migrate from JRecord | [JRecord Migration](docs/JRECORD_MIGRATION.md) |
| Evaluate adoption and current limits | [Engineering Report](docs/REPORT.md) · [Roadmap](docs/ROADMAP.md) |

## Development

```bash
just build
just test
just lint
just fmt
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for the contributor workflow.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
