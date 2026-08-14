<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# External-input manifest fixtures

These tiny fixtures prove the offline manifest contract across fixed and RDW
framing with ASCII and CP037 metadata. All four manifests share `simple.cpy`;
their payloads are five bytes and contain no sensitive data.

`copybook_sha256` covers the exact UTF-8 bytes read from `simple.cpy` and passed
to the parser. The loader deliberately performs no newline or whitespace
normalization, so changing the copybook changes the manifest identity even when
its calculated LRECL remains the same.

The fixtures prove parsing, path safety, integrity, framing, and deterministic
payload boundaries. They are not benchmark measurements, are not consumed by
the weekly soak workflow, and establish no throughput or threshold claim.
