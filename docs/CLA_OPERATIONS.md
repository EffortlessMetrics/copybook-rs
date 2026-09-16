<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CLA Assistant Operations

## Decision

`copybook-rs` uses the hosted CLA Assistant service at
[cla-assistant.io](https://cla-assistant.io/). The service records assent to the
Project's existing Individual CLA; it does not define or replace the agreement.

Do not add CLA Assistant Lite, a `pull_request_target` CLA workflow, a
write-capable repository token, or repository-managed signature storage.

## Canonical sources

| Material | Canonical repository source | Hosted Gist file | SHA-256 |
| --- | --- | --- | --- |
| Agreement | [`../CLA.md`](../CLA.md) | `CLA.md` | `d7e967d036b83dc125314a6761d7a026e0905a923b106a788d7fe1904c5cbae4` |
| Signer fields | [`reference/cla-assistant-metadata.json`](reference/cla-assistant-metadata.json) | `metadata` | `b26121211321e5d82a37cd0b4da80e15246814f3483f31cf6dfec2ff70f6b00d` |

Copy the canonical bytes into the named Gist files. The hosted files must remain
byte-equivalent to the repository sources. Any byte change to either canonical
source creates a new signing version. Export the existing signature register
before changing the Gist, then update the deployment record below and allow CLA
Assistant to require re-signing.

## Deployment record

`PENDING` is fail-closed: hosted enforcement has not been proven while any
required field remains pending. Do not merge an activation change that claims
the control is active until the record and observed PR evidence are complete.

| Field | Value |
| --- | --- |
| Repository | `EffortlessMetrics/copybook-rs` |
| Service | Hosted CLA Assistant (`cla-assistant.io`) |
| GitHub App scope | `copybook-rs` only |
| Gist URL | `https://gist.github.com/EffortlessSteven/a98e159067c095688afcbd88ab105484` |
| Gist revision | `a77a8975c0eca2d5adbf9b0c7d3180a265413455` |
| Required status context | `license/cla` |
| Expected status source | CLA Assistant GitHub App |
| Individual-flow bot allowlist | `dependabot[bot]` |
| Last private CSV export | `PENDING` |

The sole bot exemption is evidence-backed. The repository has an active,
repository-owned [Dependabot configuration](../.github/dependabot.yml) for Cargo
and GitHub Actions updates and, as of 2026-09-15, GitHub reports 89 Dependabot
pull requests for this repository. This exemption applies only to
`dependabot[bot]`; it does not extend to `github-actions[bot]`, collaborators,
organization members, or other service accounts.

## Activation sequence

1. Create a public Gist with `CLA.md` and `metadata` copied byte-for-byte from
   the canonical sources above.
2. Record the Gist URL and immutable revision in the deployment record.
3. Install the hosted CLA Assistant GitHub App only on `copybook-rs`, then link
   the repository to that Gist.
4. Import only `dependabot[bot]` as an individual-flow exemption. Do not exempt
   collaborators, organization members, or service accounts that do not create
   controlled project-attributable contributions.
5. Use the activation pull request as the test PR. Verify that CLA Assistant
   comments, an unsigned user receives a non-successful `license/cla` status,
   signing changes it to success, and the recorded fields match the metadata
   source.
6. Add `license/cla` to the existing `main` ruleset as a required status check.
   Select the CLA Assistant GitHub App as the expected source. Preserve the
   existing pull-request, deletion, non-fast-forward, and no-bypass rules.
7. Export the signature register to private, access-controlled storage and
   record the export date above.
8. Merge the documentation pull request only after the hosted flow and
   source-pinned ruleset gate are both observed on that pull request.

## Corporate contributions

The bot flow is limited to the Individual CLA. Do not add an “on behalf of my
employer” option, and do not use a general exemption to simulate corporate
assent.

A corporate contribution remains blocked until the maintainers have adopted a
Corporate CLA, verified the representative's authority, and created a separate
authorization record.

## Evidence retention

Export the signature register:

- immediately before every CLA or signer-metadata change;
- after the first signature under each new CLA version;
- at every release or API-freeze checkpoint.

Store exports privately outside the public repository. The private evidence
record should include the repository, Gist revision, export timestamp, and a
checksum. Never commit signer names, email addresses, or the exported register.
