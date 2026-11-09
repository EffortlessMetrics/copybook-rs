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
| Baseline ruleset | `main` (`13361843`), unchanged and without bypass actors |
| Dedicated CLA ruleset | `PENDING` |
| Individual-flow bot allowlist | `dependabot[bot]` |
| Last private CSV export | `PENDING` |

The sole bot exemption is evidence-backed. The repository has an active,
repository-owned [Dependabot configuration](../.github/dependabot.yml) for Cargo
and GitHub Actions updates and, as of 2026-09-15, GitHub reports 89 Dependabot
pull requests for this repository. This exemption applies only to
`dependabot[bot]`; it does not extend to `github-actions[bot]`, collaborators,
organization members, corporate contributors, or other service accounts.

## Activation sequence

1. Create a public Gist with `CLA.md` and `metadata` copied byte-for-byte from
   the canonical sources above.
2. Record the Gist URL and immutable revision in the deployment record.
3. Install the hosted CLA Assistant GitHub App only on `copybook-rs`, then link
   the repository to that Gist.
4. Import only `dependabot[bot]` as an individual-flow exemption. Do not exempt
   collaborators, organization members, corporate contributors, or service
   accounts that do not create controlled project-attributable contributions.
5. Use the activation pull request as the test PR. Verify that CLA Assistant
   comments, an unsigned user receives a non-successful `license/cla` status,
   signing changes it to success, and the recorded fields match the metadata
   source.
6. Create a dedicated default-branch CLA ruleset whose required check is
   `license/cla`, with the CLA Assistant GitHub App selected as the expected
   source. Preserve the existing `main` ruleset, including its pull-request,
   deletion, non-fast-forward, and no-bypass posture.
7. Leave the CLA ruleset bypass list empty unless the corporate process below
   is activated. If it is activated, add only the dedicated corporate-approval
   team in `pull_request` bypass mode.
8. Export the signature register to private, access-controlled storage and
   record the export date above.
9. Merge the documentation pull request only after the hosted flow and
   source-pinned CLA ruleset gate are both observed on that pull request.

## Corporate contributions

The hosted bot flow is limited to the Individual CLA. Do not add an “on behalf
of my employer” option, import a corporate contributor as though they signed the
Individual CLA, or place a human account or employer organization on the CLA
Assistant allowlist.

A corporate contribution remains blocked until the maintainers have adopted a
Corporate CLA, verified the representative's authority, and created a private
authorization record identifying the entity, agreement version, covered GitHub
usernames, scope, and effective date.

The corporate merge path is an audited exception to the CLA rule only:

1. enforce the App-pinned `license/cla` check in the dedicated CLA ruleset,
   separate from the baseline `main` ruleset;
2. make a dedicated `cla-corporate-approvers` team the only
   `pull_request`-mode bypass actor on that CLA-only ruleset;
3. require an approver to verify the private Corporate CLA record before
   bypassing the CLA ruleset for a specific pull request; and
4. retain a private receipt containing the entity, agreement version, covered
   usernames, pull request, approver, timestamp, and reason.

All ordinary pull-request, review, CI, deletion, and non-fast-forward rules
remain enforced. Until the Corporate CLA and this narrowly scoped exception path
are both configured, an entity-owned contribution cannot merge.

## Evidence retention

Export the signature register:

- immediately before every CLA or signer-metadata change;
- after the first signature under each new CLA version;
- at every release or API-freeze checkpoint.

Store exports privately outside the public repository. The private evidence
record should include the repository, Gist revision, export timestamp, and a
checksum. Never commit signer names, email addresses, or the exported register.
