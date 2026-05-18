<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Proposals

Proposals are the source of truth for **why** a lane exists.

A proposal owns user pain, affected surfaces, success criteria, rejected alternatives,
risks, non-goals, and the specs or ADRs a lane needs. It does not own exact PR
sequencing, implementation details, generated status, or proof receipts.

## Naming

Use stable, boring IDs:

```text
COPYBOOK-PROP-0001-<lane>.md
```

## Required shape

```md
# COPYBOOK-PROP-0001: Lane title

Status: proposed
Owner: n/a
Created: YYYY-MM-DD
Target milestone: n/a
Linked specs: n/a
Linked ADRs: n/a
Linked plan: n/a
Support/status impact: n/a
Policy impact: n/a

## Problem

## Users and surfaces

## Success criteria

## Proposed shape

## Alternatives considered

## Specs to create or update

## ADRs needed

## Implementation campaign shape

## Evidence plan

## Risks

## Non-goals

## Exit criteria

## Claim boundary
```

Keep PR order and task details in `plans/<lane>/implementation-plan.md`, not in
proposal documents.
