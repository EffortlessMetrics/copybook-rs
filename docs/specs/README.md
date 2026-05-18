<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Specs

Specs are the source of truth for **what must be true**.

A spec owns required behavior, non-goals, acceptance examples, proof
requirements, test mapping, implementation mapping, CI proof, and support-tier
impact. It does not own product rationale, PR sequencing, active queues, or
durable architecture decisions unless unavoidable.

## Naming

Use stable, boring IDs:

```text
COPYBOOK-SPEC-0001-<behavior-contract>.md
```

## Required shape

````md
# COPYBOOK-SPEC-0001: Behavior contract title

Status: accepted
Owner: n/a
Created: YYYY-MM-DD
Linked proposal: n/a
Linked ADRs: n/a
Linked plan: n/a
Linked issues: n/a
Linked PRs: n/a
Support-tier impact: n/a
Policy impact: n/a

## Problem

## Behavior

## Non-goals

## Required evidence

## Acceptance examples

### Example: accepted

```text
input / command / fixture
expected behavior
```

### Example: rejected

```text
input / command / fixture
expected rejection or failure mode
```

## Test mapping

## Implementation mapping

## CI proof

## Metrics / promotion rule

## Claim boundaries
````

Specs should answer agent and reviewer questions about behavior without turning
into task lists.
