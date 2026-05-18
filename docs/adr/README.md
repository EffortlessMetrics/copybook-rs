<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Architecture Decision Records

ADRs are the source of truth for **durable decisions**.

Use ADRs sparingly. A good ADR should still matter six months after it lands.
ADRs own context, the selected decision, consequences, rejected alternatives, and
follow-up specs or plans. They do not own PR task lists, current metric state, or
implementation queues.

## Naming

Existing historical ADRs use `ADR-*` names. New source-of-truth ADRs should use
stable copybook-rs IDs:

```text
COPYBOOK-ADR-0001-<durable-decision>.md
```

## Required shape

```md
# COPYBOOK-ADR-0001: Decision title

Status: accepted
Date: YYYY-MM-DD
Owner: n/a
Linked proposal: n/a
Linked specs: n/a
Linked plan: n/a

## Decision

## Context

## Consequences

## Alternatives considered

## Follow-up specs / plans
```
