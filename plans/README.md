<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Plans

Plans are the source of truth for **how work lands**.

A lane plan owns PR sequence, work items, dependencies, proof commands, rollback,
and status handoff. It does not own product motivation, durable architecture, or
generated status truth.

## Layout

```text
plans/
  <lane>/
    README.md
    implementation-plan.md
    closeout.md
```

## Implementation plan shape

````md
# Lane implementation plan

Status: active
Owner: n/a
Created: YYYY-MM-DD
Linked proposal: n/a
Linked specs: n/a
Linked ADRs: n/a
Linked plan: self
Linked issues: n/a
Linked PRs: n/a
Support-tier impact: n/a
Policy impact: n/a
Active goal: .copybook/goals/active.toml

## Current state

## Work item: short-id

Status: ready | active | blocked | completed | superseded
Linked proposal: n/a
Linked spec: n/a
Linked ADR: n/a
Blocks: n/a
Blocked by: n/a

### Goal

### Production delta

### Non-goals

### Acceptance

### Proof commands

```bash
git diff --check
```

### Rollback

### Notes
````

If a plan starts explaining why a lane exists, move that material to
`docs/proposals/`. If it starts defining behavior contracts, move that material
to `docs/specs/`.
