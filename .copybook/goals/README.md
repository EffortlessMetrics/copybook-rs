<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Active goals

The active goal manifest is the source of truth for **what agents should do
now**.

Use `.copybook/goals/active.toml` when a lane is selected. Archive superseded
active manifests under `.copybook/goals/archive/`.

## Manifest shape

```toml
id = "copybook-lane-id"
title = "Human readable lane title"
status = "active"
owner = "codex-claude"
created = "YYYY-MM-DD"

proposal = "docs/proposals/COPYBOOK-PROP-0001-lane.md"
plan = "plans/lane/implementation-plan.md"

specs = [
  "docs/specs/COPYBOOK-SPEC-0001-contract.md",
]

adrs = []

objective = """
State the current lane objective in one paragraph.
"""

end_state = [
  "Checkable end-state outcome.",
]

claim_boundaries = [
  "Do not broaden behavior beyond the linked spec.",
]

status_docs = [
  "docs/reference/COBOL_SUPPORT_MATRIX.md",
  "docs/ROADMAP.md",
]

[[work_item]]
id = "work-item-id"
status = "ready"
spec = "docs/specs/COPYBOOK-SPEC-0001-contract.md"
adr = "n/a"
plan = "plans/lane/implementation-plan.md#work-item-work-item-id"
current_pointer = "docs/ROADMAP.md"
claim_boundary = "What this work item may and may not claim."
commands = [
  "git diff --check",
]
```

Agents should not invent work when `active.toml` is absent, paused, stale, or
missing linked files. They should stop and report the missing source of truth.
