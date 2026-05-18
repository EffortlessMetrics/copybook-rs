<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# AGENTS.md

## Repo source-of-truth stack

This repo uses a linked source-of-truth stack:

```text
Roadmap -> Proposal -> Spec -> ADR -> Plan -> Active goal -> PR -> Proof
```

Read these before changing files:

1. `docs/reference/SPEC_SYSTEM.md`
2. `.copybook/goals/active.toml` when it exists
3. the linked implementation plan
4. the linked spec for the selected work item
5. linked ADRs

## Scope rule

Implement one work item per PR.

Docs-only artifacts are separate PRs:

- proposal PRs explain why;
- spec PRs define behavior;
- ADR PRs record durable decisions;
- plan PRs define sequencing;
- active goal PRs define current execution.

Runtime/code PRs must link to the spec and plan item they implement.

## Proof rule

Run the proof commands listed in the plan item.

If a proof command cannot run, record:

- command;
- reason unavailable;
- substitute evidence if any;
- whether this blocks merge.

Always run `git diff --check` before completion.

## Generated status rule

Do not hand-edit generated status. Run the generator or checker named in the
plan.

## Policy rule

If you add an exception, add or update the relevant `policy/*.toml` ledger with:

- owner;
- reason;
- `covered_by`;
- created;
- `review_after`;
- expiry if temporary.

## Stop conditions

Stop and report instead of guessing when:

- the active goal is missing or stale and the request expects implementation;
- linked specs or plans are missing;
- proof commands cannot run;
- unrelated staged changes exist;
- generated status is dirty;
- requested behavior contradicts an ADR;
- the requested task would require a new proposal, spec, or ADR that was not
  requested.
