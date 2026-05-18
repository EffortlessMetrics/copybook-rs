<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Repo source-of-truth system

copybook-rs uses a linked source-of-truth stack so humans and agents can tell
which artifact owns each kind of truth.

## Stack

```text
Roadmap
  -> Proposal
    -> Spec
      -> ADR
        -> Implementation plan
          -> Active goal
            -> PR
              -> Proof
```

## Artifact roles

| Artifact | Owns | Does not own |
|---|---|---|
| Roadmap | release direction and milestone framing | detailed PR queue |
| Proposal | why, users, alternatives, risks | behavior contract or exact PR order |
| Spec | behavior, acceptance, examples, proof | product rationale or PR sequence |
| ADR | durable decision and consequences | task list or current status |
| Plan | PR order, proof commands, rollback | product rationale or architecture decision |
| Active goal | current machine-readable work | generated status or long prose |
| Support tiers | public claim proof | feature design |
| Policy ledgers | exceptions and CI or policy receipts | broad architecture |

## Rules

1. One kind of truth per artifact.
2. One semantic artifact per PR unless the linked plan says otherwise.
3. Specs define behavior; plans define sequencing.
4. Proposals explain why; ADRs record durable decisions.
5. Active goals tell agents what to do now.
6. Generated status is updated by tools, not by hand.
7. Public claims require a support-tier row or equivalent proof pointer.
8. Policy exceptions require owner, reason, coverage, and review date.

## Required headers

Every new proposal, spec, ADR, and plan should include the applicable headers
below. Use `n/a` when a header does not apply.

```text
Status:
Owner:
Created:
Linked proposal:
Linked specs:
Linked ADRs:
Linked plan:
Linked issues:
Linked PRs:
Support-tier impact:
Policy impact:
```

## Agent workflow

Agents must:

1. read repo instructions such as `AGENTS.md` or `CLAUDE.md`;
2. read this source-of-truth system;
3. read `.copybook/goals/active.toml` when it exists;
4. choose exactly one ready work item from the active goal and linked plan;
5. read the linked spec for acceptance and linked ADRs for constraints;
6. implement only that item;
7. run the listed proof commands;
8. update receipts, status, or policy ledgers only when the work item requires
   it;
9. stop on missing or contradictory source-of-truth artifacts.

## Stop conditions

Stop and report instead of guessing when:

- the active goal is missing, paused, stale, or references missing files;
- linked specs or plans do not exist;
- proof commands cannot run;
- generated status differs from committed status;
- unrelated staged files exist;
- requested work conflicts with an ADR;
- a public claim lacks support-tier proof.

## Active goal lifecycle

Activate one lane at a time with:

```text
.copybook/goals/active.toml
```

Set `status = "paused"` with a reason when no lane is selected. Archive old
manifests under:

```text
.copybook/goals/archive/YYYY-MM-DD-<lane>.toml
```

Do not leave multiple active goals.

## Closeout format

At the end of a lane, write `plans/<lane>/closeout.md`:

```md
# Lane closeout: <lane>

Status: completed
Date: YYYY-MM-DD
Owner: n/a
Linked proposal: n/a
Linked specs: n/a
Linked ADRs: n/a
Linked plan: plans/<lane>/implementation-plan.md
Active goal archive: .copybook/goals/archive/YYYY-MM-DD-<lane>.toml

## What shipped

## Proof

## Receipts

## What did not ship

## Deferred work

## Claim boundary

## Next lane recommendation
```

## Common failure modes

### Spec becomes a task list

Move PR order to `plans/<lane>/implementation-plan.md`; keep the spec focused on
behavior, examples, and proof.

### Plan becomes product rationale

Move user pain, alternatives, and lane motivation to `docs/proposals/`; keep the
plan focused on work items.

### Active goal becomes prose

Keep `.copybook/goals/active.toml` machine-readable and link out to documents.
Do not add long generated tables.

### Agent hand-edits generated status

Add a generated-status rule, make the generator command explicit, and run the
checker instead of hand-editing generated outputs.

### Support claims drift

Require a support-tier impact header and proof pointer before broadening public
README claims.

### Policy exceptions become silent debt

Every exception must have owner, reason, `covered_by`, `review_after`, and an
expiry when temporary.

### Mega PR

Use one semantic artifact per PR and one implementation work item per runtime
PR.

## What good looks like

A new contributor or agent can arrive cold and answer:

```text
What are we doing?
Why?
What must be true?
What decision constrains it?
What PR lands next?
What command proves it?
What may we claim?
What must we not claim?
```

If the repo answers those questions without chat history, the method is working.
