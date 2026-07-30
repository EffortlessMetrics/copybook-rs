<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Agentic PR Operations

## Intent

When a maintainer delegates an issue and PR queue to an agent, routine PR
operations are part of the delegated work. The agent should carry a focused
lane from discovery through merge using current repository and GitHub evidence,
without stopping for a second authorization for every normal review-bot action.

This design preserves maintainer control at the boundaries that can change
scope, provenance, release state, or user intent.

## Default authority for a delegated lane

After a maintainer asks an agent to work issue by issue and PR by PR, the agent
may perform these normal in-scope operations for the selected lane:

- create or reuse an isolated worktree and branch;
- edit, test, commit, and push the focused change;
- open, update, mark ready, and merge the corresponding PR when its merge
  contract is satisfied;
- inspect every review-bot comment, implement all actionable comments that fit
  the lane, and resolve each addressed thread;
- leave informational, duplicate, outdated, or rate-limit notices as
  non-blocking review metadata.

Resolving a thread is an evidence operation, not a separate product decision,
when the implementation addresses the exact comment and the relevant proof is
green. The agent records the addressed behavior and proof in the PR summary or
handoff.

The agent must stop and request direction for any of the following:

- conflicting or ambiguous review feedback that needs a product decision;
- a requested change that materially widens the issue or PR concern;
- force-push, direct mutation of `origin/main`, tag, publish, deploy, or secret
  handling;
- a merge decision when required checks are failing, stale, missing, or the
  current diff has unresolved actionable feedback;
- an external coordination or permission boundary that cannot be satisfied
  from the delegated repository workflow.

## Review-to-merge state machine

Each selected lane follows this sequence:

1. Refresh the branch, worktree, issue, PR, checks, and review threads.
2. Classify comments as actionable, informational, duplicate, outdated, or
   external. Only actionable comments can change the patch.
3. Implement each in-scope actionable comment and run the narrowest decisive
   proof before broader gates.
4. Push the exact reviewed head. Re-read comments and checks after the push.
5. Resolve addressed actionable threads. Do not resolve a comment whose
   requested behavior is not actually proven.
6. Merge only when the current head is mergeable, required checks are green,
   and no actionable thread remains unresolved.
7. Sync the target branch, preserve unrelated worktrees, and select the next
   bounded lane.

The agent must distinguish local proof, remote publication, and merged state.
An old green check, a local-only test, or a bot rate-limit notice does not
prove that the current PR is ready to merge.

## Review-bot handling contract

| Bot result | Agent action |
| --- | --- |
| Actionable and in scope | Fix, test, push, and resolve the thread |
| Actionable but ambiguous or widening scope | Stop and request direction |
| Informational or approval | Record if useful, no patch required |
| Duplicate or outdated | Preserve the evidence, no duplicate fix |
| Rate-limited or unavailable | Continue with repository checks and other review evidence; do not treat availability as approval |

This contract applies equally to inline comments and top-level automated review
summaries. A review-bot comment is not a reason to abandon a bounded lane, and
an absent bot review is not a reason to claim review coverage that did not run.

## Invariants

- One issue and one semantic concern remain active at a time.
- Every code change has a focused witness and a repository-appropriate gate.
- Every resolved actionable comment maps to a changed line, test, or explicit
  proof in the PR.
- No unrelated dirty worktree or neighboring worktree is discarded.
- Publication and merge are claimed only from live GitHub state.
