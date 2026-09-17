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

## Current-head CI observation procedure

Aggregate tallies (`gh pr checks` summaries, green percentages, merge-button
availability) are not merge evidence: they mix old runs, hide which head ran,
and count canceled or advisory jobs as product signal. Every assessment binds
one observation to repository, PR, current head SHA, workflow/check identity,
and run/attempt before presenting it.

1. Re-read the PR head first and last. Capture `headRefOid`, assess, then
   re-read it; a changed SHA invalidates the assessment (new push) instead
   of being averaged in.
2. Query check runs by exact commit SHA
   (`repos/<owner>/<repo>/commits/<head>/check-runs`, paginated), not by PR
   number alone. Merge-queue or merge-ref runs belong to different SHAs and
   are recorded separately, never treated as head evidence.
3. Keep pass, fail, pending, canceled, skipped, advisory, and unavailable
   states distinct. A canceled sibling of a failed fail-fast root is
   canceled, not a failure and not a pass. An old success never replaces a
   newer failed attempt; select attempts by latest timestamp.
4. Map every check name to the required/advisory policy in
   `docs/CI_GATING_POLICY.md`, executed by
   `scripts/ci/pr_head_status.py`. Unmapped names block a ready verdict
   until classified. Conditional non-applicability comes from workflow
   policy (trigger paths, changes-gates, schedule scope), not from a
   check's absence. Hosted ruleset contents are not visible to read-only
   tokens: record that as unknown, not as proof of no protection.
5. Report actionable review disposition separately from CI. A prior clean
   review never suppresses new findings; a bot comment is evaluated on its
   merits. An unresolved review count is not a correctness analysis.
6. For waits, watch one exact run with a bounded watcher
   (`gh run watch --exit-status` on the selected run id), emitting state
   changes, failed-job details, and the next actionable task — not repeated
   same-state totals. Reconcile a new push explicitly as a new head.
7. Measure queue, execution, registry propagation, and observation delay
   from their own timestamps. Polling overhead is not build time.
8. The helper and the commands above are read-only: no merge, rerun,
   branch/ruleset change, thread resolution, tag, or publication. Any
   actual settings change remains a separate maintainer decision.

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
