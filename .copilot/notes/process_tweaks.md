# Process Tweaks

## CI-Off Mode
- Heavy CI lanes are dispatch-only
- Local gates + PR receipts are source of truth
- Default gate: fmt-check, lint, test, bdd-smoke

## Agent Coordination
- Agents work on same branch (restructure-workspace-add-crates)
- Commit batches after wave completion
- Review agent output before committing (watch for conflicts)

## Merge Strategy
- Squash-merge feature branches
- Conventional commit messages
- Co-authored-by trailer for Copilot
