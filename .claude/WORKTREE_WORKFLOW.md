# PSTX Worktree Workflow Guide

This document outlines the proper worktree-based development workflow for PSTX agents and developers.

## Core Principles

### 1. **Worktree Independence**
- Each worktree (lane-1, lane-2, etc.) operates independently
- No shared state or cross-worktree dependencies
- Each worktree syncs directly with `origin/main`
- Eliminates stale worktree references and sync conflicts

### 2. **Self-Contained Development**
- All work happens within the current worktree
- No need for shared main worktree or complex sync strategies
- Each worktree maintains its own current state with origin/main
- Parallel development without interference

### 3. **Proper Sync Strategy**
```bash
# In each worktree, sync independently:
git pull origin main

# NOT shared worktree patterns like:
# git worktree add ../main main  # AVOID
# git merge ../main/main         # AVOID
```

## Workflow Steps

### Initial Setup
```bash
# Each worktree tracks origin/main directly
git branch --set-upstream-to=origin/main lane/N

# Verify independent tracking
git status  # Should show "behind/ahead of origin/main"
```

### Development Cycle
1. **Start in Current Worktree**: All work happens in current lane
2. **Independent Sync**: `git pull origin main` when needed
3. **Self-Contained Changes**: Commit and develop independently
4. **Remote Integration**: Push to feature branch, create PR
5. **Post-Merge Sync**: `git pull origin main` to get merged changes

### Agent Workflow Integration

#### pr-initial-reviewer
- Works in assigned worktree
- Tags PR with `pstx:lane-N` to prevent conflicts
- No branch switching within worktree

#### docs-updater-pre-merge
- Updates documentation within current worktree
- Syncs independently with `git pull origin main`
- No shared worktree dependencies

#### pr-merger
- Executes remote merge via `gh pr merge`
- Each worktree syncs independently post-merge
- No cross-worktree state management

## Benefits of This Approach

### ✅ **Eliminates Common Issues**
- No stale worktree references
- No cross-worktree sync conflicts  
- No shared state corruption
- No complex worktree orchestration

### ✅ **Enables True Parallelism**
- Multiple agents can work simultaneously
- Each worktree is self-contained
- No coordination overhead between worktrees
- Independent development velocities

### ✅ **Simplifies Operations**
- Single sync command: `git pull origin main`
- Clear state: always relative to origin/main
- No complex recovery procedures
- Predictable behavior across all worktrees

## Migration from Shared Worktree Model

### Old Pattern (Avoid)
```bash
# Shared main worktree
git worktree add ../main main
git merge ../main/main
```

### New Pattern (Use)
```bash
# Independent sync
git pull origin main
```

### Cleanup Old References
```bash
# Remove stale worktree references
git worktree prune

# Verify clean state
git worktree list
```

## Agent Configuration Updates

All agents have been updated to reflect this workflow:

- **pr-merger.md**: Updated to emphasize worktree independence
- **docs-updater-pre-merge.md**: Changed from "lane-autonomous" to "worktree-based"
- **pr-initial-reviewer.md**: Updated branching strategy and tag management

## Best Practices

### ✅ **Do**
- Sync each worktree independently with `git pull origin main`
- Use `git worktree prune` to clean stale references
- Work entirely within current worktree
- Tag PRs immediately to prevent conflicts

### ❌ **Don't**
- Create shared main worktrees
- Attempt cross-worktree merges
- Rely on worktree-to-worktree sync
- Switch branches within worktrees during agent operations

## Troubleshooting

### Stale Worktree References
```bash
git worktree prune
git worktree list  # Verify cleanup
```

### Sync Issues
```bash
# Reset to clean state
git fetch origin
git reset --hard origin/main
```

### Worktree Health Check
```bash
git status  # Should show clean state vs origin/main
git branch -vv  # Should show tracking origin/main
```

This workflow ensures reliable, predictable, and parallel development across all PSTX worktrees.