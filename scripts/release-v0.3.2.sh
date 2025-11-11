#!/bin/bash
set -euo pipefail

#===============================================================================
# Release automation script for copybook-rs v0.3.2
#
# Features:
# - Waits for PR checks to complete (with timeout)
# - Squash-merges PRs with branch deletion
# - Creates and pushes git tag
# - Creates GitHub Release (draft by default)
# - Idempotent and safe to re-run
#
# Usage:
#   ./scripts/release-v0.3.2.sh --version v0.3.2 --notes /tmp/v0.3.2-release-notes.md --prs 142,143,144
#   ./scripts/release-v0.3.2.sh --version v0.3.2 --notes /tmp/v0.3.2-release-notes.md --prs 142,143,144 --publish
#===============================================================================

# Default values
VERSION=""
NOTES_FILE=""
PRS=""
PUBLISH_FLAG="--draft"
WAIT_FOR_CHECKS=true
MAX_WAIT_MINUTES=30
POLL_INTERVAL_SECONDS=30

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --version)
      VERSION="$2"
      shift 2
      ;;
    --notes)
      NOTES_FILE="$2"
      shift 2
      ;;
    --prs)
      PRS="$2"
      shift 2
      ;;
    --publish)
      PUBLISH_FLAG=""
      shift
      ;;
    --no-wait)
      WAIT_FOR_CHECKS=false
      shift
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      echo "Usage: $0 --version VERSION --notes NOTES_FILE --prs PR_LIST [--publish] [--no-wait]"
      exit 1
      ;;
  esac
done

# Validate required arguments
if [[ -z "$VERSION" ]] || [[ -z "$NOTES_FILE" ]] || [[ -z "$PRS" ]]; then
  echo -e "${RED}Error: Missing required arguments${NC}"
  echo "Usage: $0 --version VERSION --notes NOTES_FILE --prs PR_LIST [--publish] [--no-wait]"
  echo ""
  echo "Example:"
  echo "  $0 --version v0.3.2 --notes /tmp/v0.3.2-release-notes.md --prs 142,143,144"
  exit 1
fi

# Validate notes file exists
if [[ ! -f "$NOTES_FILE" ]]; then
  echo -e "${RED}Error: Release notes file not found: $NOTES_FILE${NC}"
  exit 1
fi

# Check gh auth status
if ! gh auth status >/dev/null 2>&1; then
  echo -e "${RED}Error: Not authenticated with GitHub CLI${NC}"
  echo "Run: gh auth login"
  exit 1
fi

# Convert comma-separated PRs to array
IFS=',' read -ra PR_ARRAY <<< "$PRS"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  copybook-rs Release Automation${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "  Version:      ${GREEN}${VERSION}${NC}"
echo -e "  Notes:        ${NOTES_FILE}"
echo -e "  PRs:          ${PRS}"
echo -e "  Publish:      $([ -z "$PUBLISH_FLAG" ] && echo "${GREEN}Yes${NC}" || echo "${YELLOW}Draft${NC}")"
echo -e "  Wait checks:  $([ "$WAIT_FOR_CHECKS" = true ] && echo "${GREEN}Yes${NC}" || echo "${YELLOW}No${NC}")"
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

#===============================================================================
# Function: Wait for PR checks to complete
#===============================================================================
wait_for_pr_checks() {
  local pr=$1
  local start_time=$(date +%s)
  local max_wait_seconds=$((MAX_WAIT_MINUTES * 60))

  echo -e "${BLUE}[PR #${pr}]${NC} Waiting for checks to complete..."

  while true; do
    # Get check status
    local status=$(gh pr view "$pr" --json statusCheckRollup --jq '
      .statusCheckRollup |
      map(select(.status == "COMPLETED")) |
      {
        total: (. | length),
        success: ([.[] | select(.conclusion == "SUCCESS")] | length),
        failed: ([.[] | select(.conclusion == "FAILURE")] | length),
        skipped: ([.[] | select(.conclusion == "SKIPPED")] | length)
      }
    ')

    local total=$(echo "$status" | jq -r '.total')
    local success=$(echo "$status" | jq -r '.success')
    local failed=$(echo "$status" | jq -r '.failed')
    local skipped=$(echo "$status" | jq -r '.skipped')
    local completed=$((success + failed + skipped))

    # Get total checks count
    local total_checks=$(gh pr view "$pr" --json statusCheckRollup --jq '.statusCheckRollup | length')

    # Check if all checks are completed
    if [[ $completed -eq $total_checks ]]; then
      if [[ $failed -gt 0 ]]; then
        echo -e "${RED}[PR #${pr}]${NC} ❌ Checks failed: ${failed} failed, ${success} passed, ${skipped} skipped"
        echo -e "${YELLOW}Failed checks:${NC}"
        gh pr checks "$pr" | grep -i fail || true
        return 1
      else
        echo -e "${GREEN}[PR #${pr}]${NC} ✅ All checks passed: ${success} success, ${skipped} skipped"
        return 0
      fi
    fi

    # Check timeout
    local current_time=$(date +%s)
    local elapsed=$((current_time - start_time))
    if [[ $elapsed -ge $max_wait_seconds ]]; then
      echo -e "${RED}[PR #${pr}]${NC} ⏱️  Timeout waiting for checks (${MAX_WAIT_MINUTES} minutes)"
      echo -e "${YELLOW}Current status:${NC} ${completed}/${total_checks} completed (${success} success, ${failed} failed, ${skipped} skipped)"
      return 1
    fi

    # Progress update
    local elapsed_min=$((elapsed / 60))
    echo -e "${YELLOW}[PR #${pr}]${NC} ⏳ Checks: ${completed}/${total_checks} completed (${success} ✓, ${failed} ✗, ${skipped} ⊘) [${elapsed_min}m elapsed]"

    sleep "$POLL_INTERVAL_SECONDS"
  done
}

#===============================================================================
# Function: Merge PR
#===============================================================================
merge_pr() {
  local pr=$1

  # Get PR title for squash commit message
  local pr_title=$(gh pr view "$pr" --json title --jq '.title')

  echo -e "${BLUE}[PR #${pr}]${NC} Merging: ${pr_title}"

  # Check if already merged
  local state=$(gh pr view "$pr" --json state --jq '.state')
  if [[ "$state" == "MERGED" ]]; then
    echo -e "${GREEN}[PR #${pr}]${NC} ✅ Already merged, skipping"
    return 0
  fi

  # Attempt merge
  if gh pr merge "$pr" --squash --delete-branch --subject "$pr_title"; then
    echo -e "${GREEN}[PR #${pr}]${NC} ✅ Merged successfully"
    return 0
  else
    echo -e "${RED}[PR #${pr}]${NC} ❌ Merge failed"
    return 1
  fi
}

#===============================================================================
# Step 1: Wait for all PR checks (if enabled)
#===============================================================================
if [[ "$WAIT_FOR_CHECKS" = true ]]; then
  echo -e "${BLUE}━━━ Step 1: Waiting for PR checks ━━━${NC}"
  echo ""

  for pr in "${PR_ARRAY[@]}"; do
    if ! wait_for_pr_checks "$pr"; then
      echo -e "${RED}Error: PR #${pr} checks failed or timed out${NC}"
      exit 1
    fi
    echo ""
  done

  echo -e "${GREEN}✅ All PR checks passed!${NC}"
  echo ""
fi

#===============================================================================
# Step 2: Merge all PRs
#===============================================================================
echo -e "${BLUE}━━━ Step 2: Merging PRs ━━━${NC}"
echo ""

for pr in "${PR_ARRAY[@]}"; do
  if ! merge_pr "$pr"; then
    echo -e "${RED}Error: Failed to merge PR #${pr}${NC}"
    exit 1
  fi
  echo ""
done

echo -e "${GREEN}✅ All PRs merged!${NC}"
echo ""

#===============================================================================
# Step 3: Update local main branch
#===============================================================================
echo -e "${BLUE}━━━ Step 3: Updating local main branch ━━━${NC}"
echo ""

git fetch origin main
git checkout main
git pull origin main

echo -e "${GREEN}✅ Local main branch updated${NC}"
echo ""

#===============================================================================
# Step 4: Create and push tag
#===============================================================================
echo -e "${BLUE}━━━ Step 4: Creating git tag ━━━${NC}"
echo ""

# Check if tag already exists
if git rev-parse "$VERSION" >/dev/null 2>&1; then
  echo -e "${YELLOW}⚠️  Tag ${VERSION} already exists locally${NC}"
  read -p "Delete and recreate? (y/N): " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    git tag -d "$VERSION"
    git push origin ":refs/tags/$VERSION" 2>/dev/null || true
  else
    echo -e "${YELLOW}Using existing tag${NC}"
  fi
fi

# Create tag if it doesn't exist
if ! git rev-parse "$VERSION" >/dev/null 2>&1; then
  git tag -a "$VERSION" -m "CI hardening + Level-88 VALUE comma support"
  echo -e "${GREEN}✅ Tag ${VERSION} created${NC}"
fi

# Push tag
if git push origin "$VERSION" 2>/dev/null; then
  echo -e "${GREEN}✅ Tag ${VERSION} pushed to origin${NC}"
else
  echo -e "${YELLOW}⚠️  Tag ${VERSION} already exists on origin${NC}"
fi

echo ""

#===============================================================================
# Step 5: Create GitHub Release
#===============================================================================
echo -e "${BLUE}━━━ Step 5: Creating GitHub Release ━━━${NC}"
echo ""

# Check if release already exists
if gh release view "$VERSION" >/dev/null 2>&1; then
  echo -e "${YELLOW}⚠️  Release ${VERSION} already exists${NC}"
  read -p "Delete and recreate? (y/N): " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    gh release delete "$VERSION" --yes
  else
    echo -e "${YELLOW}Skipping release creation${NC}"
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}  Release process complete!${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
  fi
fi

# Create release
RELEASE_TITLE="${VERSION} — CI Hardening & Level-88 VALUE Comma Support"

if gh release create "$VERSION" \
  --title "$RELEASE_TITLE" \
  --notes-file "$NOTES_FILE" \
  $PUBLISH_FLAG; then

  if [[ -z "$PUBLISH_FLAG" ]]; then
    echo -e "${GREEN}✅ Release ${VERSION} published${NC}"
  else
    echo -e "${GREEN}✅ Draft release ${VERSION} created${NC}"
    echo -e "${YELLOW}   To publish: gh release edit ${VERSION} --draft=false${NC}"
  fi
else
  echo -e "${RED}❌ Failed to create release${NC}"
  exit 1
fi

echo ""

#===============================================================================
# Success summary
#===============================================================================
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}  ✅ Release ${VERSION} complete!${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "📦 Release page:"
echo -e "   ${BLUE}https://github.com/EffortlessMetrics/copybook-rs/releases/tag/${VERSION}${NC}"
echo ""
echo -e "🔍 Compare view:"
echo -e "   ${BLUE}https://github.com/EffortlessMetrics/copybook-rs/compare/v0.3.1...${VERSION}${NC}"
echo ""
echo -e "${YELLOW}📋 Post-release checklist:${NC}"
echo -e "   1. Verify main CI is green: ${BLUE}gh run list --branch main --limit 5${NC}"
echo -e "   2. Check nightly soak not cancelled by pushes"
echo -e "   3. Confirm release artifacts render correctly"
echo -e "   4. Update CHANGELOG.md if needed"
echo ""
echo -e "${GREEN}🎉 All done!${NC}"
echo ""
