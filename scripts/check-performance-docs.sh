#!/usr/bin/env bash
set -euo pipefail

# Performance documentation compliance checker
# Ensures all performance claims reference canonical receipts and follow governance policy

DOCS_DIR="docs"
RECEIPT_FILE="scripts/bench/perf.json"
GOVERNANCE_FILE="docs/PERFORMANCE_GOVERNANCE.md"
HISTORICAL_FILE="docs/HISTORICAL_PERFORMANCE.md"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Function to check if file contains performance numbers
contains_performance_numbers() {
  local file="$1"
  if [[ -f "$file" ]]; then
    # Look for performance patterns (MiB/s, GiB/s, MB/s)
    if grep -q -E "MiB/s|GiB/s|MB/s" "$file"; then
      return 0
    fi
  fi
  return 1
}

# Function to check if performance numbers reference canonical receipts
references_canonical_receipts() {
  local file="$1"
  if [[ -f "$file" ]]; then
    # Check for reference to scripts/bench/perf.json
    if grep -q "scripts/bench/perf.json" "$file"; then
      return 0
    fi
    # Check for receipt hash references
    if grep -q -E "[a-f0-9]{7,40}" "$file"; then
      return 0
    fi
  fi
  return 1
}

# Function to check for historical claims without proper labeling
has_unlabeled_historical_claims() {
  local file="$1"
  if [[ -f "$file" ]]; then
    # Look for historical targets without "OUTDATED" or "HISTORICAL" labels
    if grep -q -E "4\.1 GiB/s|560 MiB/s|GiB/s" "$file"; then
      # Check if it's properly labeled as historical
      if ! grep -q -E "OUTDATED|HISTORICAL|ARCHIVED" "$file"; then
        return 1
      fi
    fi
  fi
  return 0
}

# Function to validate receipt format version compliance
uses_receipt_format_version() {
  local file="$1"
  if [[ -f "$file" ]]; then
    # Check for format_version field usage
    if grep -q "format_version" "$file"; then
      return 0
    fi
  fi
  return 1
}

echo "🔍 Checking performance documentation compliance..."

# Check all markdown files for compliance
ISSUES_FOUND=0

# Scan all markdown files in docs directory
for doc_file in "$DOCS_DIR"/*.md; do
  if [[ ! -f "$doc_file" ]]; then
    continue
  fi
  
  echo -n "🔍 Checking: $(basename "$doc_file")"
  
  # Check for performance numbers
  if contains_performance_numbers "$doc_file"; then
    echo -e "  ${RED}❌ Contains performance numbers${NC}"
    
    # Check if references canonical receipts
    if ! references_canonical_receipts "$doc_file"; then
      echo -e "  ${RED}❌ Does not reference canonical receipts${NC}"
      ISSUES_FOUND=$((ISSUES_FOUND + 1))
    else
      echo -e "  ${GREEN}✅ References canonical receipts${NC}"
    fi
    
    # Check for unlabeled historical claims
    if has_unlabeled_historical_claims "$doc_file"; then
      echo -e "  ${YELLOW}⚠️  Contains unlabeled historical claims${NC}"
      ISSUES_FOUND=$((ISSUES_FOUND + 1))
    fi
    
    # Check for receipt format version usage
    if ! uses_receipt_format_version "$doc_file"; then
      echo -e "  ${YELLOW}⚠️  Does not use receipt format version${NC}"
      ISSUES_FOUND=$((ISSUES_FOUND + 1))
    fi
  else
    echo -e "  ${GREEN}✅ No performance compliance issues${NC}"
  fi
done

# Check governance policy exists
if [[ ! -f "$GOVERNANCE_FILE" ]]; then
  echo -e "  ${RED}❌ Performance governance policy missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Performance governance policy exists${NC}"
fi

# Check historical performance file exists
if [[ ! -f "$HISTORICAL_FILE" ]]; then
  echo -e "  ${RED}❌ Historical performance file missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Historical performance file exists${NC}"
fi

# Check receipt schema exists
if [[ ! -f "schemas/perf-receipt-schema.json" ]]; then
  echo -e "  ${RED}❌ Performance receipt schema missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Performance receipt schema exists${NC}"
fi

# Check validation script exists
if [[ ! -f "scripts/validate-perf-receipt.sh" ]]; then
  echo -e "  ${RED}❌ Receipt validation script missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Receipt validation script exists${NC}"
fi

# Check enhanced benchmark script exists
if [[ ! -f "scripts/bench-enhanced.sh" ]]; then
  echo -e "  ${RED}❌ Enhanced benchmark script missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Enhanced benchmark script exists${NC}"
fi

# Check canonical receipt exists
if [[ ! -f "$RECEIPT_FILE" ]]; then
  echo -e "  ${RED}❌ Canonical receipt file missing${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "  ${GREEN}✅ Canonical receipt file exists${NC}"
fi

echo
if [[ $ISSUES_FOUND -eq 0 ]]; then
  echo -e "${GREEN}✅ All performance documentation compliance checks passed${NC}"
  exit 0
else
  echo -e "${RED}❌ Found $ISSUES_FOUND performance documentation compliance issues${NC}"
  exit 1
fi