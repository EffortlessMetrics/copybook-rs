#!/usr/bin/env bash
set -euo pipefail

# Performance receipt validation script for copybook-rs
# Ensures receipts conform to canonical schema and contain required metadata

RECEIPT_FILE="${1:-scripts/bench/perf.json}"
# Reserved for future JSON Schema validation (see schemas/perf-receipt-schema.json)
# shellcheck disable=SC2034
_SCHEMA_FILE="schemas/perf-receipt-schema.json"

# Function to validate JSON against schema (basic validation)
validate_receipt_structure() {
  local receipt_file="$1"
  
  if ! command -v jq >/dev/null 2>&1; then
    echo "❌ jq is required for receipt validation" >&2
    return 1
  fi
  
  # Check required top-level fields
  local required_fields=("format_version" "timestamp" "commit" "build_profile" "target_cpu" "environment" "benchmarks" "summary")
  for field in "${required_fields[@]}"; do
    if ! jq -e ".${field}" "$receipt_file" >/dev/null; then
      echo "❌ Missing required field: ${field}" >&2
      return 1
    fi
  done
  
  # Check environment sub-fields
  local env_fields=("os" "kernel" "cpu_model" "cpu_cores" "wsl2_detected")
  for field in "${env_fields[@]}"; do
    if ! jq -e ".environment.${field}" "$receipt_file" >/dev/null; then
      echo "❌ Missing required environment field: ${field}" >&2
      return 1
    fi
  done
  
  # Check benchmark fields
  if ! jq -e '.benchmarks[] | select(has("name") and has("mean_ns") and has("bytes_processed") and has("mean_mibps"))' "$receipt_file" >/dev/null; then
    echo "❌ Benchmarks missing required fields" >&2
    return 1
  fi
  
  # Check summary fields
  local summary_fields=("display_mibps" "comp3_mibps" "max_rss_mib")
  for field in "${summary_fields[@]}"; do
    if ! jq -e ".summary.${field}" "$receipt_file" >/dev/null; then
      echo "❌ Missing required summary field: ${field}" >&2
      return 1
    fi
  done
  
  # Check integrity field
  if ! jq -e '.integrity.sha256' "$receipt_file" >/dev/null; then
    echo "❌ Missing integrity SHA256 hash" >&2
    return 1
  fi
  
  echo "✅ Receipt structure validation passed"
  return 0
}

# Function to validate receipt integrity
validate_receipt_integrity() {
  local receipt_file="$1"

  # Extract stored hash
  local stored_hash
  stored_hash=$(jq -r '.integrity.sha256' "$receipt_file")

  # Calculate actual hash using canonical JSON (sorted keys, compact) excluding integrity field
  # This MUST match the generator: json.dumps(report, sort_keys=True, separators=(',', ':'))
  local actual_hash
  actual_hash=$(jq -cS 'del(.integrity)' "$receipt_file" | sha256sum | cut -d' ' -f1)

  if [[ "$stored_hash" == "$actual_hash" ]]; then
    echo "✅ Receipt integrity validation passed"
    return 0
  else
    echo "❌ Receipt integrity validation failed" >&2
    echo "  Stored hash: ${stored_hash}" >&2
    echo "  Actual hash: ${actual_hash}" >&2
    echo "  Hint: Receipt may have been modified after hashing" >&2
    return 1
  fi
}

# Function to validate format version
validate_format_version() {
  local receipt_file="$1"
  local format_version
  format_version=$(jq -r '.format_version' "$receipt_file")
  
  # Simple semantic version validation
  if [[ "$format_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "✅ Format version validation passed: ${format_version}"
    return 0
  else
    echo "❌ Invalid format version: ${format_version}" >&2
    return 1
  fi
}

# Function to validate timestamp format
validate_timestamp() {
  local receipt_file="$1"
  local timestamp
  timestamp=$(jq -r '.timestamp' "$receipt_file")
  
  # Basic ISO 8601 validation with Z suffix
  if [[ "$timestamp" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$ ]]; then
    echo "✅ Timestamp format validation passed: ${timestamp}"
    return 0
  else
    echo "❌ Invalid timestamp format: ${timestamp}" >&2
    return 1
  fi
}

# Function to validate commit hash
validate_commit_hash() {
  local receipt_file="$1"
  local commit
  commit=$(jq -r '.commit' "$receipt_file")
  
  # Git commit hash validation (7-40 hex characters)
  if [[ "$commit" =~ ^[a-f0-9]{7,40}$ ]]; then
    echo "✅ Commit hash validation passed: ${commit}"
    return 0
  else
    echo "❌ Invalid commit hash format: ${commit}" >&2
    return 1
  fi
}

# Function to validate performance values
validate_performance_values() {
  local receipt_file="$1"

  # Check that performance values are reasonable
  local display_mibps
  local comp3_mibps
  display_mibps=$(jq -r '.summary.display_mibps' "$receipt_file")
  comp3_mibps=$(jq -r '.summary.comp3_mibps' "$receipt_file")

  # Basic sanity checks using awk for numeric comparison (no bc dependency)
  if awk -v val="$display_mibps" 'BEGIN { exit !(val < 0) }'; then
    echo "❌ Invalid DISPLAY throughput: ${display_mibps} MiB/s (must be >= 0)" >&2
    return 1
  fi

  if awk -v val="$comp3_mibps" 'BEGIN { exit !(val < 0) }'; then
    echo "❌ Invalid COMP-3 throughput: ${comp3_mibps} MiB/s (must be >= 0)" >&2
    return 1
  fi

  # Check for extremely high values that might indicate measurement errors
  if awk -v val="$display_mibps" 'BEGIN { exit !(val > 100000) }'; then
    echo "⚠️  DISPLAY throughput seems unusually high: ${display_mibps} MiB/s" >&2
  fi

  if awk -v val="$comp3_mibps" 'BEGIN { exit !(val > 10000) }'; then
    echo "⚠️  COMP-3 throughput seems unusually high: ${comp3_mibps} MiB/s" >&2
  fi

  echo "✅ Performance values validation passed"
  return 0
}

# Main validation function
validate_receipt() {
  local receipt_file="$1"
  
  echo "🔍 Validating performance receipt: ${receipt_file}"
  
  if [[ ! -f "$receipt_file" ]]; then
    echo "❌ Receipt file not found: ${receipt_file}" >&2
    return 1
  fi
  
  # Check if it's valid JSON
  if ! jq empty "$receipt_file" >/dev/null 2>&1; then
    echo "❌ Invalid JSON format" >&2
    return 1
  fi
  
  # Run all validations
  validate_receipt_structure "$receipt_file" || return 1
  validate_format_version "$receipt_file" || return 1
  validate_timestamp "$receipt_file" || return 1
  validate_commit_hash "$receipt_file" || return 1
  validate_performance_values "$receipt_file" || return 1
  validate_receipt_integrity "$receipt_file" || return 1
  
  echo "✅ All receipt validations passed"
  return 0
}

# Script entry point
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  if [[ $# -eq 0 ]]; then
    echo "Usage: $0 [receipt_file]"
    echo "  receipt_file: Path to performance receipt file (default: scripts/bench/perf.json)"
    exit 1
  fi
  
  validate_receipt "$RECEIPT_FILE"
fi