#!/usr/bin/env bash
# Audit script to find panics in production code (excluding tests)

set -euo pipefail

echo "=== Panic Audit for copybook-rs Production Code ==="
echo ""

# Function to check if a file is primarily test code
is_test_file() {
    local file="$1"

    # Check filename patterns
    if [[ "$file" == *"tests.rs" ]] || [[ "$file" == *"_test.rs" ]] || [[ "$file" == */tests/* ]]; then
        return 0  # is test
    fi

    # Check if file starts with #[cfg(test)]
    if head -n 20 "$file" | grep -q "^#\[cfg(test)\]"; then
        return 0  # is test
    fi

    return 1  # not test
}

# Function to count panics excluding test blocks
count_non_test_panics() {
    local file="$1"

    # Use awk to skip content between #[cfg(test)] and next module/end
    awk '
        /^#\[cfg\(test\)\]/ { in_test=1; next }
        /^#\[allow\(clippy::unwrap_used/ && in_test { next }
        /^mod tests/ { in_test=1; next }
        /^}$/ && in_test { in_test=0; next }
        !in_test && /(\.unwrap\(\)|\.expect\(|panic!)/ { print }
    ' "$file" | wc -l
}

total_core=0
total_codec=0
total_cli=0

echo "## copybook-core"
echo ""
for file in copybook-core/src/**/*.rs copybook-core/src/*.rs; do
    if [[ -f "$file" ]] && ! is_test_file "$file"; then
        count=$(grep -c "unwrap\|expect\|panic!" "$file" 2>/dev/null || echo "0")
        if [[ $count -gt 0 ]]; then
            echo "- $file: $count instances"
            total_core=$((total_core + count))
        fi
    fi
done

echo ""
echo "## copybook-codec"
echo ""
for file in copybook-codec/src/**/*.rs copybook-codec/src/*.rs; do
    if [[ -f "$file" ]] && ! is_test_file "$file"; then
        count=$(grep -c "unwrap\|expect\|panic!" "$file" 2>/dev/null || echo "0")
        if [[ $count -gt 0 ]]; then
            echo "- $file: $count instances"
            total_codec=$((total_codec + count))
        fi
    fi
done

echo ""
echo "## copybook-cli"
echo ""
for file in copybook-cli/src/**/*.rs copybook-cli/src/*.rs; do
    if [[ -f "$file" ]] && ! is_test_file "$file"; then
        count=$(grep -c "unwrap\|expect\|panic!" "$file" 2>/dev/null || echo "0")
        if [[ $count -gt 0 ]]; then
            echo "- $file: $count instances"
            total_cli=$((total_cli + count))
        fi
    fi
done

echo ""
echo "=== Summary ==="
echo "copybook-core: $total_core"
echo "copybook-codec: $total_codec"
echo "copybook-cli: $total_cli"
echo "TOTAL: $((total_core + total_codec + total_cli))"
