#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Fuzzing Report Generator
# Generates a comprehensive fuzzing report from fuzzer output

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
REPORT_DIR="${REPORT_DIR:-.fuzz-reports}"
TIMESTAMP=$(date -u +"%Y%m%d_%H%M%S")
REPORT_FILE="${REPORT_DIR}/fuzz_report_${TIMESTAMP}.md"
ARTIFACTS_DIR="${ARTIFACTS_DIR:-fuzz/artifacts}"

# Create report directory
mkdir -p "${REPORT_DIR}"

echo "Generating fuzzing report..."
echo "Report file: ${REPORT_FILE}"

# Start the report
cat > "${REPORT_FILE}" << 'EOF'
# Fuzzing Report

Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
Commit: $(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
Branch: $(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")

## Executive Summary

EOF

# Check for crash artifacts
CRASH_COUNT=0
for target_dir in "${ARTIFACTS_DIR}"/*/; do
    if [ -d "${target_dir}" ]; then
        target=$(basename "${target_dir}")
        crash_files=$(find "${target_dir}" -name "crash-*" 2>/dev/null | wc -l)
        CRASH_COUNT=$((CRASH_COUNT + crash_files))
    fi
done

if [ "${CRASH_COUNT}" -gt 0 ]; then
    echo -e "${RED}⚠️  FOUND ${CRASH_COUNT} CRASHES${NC}" | tee -a "${REPORT_FILE}"
    echo "" | tee -a "${REPORT_FILE}"
    echo "**Status:** FAILED - Crashes found" >> "${REPORT_FILE}"
else
    echo -e "${GREEN}✅ NO CRASHES FOUND${NC}" | tee -a "${REPORT_FILE}"
    echo "" | tee -a "${REPORT_FILE}"
    echo "**Status:** PASSED - No crashes found" >> "${REPORT_FILE}"
fi

echo "" >> "${REPORT_FILE}"

# Fuzz Targets Section
cat >> "${REPORT_FILE}" << 'EOF'
## Fuzz Targets

| Target | Status | Crashes | Corpus Size |
|--------|--------|---------|-------------|
EOF

# Analyze each fuzz target
for target_dir in "${ARTIFACTS_DIR}"/*/; do
    if [ -d "${target_dir}" ]; then
        target=$(basename "${target_dir}")
        corpus_dir="fuzz/corpus/${target}"
        
        # Count crashes
        crash_files=$(find "${target_dir}" -name "crash-*" 2>/dev/null | wc -l)
        
        # Count corpus size
        corpus_size=$(find "${corpus_dir}" -type f 2>/dev/null | wc -l)
        
        # Determine status
        if [ "${crash_files}" -gt 0 ]; then
            status="❌ FAILED"
        else
            status="✅ PASSED"
        fi
        
        echo "| ${target} | ${status} | ${crash_files} | ${corpus_size} |" >> "${REPORT_FILE}"
    fi
done

echo "" >> "${REPORT_FILE}"

# Crash Details Section
if [ "${CRASH_COUNT}" -gt 0 ]; then
    cat >> "${REPORT_FILE}" << 'EOF'
## Crash Details

EOF
    
    for target_dir in "${ARTIFACTS_DIR}"/*/; do
        if [ -d "${target_dir}" ]; then
            target=$(basename "${target_dir}")
            crash_files=$(find "${target_dir}" -name "crash-*" 2>/dev/null)
            
            if [ -n "${crash_files}" ]; then
                echo "### ${target}" >> "${REPORT_FILE}"
                echo "" >> "${REPORT_FILE}"
                
                for crash_file in ${crash_files}; do
                    crash_name=$(basename "${crash_file}")
                    crash_size=$(stat -f%z "${crash_file}" 2>/dev/null || stat -c%s "${crash_file}" 2>/dev/null || echo "unknown")
                    
                    echo "#### ${crash_name}" >> "${REPORT_FILE}"
                    echo "- Size: ${crash_size} bytes" >> "${REPORT_FILE}"
                    echo "- Path: \`${crash_file}\`" >> "${REPORT_FILE}"
                    echo "" >> "${REPORT_FILE}"
                    
                    # Try to get a preview of the crash input
                    if [ -f "${crash_file}" ]; then
                        echo "**Input Preview:**" >> "${REPORT_FILE}"
                        echo '```' >> "${REPORT_FILE}"
                        head -c 100 "${crash_file}" | od -A x -t x1z -v | head -20 >> "${REPORT_FILE}" || echo "Unable to preview" >> "${REPORT_FILE}"
                        echo '```' >> "${REPORT_FILE}"
                        echo "" >> "${REPORT_FILE}"
                    fi
                done
            fi
        fi
    done
fi

# Corpus Statistics Section
cat >> "${REPORT_FILE}" << 'EOF'
## Corpus Statistics

EOF

for corpus_dir in fuzz/corpus/*/; do
    if [ -d "${corpus_dir}" ]; then
        target=$(basename "${corpus_dir}")
        file_count=$(find "${corpus_dir}" -type f | wc -l)
        total_size=$(du -sh "${corpus_dir}" 2>/dev/null | cut -f1 || echo "unknown")
        
        echo "### ${target}" >> "${REPORT_FILE}"
        echo "- Files: ${file_count}" >> "${REPORT_FILE}"
        echo "- Total Size: ${total_size}" >> "${REPORT_FILE}"
        echo "" >> "${REPORT_FILE}"
    fi
done

# Recommendations Section
cat >> "${REPORT_FILE}" << 'EOF'
## Recommendations

EOF

if [ "${CRASH_COUNT}" -gt 0 ]; then
    cat >> "${REPORT_FILE}" << 'EOF'
### Priority Actions

1. **Immediate**: Investigate all crash artifacts and create bug reports
2. **Short-term**: Add crash inputs to regression test suite
3. **Medium-term**: Fix identified bugs and verify fixes with fuzzers

### Triage Process

1. Reproduce each crash locally using:
   ```bash
   cargo fuzz run <target> <crash_file>
   ```

2. Minimize crash input:
   ```bash
   cargo fuzz tmin <target> <crash_file>
   ```

3. Add minimized input to test corpus

4. Create issue with:
   - Crash file
   - Backtrace
   - Minimal reproduction case
EOF
else
    cat >> "${REPORT_FILE}" << 'EOF'
### Maintenance Actions

1. **Weekly**: Review corpus size and growth
2. **Monthly**: Run corpus minimization
3. **Quarterly**: Review fuzzer coverage and add new targets

### Corpus Optimization

To minimize corpus size:
```bash
cargo fuzz cmin <target>
```

To check coverage (requires instrumentation):
```bash
cargo fuzz coverage <target>
```
EOF
fi

# Footer
cat >> "${REPORT_FILE}" << 'EOF'
---

**Report generated by:** scripts/fuzz-report.sh
**For questions or issues:** Please open a GitHub issue
EOF

echo -e "${GREEN}✅ Report generated: ${REPORT_FILE}${NC}"
echo ""
echo "To view the report:"
echo "  cat ${REPORT_FILE}"
echo ""
echo "To upload to GitHub Actions artifacts:"
echo "  (Already handled by CI workflow)"
