#!/bin/bash
# Upload mutation testing metrics to Prometheus
# This script parses mutation testing results and uploads them to a Prometheus pushgateway

set -euo pipefail

# Configuration
PROMETHEUS_PUSHGATEWAY="${PROMETHEUS_PUSHGATEWAY:-http://localhost:9091}"
JOB_NAME="copybook-mutants"
TIMESTAMP=$(date -u +"%s")

# Check if mutation results exist
if [ ! -f "mutants-summary.csv" ]; then
    echo "Error: mutants-summary.csv not found"
    echo "Run mutation testing first: just mutants"
    exit 1
fi

# Function to upload metric to Prometheus
upload_metric() {
    local metric_name="$1"
    local metric_value="$2"
    local labels="$3"

    if [ -n "$labels" ]; then
        curl -X POST "${PROMETHEUS_PUSHGATEWAY}/metrics/job/${JOB_NAME}/${labels}" \
            --data-binary "${metric_name} ${metric_value} ${TIMESTAMP}" \
            --silent --show-error
    else
        curl -X POST "${PROMETHEUS_PUSHGATEWAY}/metrics/job/${JOB_NAME}" \
            --data-binary "${metric_name} ${metric_value} ${TIMESTAMP}" \
            --silent --show-error
    fi
}

# Parse and upload metrics from mutants-summary.csv
while IFS=, read -r crate caught missed unviable timeout total score; do
    # Skip header if present
    if [ "$crate" = "crate" ]; then
        continue
    fi

    # Remove % from score if present
    score_value=$(echo "$score" | sed 's/%//')

    # Upload mutation score
    upload_metric "mutation_score" "$score_value" "crate/${crate}"

    # Upload caught mutants count
    upload_metric "mutation_caught_total" "$caught" "crate/${crate}"

    # Upload missed mutants count
    upload_metric "mutation_missed_total" "$missed" "crate/${crate}"

    # Upload unviable mutants count
    upload_metric "mutation_unviable_total" "$unviable" "crate/${crate}"

    # Upload timeout mutants count
    upload_metric "mutation_timeout_total" "$timeout" "crate/${crate}"

    # Upload total mutants count
    upload_metric "mutation_total" "$total" "crate/${crate}"

    echo "Uploaded metrics for crate: $crate"
done < mutants-summary.csv

echo "Mutation testing metrics uploaded to Prometheus"
echo "View at: ${PROMETHEUS_PUSHGATEWAY}"
