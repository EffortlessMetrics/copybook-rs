# SPDX-License-Identifier: AGPL-3.0-or-later
# Reproducible benchmark container for copybook-rs performance testing
# Provides consistent environment for running cargo bench and generating perf receipts
#
# Build: docker build -t copybook-rs-bench .
# Run:   docker run -v $(pwd)/output:/workspace/output copybook-rs-bench
# View:  cat output/perf.json

FROM rust:1.92-bookworm

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    jq \
    git \
    time \
    build-essential \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

# Copy entire workspace for cargo bench
COPY . .

# Pre-build dependencies to cache layers
RUN cargo fetch

# Warm the build cache (optional, speeds up first run)
RUN cargo build --package copybook-bench --release --benches || true

# Configure git for commit hash extraction
RUN git config --global user.email "bench@copybook-rs.container" && \
    git config --global user.name "Bench Container" && \
    git config --global --add safe.directory /workspace

# Environment variables for benchmark configuration
ENV BENCH_FILTER="slo_validation"
ENV PERF=1
ENV RUSTFLAGS="-C target-cpu=native"

# Create output directory
RUN mkdir -p /workspace/output

# Entry point: run benchmarks and emit receipts
COPY scripts/bench-entrypoint.sh /usr/local/bin/bench-entrypoint.sh
RUN chmod +x /usr/local/bin/bench-entrypoint.sh

ENTRYPOINT ["/usr/local/bin/bench-entrypoint.sh"]
CMD []

# Volumes:
# - /workspace/output - Mount here to extract perf.json receipts
#
# Exit codes:
# - 0: Benchmarks completed successfully
# - Non-zero: Benchmark execution failed
