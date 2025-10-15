#!/usr/bin/env python3
"""
Script to adapt all review agents in .claude/agents4/review/ to copybook-rs standards.
This replaces BitNet.rs-specific content with copybook-rs enterprise mainframe patterns.
"""

import os
import re
from pathlib import Path

# Base directory for review agents
AGENTS_DIR = Path(".claude/agents4/review/")

# Mapping of BitNet.rs terms to copybook-rs terms
REPLACEMENTS = {
    # Core domain replacement
    "BitNet.rs neural network inference": "copybook-rs enterprise mainframe data processing",
    "BitNet neural network": "copybook-rs enterprise mainframe",
    "BitNet.rs": "copybook-rs",
    "neural network": "COBOL parsing",
    "quantization": "COBOL parsing",
    "inference": "data conversion",
    "GPU": "enterprise performance",

    # Technical replacements
    "I2S, TL1, TL2": "DISPLAY, COMP, COMP-3",
    "quantization accuracy": "COBOL parsing accuracy",
    "cross-validation": "mainframe compatibility",
    "GGUF": "EBCDIC",
    "tensor": "field",
    "model": "copybook",
    "CUDA": "SIMD",

    # Performance targets
    ">99% accuracy": "enterprise performance targets (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s)",
    "99.8%": "4.1 GiB/s",
    "99.6%": "560 MiB/s",

    # Crate names
    "bitnet-quantization": "copybook-core",
    "bitnet-kernels": "copybook-codec",
    "bitnet-inference": "copybook-cli",
    "bitnet-wasm": "copybook-gen",
    "bitnet-tokenizers": "copybook-bench",

    # Commands
    "--no-default-features --features cpu": "--workspace",
    "--no-default-features --features gpu": "--workspace --release",
    "cargo run -p xtask -- crossval": "cargo xtask ci",
    "cargo run -p xtask -- benchmark": "cargo bench --package copybook-bench",
    "./scripts/verify-tests.sh": "cargo xtask ci --quick",

    # Error patterns
    "CUDA unavailable": "xtask unavailable",
    "GPU memory": "parsing memory",
    "C++ reference": "mainframe compatibility",

    # Evidence patterns
    "CPU: ok, GPU: ok": "workspace release ok",
    "tokens/sec": "records/sec",
    "I2S: 99.X%": "DISPLAY: X.Y GiB/s",

    # Architecture
    "quantization kernels": "COBOL parsing kernels",
    "inference pipeline": "data processing pipeline",
    "1-bit neural networks": "enterprise mainframe data processing",
}

def adapt_agent_file(filepath):
    """Adapt a single agent file to copybook-rs standards."""
    print(f"Processing {filepath.name}...")

    with open(filepath, 'r') as f:
        content = f.read()

    # Apply replacements
    original_content = content
    for old_term, new_term in REPLACEMENTS.items():
        content = content.replace(old_term, new_term)

    # Specific patterns for workspace structure
    content = re.sub(
        r'bitnet-[a-zA-Z]+',
        lambda m: {
            'bitnet-quantization': 'copybook-core',
            'bitnet-kernels': 'copybook-codec',
            'bitnet-inference': 'copybook-cli',
            'bitnet-wasm': 'copybook-gen',
            'bitnet-tokenizers': 'copybook-bench'
        }.get(m.group(0), 'copybook-core'),
        content
    )

    # Update command patterns
    content = re.sub(
        r'cargo test --workspace --no-default-features --features \w+',
        'cargo test --workspace',
        content
    )

    content = re.sub(
        r'cargo build --release --no-default-features --features \w+',
        'cargo build --workspace --release',
        content
    )

    # Update evidence patterns
    content = re.sub(
        r'tests: cargo test: (\d+)/(\d+) pass; CPU: (\d+)/(\d+), GPU: (\d+)/(\d+); quarantined: (\d+) \(linked\)',
        r'tests: nextest: \1/\2 pass; enterprise validation: \3/\4; quarantined: \7 (linked)',
        content
    )

    # Only write if content changed
    if content != original_content:
        with open(filepath, 'w') as f:
            f.write(content)
        print(f"  ✓ Updated {filepath.name}")
        return True
    else:
        print(f"  - No changes needed for {filepath.name}")
        return False

def main():
    """Process all agent files in the review directory."""
    if not AGENTS_DIR.exists():
        print(f"Error: Directory {AGENTS_DIR} does not exist")
        return

    agent_files = list(AGENTS_DIR.glob("*.md"))
    if not agent_files:
        print(f"No .md files found in {AGENTS_DIR}")
        return

    print(f"Found {len(agent_files)} agent files to process")

    updated_count = 0
    for agent_file in sorted(agent_files):
        if adapt_agent_file(agent_file):
            updated_count += 1

    print(f"\nCompleted! Updated {updated_count} of {len(agent_files)} agent files.")

if __name__ == "__main__":
    main()