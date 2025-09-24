#!/usr/bin/env python3
"""
Final comprehensive cleanup script for review agents.
"""

import os
import re
from pathlib import Path

# Base directory for review agents
AGENTS_DIR = Path("/home/steven/code/Rust/copybook-rs/.claude/agents4/review/")

def final_cleanup_agent(filepath):
    """Perform final cleanup of agent file."""
    print(f"Final cleanup of {filepath.name}...")

    with open(filepath, 'r') as f:
        content = f.read()

    original_content = content

    # Fix remaining garbled terms
    content = content.replace("1-bit quantized COBOL parsings", "enterprise mainframe data processing")
    content = content.replace("Neural Network Security Testing (NNST)", "COBOL Parsing Security Testing")
    content = content.replace("HuggingFace tokens", "mainframe authentication tokens")
    content = content.replace("copybook poisoning attacks", "malicious copybook attacks")

    # Fix workspace references
    content = content.replace("copybook-rs workspace crates", "copybook-rs 5-crate workspace (core, codec, cli, gen, bench)")

    # Fix command patterns
    content = content.replace("cargo clippy --workspace --all-targets --workspace", "cargo clippy --workspace --all-targets")
    content = content.replace("--workspace -- -D warnings", "-- -D warnings -W clippy::pedantic")

    # Fix technical descriptions
    content = content.replace("COBOL parsing COBOL parsing", "COBOL parsing")
    content = content.replace("enterprise performance/CPU", "high-performance")
    content = content.replace("enterprise performance memory", "memory")
    content = content.replace("SIMD enterprise performance", "SIMD CPU")

    # Fix performance metrics formatting
    content = content.replace("GiB/s for DISPLAY, MiB/s for COMP-3ond", "records/second")
    content = content.replace("GiB/s for DISPLAY, MiB/s for COMP-3", "GiB/s (DISPLAY), MiB/s (COMP-3)")

    # Fix duplicate workspace patterns
    content = content.replace("cargo bench --workspace --workspace", "cargo bench --package copybook-bench")
    content = content.replace("cargo test --workspace --workspace", "cargo test --workspace")

    # Fix evidence patterns
    content = content.replace("I2S ≥4.1 GiB/s, TL1 ≥560 MiB/s, TL2 ≥99.7%", "DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s")
    content = content.replace("I2S: 4.1 GiB/s", "DISPLAY: 4.1+ GiB/s")
    content = content.replace("TL1: 560 MiB/s", "COMP-3: 560+ MiB/s")

    # Fix specific technical terms
    content = content.replace("copybook weight handling", "copybook field handling")
    content = content.replace("weight data conversion", "field layout computation")
    content = content.replace("Tensor Core acceleration", "SIMD acceleration")
    content = content.replace("mixed precision", "high-precision")

    # Fix command examples
    content = content.replace("--tokens 128", "--batch-size 128")
    content = content.replace("--copybook examples/copybook.cpy --tokens", "--input examples/data.bin --copybook examples/schema.cpy --records")

    # Fix remaining neural network references
    content = content.replace("Neural Network Validation", "COBOL Parsing Validation")
    content = content.replace("attention computation", "field processing")
    content = content.replace("KV cache", "field cache")

    # Clean up garbled validation patterns
    content = re.sub(r'test_dequantize_cpu_and_gpu_paths', 'enterprise_performance_validation', content)
    content = re.sub(r'COPYBOOK_DATA="[^"]*"', 'COPYBOOK_TEST_DATA="examples/test.cpy"', content)

    # Only write if content changed
    if content != original_content:
        with open(filepath, 'w') as f:
            f.write(content)
        print(f"  ✓ Cleaned up {filepath.name}")
        return True
    else:
        print(f"  - No cleanup needed for {filepath.name}")
        return False

def main():
    """Process all agent files for final cleanup."""
    if not AGENTS_DIR.exists():
        print(f"Error: Directory {AGENTS_DIR} does not exist")
        return

    agent_files = list(AGENTS_DIR.glob("*.md"))
    if not agent_files:
        print(f"No .md files found in {AGENTS_DIR}")
        return

    print(f"Found {len(agent_files)} agent files for final cleanup")

    cleaned_count = 0
    for agent_file in sorted(agent_files):
        if final_cleanup_agent(agent_file):
            cleaned_count += 1

    print(f"\nCompleted! Final cleanup applied to {cleaned_count} of {len(agent_files)} agent files.")

if __name__ == "__main__":
    main()