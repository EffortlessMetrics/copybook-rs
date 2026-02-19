#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""
Script to fix issues introduced by the bulk agent adaptation.
"""

import os
import re
from pathlib import Path

# Base directory for review agents
AGENTS_DIR = Path(".claude/agents4/review/")

def fix_agent_file(filepath):
    """Fix specific issues in agent files."""
    print(f"Fixing {filepath.name}...")

    with open(filepath, 'r') as f:
        content = f.read()

    original_content = content

    # Fix incorrect "copybook:" entries in YAML frontmatter
    content = re.sub(r'^copybook: sonnet$', 'model: sonnet', content, flags=re.MULTILINE)

    # Fix doubled workspace flags
    content = content.replace('--workspace --workspace', '--workspace')
    content = content.replace('--workspace --release --workspace', '--workspace --release')

    # Fix garbled command patterns
    content = content.replace('copybook-core parsing', 'copybook-core')
    content = content.replace('copybook-codec parsing', 'copybook-codec')
    content = content.replace('deCOBOL parsing', 'data conversion')

    # Fix specific performance patterns
    content = content.replace('I2S: 4.1 GiB/s, TL1: 560 MiB/s, TL2: 99.7%', 'DISPLAY: ≥4.1 GiB/s, COMP-3: ≥560 MiB/s')
    content = content.replace('copybook.gguf', 'copybook.cpy')
    content = content.replace('copybooks/bitnet/', 'examples/')

    # Fix garbled technical terms
    content = content.replace('weight deCOBOL parsing', 'field layout computation')
    content = content.replace('COBOL parsing/deCOBOL parsing', 'COBOL parsing/data conversion')
    content = content.replace('COBOL parsing kernels (DISPLAY, COMP, COMP-3)', 'COBOL parsing engines (lexer, parser, AST)')

    # Fix evidence patterns
    content = content.replace('records/sec', 'GiB/s for DISPLAY, MiB/s for COMP-3')

    # Fix specific technical terms
    content = content.replace('BITNET_DETERMINISTIC=1', 'deterministic parsing')
    content = content.replace('BITNET_EBCDIC', 'COPYBOOK_DATA')

    # Fix workspace crate references
    content = re.sub(r'bitnet-\*', 'copybook-*', content)

    # Only write if content changed
    if content != original_content:
        with open(filepath, 'w') as f:
            f.write(content)
        print(f"  ✓ Fixed {filepath.name}")
        return True
    else:
        print(f"  - No fixes needed for {filepath.name}")
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

    print(f"Found {len(agent_files)} agent files to fix")

    fixed_count = 0
    for agent_file in sorted(agent_files):
        if fix_agent_file(agent_file):
            fixed_count += 1

    print(f"\nCompleted! Fixed {fixed_count} of {len(agent_files)} agent files.")

if __name__ == "__main__":
    main()