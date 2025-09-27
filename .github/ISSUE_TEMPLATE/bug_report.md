---
name: Bug Report
about: Report a bug in copybook-rs COBOL data processing
title: '[BUG] '
labels: 'bug'
assignees: ''
---

## Bug Description
A clear and concise description of what the bug is.

## Environment
- **copybook-rs version**: [e.g., 0.3.1]
- **Rust version**: [e.g., 1.90.0]
- **Operating System**: [e.g., Ubuntu 22.04, Windows 11, macOS 14]
- **Target Platform**: [e.g., x86_64-unknown-linux-gnu]

## COBOL Context
- **Copybook features used**: [e.g., OCCURS DEPENDING ON, REDEFINES, COMP-3]
- **Data format**: [e.g., Fixed-length, RDW, Variable]
- **Codepage**: [e.g., CP037, ASCII, CP1047]
- **Record size**: [e.g., 80 bytes, variable]

## Steps to Reproduce
1. Prepare copybook: '...'
2. Create test data: '...'
3. Run command: `cargo run --bin copybook -- ...`
4. See error

## Expected Behavior
A clear and concise description of what you expected to happen.

## Actual Behavior
A clear and concise description of what actually happened.

## Sample Data
If possible, provide:
- **Copybook definition** (sanitized if needed):
```cobol
01 SAMPLE-RECORD.
   05 FIELD1 PIC X(10).
   05 FIELD2 PIC 9(5).
```

- **Test data** (hex dump or sanitized):
```
00000000: 4865 6c6c 6f20 576f 726c 6431 3233 3435  Hello World12345
```

- **Command used**:
```bash
cargo run --bin copybook -- decode sample.cpy data.bin --format fixed --codepage cp037
```

## Error Output
```
[Paste full error output here, including stack trace if available]
```

## Additional Context
Add any other context about the problem here, such as:
- Mainframe system this data originates from
- Specific COBOL compiler used to generate the copybook
- Performance impact if applicable
- Workarounds you've tried

## Checklist
- [ ] I have searched existing issues for this bug
- [ ] I have provided a minimal reproducible example
- [ ] I have included relevant environment information
- [ ] I have sanitized any sensitive data in examples