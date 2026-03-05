# Friction Log

## PowerShell Instability (2026-03-01)
- PS sessions die immediately on H:\Code\Rust\copybook-rs
- Workaround: use Python subprocess, batch files, file-based output capture
- Root cause: likely resource exhaustion from many background agent processes

## Build Time
- Full workspace build: ~2-3 min (release)
- Full test suite: ~3-5 min (nextest)
- Incremental builds much faster after initial
