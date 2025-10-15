@echo off
setlocal enabledelayedexpansion
set PERF=1
set RUSTFLAGS=-C target-cpu=native
cargo bench -p copybook-bench -- --output-format json > target\perf.json
if not exist scripts\bench mkdir scripts\bench
copy /Y target\perf.json scripts\bench\perf.json >NUL
echo ✅ receipts: scripts\bench\perf.json
