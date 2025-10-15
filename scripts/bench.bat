@echo off
REM Run performance benchmarks on Windows
REM Usage: scripts\bench.bat

echo Running copybook-rs performance benchmarks...
echo This requires PERF=1 environment variable to enable criterion benchmarks

if not "%PERF%"=="1" (
    echo PERF environment variable not set to 1. Benchmarks are gated behind this flag.
    echo Run with: set PERF=1 ^&^& scripts\bench.bat
    exit /b 1
)

echo Building benchmark crate...
cargo build --release -p copybook-bench

echo Running benchmarks (JSON receipts enabled)...
cargo bench -p copybook-bench -- --output-format json > target\perf.json

echo Bridging receipts to scripts\bench\perf.json for CI upload...
if not exist scripts\bench (
    mkdir scripts\bench
)
copy /Y target\perf.json scripts\bench\perf.json >nul

echo Benchmark receipts available at scripts\bench\perf.json
echo Criterion reports remain under target\criterion\
