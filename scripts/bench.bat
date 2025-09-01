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

echo Running benchmarks...
cargo bench -p copybook-bench

echo Benchmark results saved to target\criterion\
echo Open target\criterion\report\index.html to view detailed results