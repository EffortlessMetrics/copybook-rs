@echo off
cd /d H:\Code\Rust\copybook-rs
cargo build --package copybook-bdd --bin bdd 2> build_log.txt
echo BUILD_EXIT=%ERRORLEVEL% > build_result.txt
type build_log.txt >> build_result.txt
if %ERRORLEVEL% NEQ 0 goto :eof
H:\Code\Rust\copybook-rs\target\debug\bdd.exe > bdd_stdout.txt 2> bdd_stderr.txt
echo TEST_EXIT=%ERRORLEVEL% > test_done.txt
