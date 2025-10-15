@echo off
setlocal enabledelayedexpansion
set PERF=1
set RUSTFLAGS=-C target-cpu=native
if "%BENCH_FILTER%"=="" (
  set "BENCH_FILTER=slo_validation"
)
cargo bench -p copybook-bench -- %BENCH_FILTER% --output-format json --quiet > target\perf.json
if not exist scripts\bench mkdir scripts\bench
copy /Y target\perf.json scripts\bench\perf.json >NUL
echo ✅ receipts: scripts\bench\perf.json

powershell -NoProfile -Command ^
  "if (Test-Path scripts/bench/perf.json) { " ^
  "  \$proc = Get-Process -Id \$PID; " ^
  "  \$rssMB = [math]::Round((\$proc.WorkingSet64 / 1MB), 0); " ^
  "  \$summary = @{display_mibps=0; comp3_mibps=0; max_rss_mib=\$rssMB} | ConvertTo-Json; " ^
  "  \$perf = Get-Content scripts/bench/perf.json | ConvertFrom-Json; " ^
  "  \$perf | Add-Member -Name summary -Value (\$summary | ConvertFrom-Json) -MemberType NoteProperty -Force; " ^
  "  \$perf | ConvertTo-Json -Depth 10 | Set-Content scripts/bench/perf.json; " ^
  "  Write-Host '✅ appended summary to scripts/bench/perf.json'; " ^
  "} else { " ^
  "  Write-Warning 'perf.json missing; skipping summary append'; " ^
  "}"
