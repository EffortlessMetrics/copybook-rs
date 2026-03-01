// SPDX-License-Identifier: AGPL-3.0-or-later
//! Health check utilities for benchmark environment validation.
//!
//! Provides pre-flight checks to verify the benchmark environment
//! is properly configured for reliable performance measurements.

use std::path::Path;

/// Status of an individual health check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HealthStatus {
    /// Check passed successfully.
    Pass,
    /// Check passed with a warning (non-critical).
    Warning,
    /// Check failed (critical issue).
    Fail,
}

/// Result of a single health check.
#[derive(Debug, Clone)]
pub struct HealthCheck {
    /// Name of the check component.
    pub name: String,
    /// Check result status.
    pub status: HealthStatus,
    /// Human-readable message describing the result.
    pub message: String,
}

/// Run all health checks and return results.
#[must_use]
pub fn run_health_checks(baseline_path: &Path) -> Vec<HealthCheck> {
    #[cfg(target_os = "linux")]
    let mut checks = vec![
        check_rust_version(),
        check_baseline_exists(baseline_path),
        check_disk_space(),
        check_memory(),
    ];

    #[cfg(not(target_os = "linux"))]
    let checks = vec![
        check_rust_version(),
        check_baseline_exists(baseline_path),
        check_disk_space(),
        check_memory(),
    ];

    #[cfg(target_os = "linux")]
    checks.push(check_cpu_governor());

    checks
}

fn check_rust_version() -> HealthCheck {
    let output = std::process::Command::new("rustc")
        .arg("--version")
        .output();

    match output {
        Ok(out) => {
            let version_str = String::from_utf8_lossy(&out.stdout);
            let version_str = version_str.trim();
            // Parse version like "rustc 1.92.0 (..."
            let meets_msrv = version_str
                .split_whitespace()
                .nth(1)
                .and_then(|v| {
                    let parts: Vec<&str> = v.split('.').collect();
                    if parts.len() >= 2 {
                        let major = parts[0].parse::<u32>().ok()?;
                        let minor = parts[1].parse::<u32>().ok()?;
                        Some(major > 1 || (major == 1 && minor >= 92))
                    } else {
                        None
                    }
                })
                .unwrap_or(false);

            if meets_msrv {
                HealthCheck {
                    name: "Rust version".to_string(),
                    status: HealthStatus::Pass,
                    message: format!("{version_str} (meets MSRV 1.92+)"),
                }
            } else {
                HealthCheck {
                    name: "Rust version".to_string(),
                    status: HealthStatus::Fail,
                    message: format!("{version_str} (requires MSRV 1.92+)"),
                }
            }
        }
        Err(_) => HealthCheck {
            name: "Rust version".to_string(),
            status: HealthStatus::Fail,
            message: "Cannot run rustc --version".to_string(),
        },
    }
}

fn check_baseline_exists(path: &Path) -> HealthCheck {
    if path.exists() {
        HealthCheck {
            name: "Baseline file".to_string(),
            status: HealthStatus::Pass,
            message: format!("Baseline exists: {}", path.display()),
        }
    } else {
        HealthCheck {
            name: "Baseline file".to_string(),
            status: HealthStatus::Warning,
            message: format!("No baseline found at {}", path.display()),
        }
    }
}

fn check_disk_space() -> HealthCheck {
    let temp = std::env::temp_dir();
    if temp.exists() {
        HealthCheck {
            name: "Disk space".to_string(),
            status: HealthStatus::Pass,
            message: "Temp directory is accessible".to_string(),
        }
    } else {
        HealthCheck {
            name: "Disk space".to_string(),
            status: HealthStatus::Fail,
            message: "Cannot access temp directory".to_string(),
        }
    }
}

#[cfg(target_os = "linux")]
fn check_cpu_governor() -> HealthCheck {
    match std::fs::read_to_string("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor") {
        Ok(governor) => {
            let governor = governor.trim();
            if governor == "performance" {
                HealthCheck {
                    name: "CPU governor".to_string(),
                    status: HealthStatus::Pass,
                    message: format!("CPU governor: {governor}"),
                }
            } else {
                HealthCheck {
                    name: "CPU governor".to_string(),
                    status: HealthStatus::Warning,
                    message: format!("CPU governor: {governor} (recommend: performance)"),
                }
            }
        }
        Err(_) => HealthCheck {
            name: "CPU governor".to_string(),
            status: HealthStatus::Warning,
            message: "Cannot read CPU governor (not available in this environment)".to_string(),
        },
    }
}

fn check_memory() -> HealthCheck {
    #[cfg(target_os = "linux")]
    {
        if let Ok(meminfo) = std::fs::read_to_string("/proc/meminfo") {
            for line in meminfo.lines() {
                if line.starts_with("MemAvailable:")
                    && let Some(kb_str) = line.split_whitespace().nth(1)
                    && let Ok(kb) = kb_str.parse::<u64>()
                {
                    let gb = kb / 1_048_576;
                    return if gb >= 1 {
                        HealthCheck {
                            name: "Available memory".to_string(),
                            status: HealthStatus::Pass,
                            message: format!("{gb} GB available"),
                        }
                    } else {
                        HealthCheck {
                            name: "Available memory".to_string(),
                            status: HealthStatus::Warning,
                            message: format!("{gb} GB available (recommend > 1 GB)"),
                        }
                    };
                }
            }
        }
    }
    HealthCheck {
        name: "Available memory".to_string(),
        status: HealthStatus::Pass,
        message: "Memory check not available on this platform".to_string(),
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_health_checks_run() {
        let path = std::path::PathBuf::from("/nonexistent/baseline.json");
        let checks = run_health_checks(&path);
        // Should have at least 4 checks (rust, baseline, disk, memory)
        assert!(checks.len() >= 4);
    }

    #[test]
    fn test_baseline_missing_is_warning() {
        let check = check_baseline_exists(Path::new("/nonexistent/baseline.json"));
        assert_eq!(check.status, HealthStatus::Warning);
        assert!(check.message.contains("No baseline found"));
    }

    #[test]
    fn test_baseline_exists() {
        let temp = std::env::temp_dir();
        let path = temp.join("health_test_baseline.json");
        std::fs::write(&path, "{}").expect("write test file");
        let check = check_baseline_exists(&path);
        assert_eq!(check.status, HealthStatus::Pass);
        assert!(check.message.contains("Baseline exists"));
        std::fs::remove_file(&path).ok();
    }

    #[test]
    fn test_rust_version_check() {
        let check = check_rust_version();
        // Should pass since we are running with Rust 1.92+
        assert_eq!(check.status, HealthStatus::Pass);
    }

    #[test]
    fn test_disk_space_check() {
        let check = check_disk_space();
        assert_eq!(check.status, HealthStatus::Pass);
    }
}
