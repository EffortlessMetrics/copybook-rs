// SPDX-License-Identifier: AGPL-3.0-or-later
//! Memory measurement utilities for benchmark diagnostics.
//!
//! Provides RSS (Resident Set Size) and file descriptor monitoring
//! for benchmark memory profiling on Linux systems.

/// Get the current process RSS (Resident Set Size) in bytes.
///
/// On Linux, reads from `/proc/self/status`.
/// Returns `None` on unsupported platforms or if measurement fails.
#[must_use]
pub fn get_rss_bytes() -> Option<usize> {
    #[cfg(target_os = "linux")]
    {
        let status = std::fs::read_to_string("/proc/self/status").ok()?;
        for line in status.lines() {
            if line.starts_with("VmRSS:") {
                let kb: usize = line.split_whitespace().nth(1)?.parse().ok()?;
                return Some(kb * 1024);
            }
        }
        None
    }
    #[cfg(not(target_os = "linux"))]
    {
        None
    }
}

/// Get the number of open file descriptors for the current process.
///
/// On Linux, counts entries in `/proc/self/fd`.
/// Returns `None` on unsupported platforms or if measurement fails.
#[must_use]
pub fn get_open_fd_count() -> Option<usize> {
    #[cfg(target_os = "linux")]
    {
        std::fs::read_dir("/proc/self/fd").ok().map(Iterator::count)
    }
    #[cfg(not(target_os = "linux"))]
    {
        None
    }
}
