use std::env;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=GITHUB_SHA");
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/heads");
    println!("cargo:rerun-if-changed=.git/refs/remotes");
    println!("cargo:rerun-if-changed=.git/packed-refs");

    let git_sha = resolve_git_sha().unwrap_or_else(|| "unknown".to_string());
    println!("cargo:rustc-env=GIT_SHA={git_sha}");
}

fn resolve_git_sha() -> Option<String> {
    if let Ok(sha) = env::var("GITHUB_SHA") {
        return Some(shorten_sha(&sha));
    }

    if let Ok(output) = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
    {
        if output.status.success() {
            if let Ok(stdout) = String::from_utf8(output.stdout) {
                let trimmed = stdout.trim();
                if !trimmed.is_empty() {
                    return Some(trimmed.to_string());
                }
            }
        }
    }

    None
}

fn shorten_sha(input: &str) -> String {
    let trimmed = input.trim();
    if trimmed.len() <= 7 {
        trimmed.to_string()
    } else {
        trimmed[..7].to_string()
    }
}
