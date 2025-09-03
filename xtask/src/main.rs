//! Xtask automation for copybook-rs
//!
//! This binary provides task automation and orchestration for copybook-rs development.
//! Run with `cargo xtask --help` to see available commands.

use clap::{Parser, Subcommand};
use std::process::{Command, exit};

#[derive(Parser)]
#[command(name = "xtask")]
#[command(about = "Task automation for copybook-rs development")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run comprehensive CI checks locally (replaces CI/Actions)
    Ci {
        /// Skip long-running tests
        #[arg(long)]
        quick: bool,
    },
    /// Run performance benchmarks (requires PERF=1)
    Bench {
        /// Specific crate to benchmark
        #[arg(short, long)]
        package: Option<String>,
    },
    /// Generate test fixtures and golden corpus
    Fixtures {
        /// Regenerate all test fixtures
        #[arg(long)]
        regenerate: bool,
    },
    /// Validate copybook parsing accuracy
    Validate {
        /// Path to copybook file to validate
        path: String,
    },
    /// Clean all build artifacts and generated files
    Clean {
        /// Also clean test fixtures
        #[arg(long)]
        fixtures: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Ci { quick } => run_ci(quick),
        Commands::Bench { package } => run_bench(package.as_deref()),
        Commands::Fixtures { regenerate } => run_fixtures(regenerate),
        Commands::Validate { path } => run_validate(&path),
        Commands::Clean { fixtures } => run_clean(fixtures),
    };

    if let Err(e) = result {
        eprintln!("Error: {e}");
        exit(1);
    }
}

fn run_ci(quick: bool) -> anyhow::Result<()> {
    println!("🚀 Running copybook-rs CI checks locally...");

    // Use just for orchestrated commands
    run_command("just", &["build"])?;
    run_command("just", &["test"])?;
    run_command("just", &["lint"])?;
    run_command("just", &["fmt-check"])?;

    if !quick {
        run_command("just", &["deny"])?;
        run_command("just", &["docs"])?;
    }

    println!("✅ All CI checks passed!");
    Ok(())
}

fn run_bench(package: Option<&str>) -> anyhow::Result<()> {
    if std::env::var("PERF").unwrap_or_default() != "1" {
        anyhow::bail!("PERF environment variable not set to 1. Run with: PERF=1 cargo xtask bench");
    }

    match package {
        Some(pkg) => run_command("just", &["bench-crate", pkg])?,
        None => run_command("just", &["bench"])?,
    }

    println!("✅ Benchmarks completed!");
    Ok(())
}

fn run_fixtures(regenerate: bool) -> anyhow::Result<()> {
    println!("📁 Managing copybook test fixtures...");

    if regenerate {
        println!("♻️  Regenerating all test fixtures...");
        run_command(
            "cargo",
            &[
                "run",
                "--bin",
                "copybook-cli",
                "--",
                "fixtures",
                "--regenerate",
            ],
        )?;
    } else {
        println!("📋 Validating existing fixtures...");
        run_command(
            "cargo",
            &[
                "run",
                "--bin",
                "copybook-cli",
                "--",
                "fixtures",
                "--validate",
            ],
        )?;
    }

    println!("✅ Fixture management completed!");
    Ok(())
}

fn run_validate(path: &str) -> anyhow::Result<()> {
    println!("🔍 Validating copybook: {path}");

    run_command(
        "cargo",
        &[
            "run",
            "--bin",
            "copybook-cli",
            "--",
            "parse",
            "--input",
            path,
            "--validate",
        ],
    )?;

    println!("✅ Copybook validation completed!");
    Ok(())
}

fn run_clean(fixtures: bool) -> anyhow::Result<()> {
    println!("🧹 Cleaning build artifacts...");

    run_command("just", &["clean"])?;

    if fixtures {
        println!("🧹 Cleaning test fixtures...");
        // Remove generated fixtures
        if std::path::Path::new("fixtures/generated").exists() {
            std::fs::remove_dir_all("fixtures/generated")?;
        }
    }

    println!("✅ Cleanup completed!");
    Ok(())
}

fn run_command(cmd: &str, args: &[&str]) -> anyhow::Result<()> {
    let status = Command::new(cmd).args(args).status()?;

    if !status.success() {
        anyhow::bail!("Command failed: {cmd} {}", args.join(" "));
    }

    Ok(())
}
