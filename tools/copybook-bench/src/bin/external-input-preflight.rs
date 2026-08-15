// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use copybook_bench::external_input::publish_external_input_preflight;

fn main() -> Result<()> {
    let mut arguments = env::args_os();
    let program = arguments
        .next()
        .unwrap_or_else(|| "external-input-preflight".into());
    let usage = || {
        format!(
            "usage: {} <manifest.json> <output.json>",
            PathBuf::from(&program).display()
        )
    };
    let manifest = arguments.next().map(PathBuf::from).context(usage())?;
    let output = arguments.next().map(PathBuf::from).context(usage())?;
    ensure!(arguments.next().is_none(), usage());
    let commit = env::var("GITHUB_SHA").context("GITHUB_SHA must identify the report commit")?;
    publish_external_input_preflight(&manifest, &output, &commit)?;
    Ok(())
}
