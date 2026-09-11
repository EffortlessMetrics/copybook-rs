// SPDX-License-Identifier: AGPL-3.0-or-later
use std::{fs, io, path::Path};

fn workspace_version(root: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let manifest = fs::read_to_string(root.join("Cargo.toml"))?;
    let parsed: toml::Value = toml::from_str(&manifest)?;
    parsed
        .get("workspace")
        .and_then(|workspace| workspace.get("package"))
        .and_then(|package| package.get("version"))
        .and_then(toml::Value::as_str)
        .map(str::to_owned)
        .ok_or_else(|| io::Error::other("missing [workspace.package].version").into())
}

#[test]
fn current_release_examples_match_workspace_version() -> Result<(), Box<dyn std::error::Error>> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let version = workspace_version(&root)?;
    let tag = format!("v{version}");

    let root_readme = fs::read_to_string(root.join("README.md"))?;
    let cli_readme = fs::read_to_string(root.join("crates/copybook-cli/README.md"))?;

    let root_expectations = [
        format!("Engineering Preview ({tag})"),
        format!("cargo install copybook-cli@{version} --locked"),
        format!("/raw/{tag}/fixtures/copybooks/simple.cpy"),
        format!("/raw/{tag}/fixtures/data/simple.bin"),
        format!("copybook = \"={version}\""),
        format!("git checkout {tag}"),
    ];

    for expected in root_expectations {
        assert!(
            root_readme.contains(&expected),
            "README.md release surface drift: missing `{expected}`"
        );
    }

    let cli_install = format!("cargo install copybook-cli@{version} --locked");
    assert!(
        cli_readme.contains(&cli_install),
        "crates/copybook-cli/README.md release surface drift: missing `{cli_install}`"
    );

    Ok(())
}
