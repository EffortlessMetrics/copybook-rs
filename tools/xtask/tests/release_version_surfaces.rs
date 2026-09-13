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

fn require_release_surface(
    document: &str,
    label: &str,
    selector: &str,
    expected: &str,
) -> Result<(), String> {
    let matches: Vec<_> = document
        .lines()
        .filter(|line| line.contains(selector))
        .collect();

    match matches.as_slice() {
        [] => Err(format!(
            "{label}: missing release surface containing `{selector}`"
        )),
        [line] if line.contains(expected) => Ok(()),
        [line] => Err(format!(
            "{label}: release surface drift: expected `{expected}` in `{line}`"
        )),
        _ => Err(format!(
            "{label}: ambiguous release surface: found {} lines containing `{selector}`",
            matches.len()
        )),
    }
}

#[test]
fn current_release_examples_match_workspace_version() -> Result<(), Box<dyn std::error::Error>> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let version = workspace_version(&root)?;
    let tag = format!("v{version}");

    let root_readme = fs::read_to_string(root.join("README.md"))?;
    let cli_readme = fs::read_to_string(root.join("crates/copybook-cli/README.md"))?;

    let root_surfaces = [
        (
            "root status",
            "Engineering Preview (",
            format!("Engineering Preview ({tag})"),
        ),
        (
            "root install command",
            "cargo install copybook-cli@",
            format!("cargo install copybook-cli@{version} --locked"),
        ),
        (
            "copybook fixture URL",
            "fixtures/copybooks/simple.cpy",
            format!("/raw/{tag}/fixtures/copybooks/simple.cpy"),
        ),
        (
            "data fixture URL",
            "fixtures/data/simple.bin",
            format!("/raw/{tag}/fixtures/data/simple.bin"),
        ),
        (
            "canonical facade dependency",
            "copybook = \"=",
            format!("copybook = \"={version}\""),
        ),
        (
            "source checkout instruction",
            "git checkout v",
            format!("git checkout {tag}"),
        ),
    ];

    for (label, selector, expected) in root_surfaces {
        require_release_surface(&root_readme, label, selector, &expected)
            .map_err(io::Error::other)?;
    }

    let cli_install = format!("cargo install copybook-cli@{version} --locked");
    require_release_surface(
        &cli_readme,
        "CLI README install command",
        "cargo install copybook-cli@",
        &cli_install,
    )
    .map_err(io::Error::other)?;

    Ok(())
}

#[test]
fn release_surface_check_accepts_one_current_surface() {
    let document = "before\ncargo install copybook-cli@0.6.0 --locked\nafter\n";
    assert!(
        require_release_surface(
            document,
            "install",
            "cargo install copybook-cli@",
            "cargo install copybook-cli@0.6.0 --locked",
        )
        .is_ok()
    );
}

#[test]
fn release_surface_check_rejects_stale_surface() {
    let document = "cargo install copybook-cli@0.5.0 --locked\n";
    let error = require_release_surface(
        document,
        "install",
        "cargo install copybook-cli@",
        "cargo install copybook-cli@0.6.0 --locked",
    )
    .expect_err("stale release surface must fail");
    assert!(error.contains("release surface drift"));
}

#[test]
fn release_surface_check_rejects_duplicate_surface() {
    let document = concat!(
        "cargo install copybook-cli@0.5.0 --locked\n",
        "cargo install copybook-cli@0.6.0 --locked\n"
    );
    let error = require_release_surface(
        document,
        "install",
        "cargo install copybook-cli@",
        "cargo install copybook-cli@0.6.0 --locked",
    )
    .expect_err("duplicate release surfaces must fail");
    assert!(error.contains("ambiguous release surface"));
}

#[test]
fn release_surface_check_rejects_missing_surface() {
    let error = require_release_surface(
        "no install command here\n",
        "install",
        "cargo install copybook-cli@",
        "cargo install copybook-cli@0.6.0 --locked",
    )
    .expect_err("missing release surface must fail");
    assert!(error.contains("missing release surface"));
}
