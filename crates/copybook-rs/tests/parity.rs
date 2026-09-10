// SPDX-License-Identifier: AGPL-3.0-or-later
//! `copybook-rs` alias parity contracts (#657).
//!
//! The redirect crate must not grow independent behavior, feature topology,
//! or version skew relative to the canonical `copybook` facade.

/// Both crates resolve the same true-owner items through identical paths.
#[test]
fn facade_paths_match_through_alias() {
    assert_eq!(
        std::any::type_name::<copybook::core::Schema>(),
        std::any::type_name::<copybook_rs::core::Schema>()
    );
    assert_eq!(
        std::any::type_name::<copybook::charset::Codepage>(),
        std::any::type_name::<copybook_rs::charset::Codepage>()
    );
    assert_eq!(
        std::any::type_name::<copybook::error::Error>(),
        std::any::type_name::<copybook_rs::error::Error>()
    );
    assert_eq!(
        std::any::type_name::<copybook::codec::options::DecodeOptions>(),
        std::any::type_name::<copybook_rs::codec::options::DecodeOptions>()
    );
    assert_eq!(
        std::any::type_name::<copybook::framing::rdw::RdwHeader>(),
        std::any::type_name::<copybook_rs::framing::rdw::RdwHeader>()
    );
}

/// Both manifests ride the workspace version, so the alias can never skew.
#[test]
fn manifests_share_workspace_version() {
    for manifest in [
        include_str!("../Cargo.toml"),
        include_str!("../../copybook/Cargo.toml"),
    ] {
        assert!(
            manifest.contains("version.workspace = true"),
            "alias and facade must share the workspace version"
        );
    }
}

/// The alias crate depends on exactly one workspace crate: the facade.
#[test]
fn alias_depends_only_on_facade() {
    const MANIFEST: &str = include_str!("../Cargo.toml");
    let deps: Vec<&str> = MANIFEST
        .lines()
        .skip_while(|line| line.trim() != "[dependencies]")
        .skip(1)
        .take_while(|line| !line.trim_start().starts_with('['))
        .filter_map(|line| {
            let name = line.split('=').next()?.trim();
            name.starts_with("copybook")
                .then(|| name.split('.').next().unwrap_or(name).trim())
        })
        .collect();
    assert_eq!(
        deps,
        ["copybook"],
        "copybook-rs must depend only on the copybook facade"
    );
}
