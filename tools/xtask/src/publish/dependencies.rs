// SPDX-License-Identifier: AGPL-3.0-or-later
//! Publication dependencies, independent of runtime feature selection.

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use anyhow::{Result, bail};

use super::{Package, REGISTRY_PATH, is_publishable_package};

type DependencyGraph = (HashMap<String, usize>, HashMap<String, Vec<String>>);

pub(super) fn build_dependency_graph(
    packages: &[&Package],
    package_roles: &HashMap<String, String>,
    workspace_id_to_name: &HashMap<String, String>,
    workspace_name_to_id: &HashMap<String, String>,
    workspace_paths: &HashMap<String, PathBuf>,
    publishable_ids: &HashSet<String>,
    enforce_role_policy: bool,
) -> Result<DependencyGraph> {
    if enforce_role_policy {
        for package in packages {
            if !is_publishable_package(package.publish.as_ref()) {
                let role = package_roles
                    .get(&package.name)
                    .map_or("unknown", String::as_str);
                bail!(
                    "selected {role} package {} is not publishable; role-aware plans require matching publishable Cargo and {REGISTRY_PATH} entries",
                    package.name
                );
            }
        }
    }

    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut dependents: HashMap<String, Vec<String>> = HashMap::new();
    let mut seen_edges = HashSet::new();

    for package in packages {
        in_degree.entry(package.name.clone()).or_insert(0);
    }

    for package in packages {
        // Optional and target-specific dependencies still enter the published
        // manifest. Build dependencies must also be available before consumers.
        // Dev dependencies do not impose a publication ordering constraint.
        for dependency in package
            .dependencies
            .iter()
            .filter(|dep| matches!(dep.kind.as_deref(), None | Some("normal" | "build")))
        {
            // Names alone are not identities: another version/source can use
            // the same name as a workspace member without joining this release.
            if dependency.source.is_some() {
                continue;
            }
            let explicit_id = dependency
                .package
                .as_ref()
                .filter(|id| workspace_id_to_name.contains_key(*id));
            let named_id = dependency
                .name
                .as_ref()
                .and_then(|name| workspace_name_to_id.get(name));
            let dep_id = if let Some(id) = explicit_id {
                Some(id)
            } else if let Some(id) = named_id {
                let Some(path) = dependency.path.as_ref() else {
                    bail!("workspace-named dependency {id} has no source or path identity");
                };
                let Some(workspace_path) = workspace_paths.get(id) else {
                    bail!("workspace manifest path missing for dependency {id}");
                };
                (workspace_path == path).then_some(id)
            } else {
                None
            };
            let Some(dep_id) = dep_id else {
                // Dependencies outside the workspace are not owned by this plan.
                continue;
            };
            let Some(dep_name) = workspace_id_to_name.get(dep_id) else {
                bail!("workspace dependency name missing for {dep_id}");
            };
            let package_role = package_roles
                .get(&package.name)
                .map(String::as_str)
                .unwrap_or_default();
            let dependency_role = package_roles
                .get(dep_name)
                .map(String::as_str)
                .unwrap_or_default();
            if enforce_role_policy
                && package_role == "primary"
                && matches!(dependency_role, "compat" | "retiring")
            {
                bail!(
                    "primary package {} depends on {dependency_role} package {dep_name}; converge the owner before publishing",
                    package.name
                );
            }

            // The legacy release line keeps its manifest-based package set,
            // not permission to reference unpublished workspace dependencies.
            if !publishable_ids.contains(dep_id) {
                bail!(
                    "selected package {} depends on omitted workspace package {dep_name}; include it in the publish plan or remove the dependency",
                    package.name
                );
            }

            // Cargo can list the same package under normal/build dependencies
            // and multiple target predicates. Count one edge, not one entry.
            if !seen_edges.insert((dep_name.clone(), package.name.clone())) {
                continue;
            }
            dependents
                .entry(dep_name.clone())
                .or_default()
                .push(package.name.clone());
            *in_degree.entry(package.name.clone()).or_default() += 1;
        }
    }

    Ok((in_degree, dependents))
}

#[cfg(test)]
mod tests {
    use anyhow::{Result, bail};
    use serde_json::{Value, json};

    use super::super::{Metadata, PlanPackage, SurfaceRegistry, ordered_publish_plan};

    fn fixture(version: &str, mut dependencies: Value) -> Result<(Metadata, SurfaceRegistry)> {
        // Match cargo metadata --no-deps: dependency names are the real package
        // names, including renamed dependencies; there is no resolved package ID.
        // Default helper references to the local workspace path. Tests may
        // supply an explicit source/path to exercise other package identities.
        if let Some(entries) = dependencies.as_array_mut() {
            for dependency in entries {
                if dependency.get("name").and_then(Value::as_str) == Some("z-helper")
                    && dependency.get("source").is_none()
                    && dependency.get("path").is_none()
                {
                    dependency["path"] = json!("/workspace/z-helper");
                }
            }
        }
        let metadata = serde_json::from_value(json!({
            "workspace_members": ["facade", "alias", "helper"],
            "packages": [
                {"id": "facade", "name": "copybook", "version": version,
                 "manifest_path": "/workspace/copybook/Cargo.toml",
                 "dependencies": dependencies},
                {"id": "alias", "name": "copybook-rs", "version": version,
                 "manifest_path": "/workspace/copybook-rs/Cargo.toml",
                 "dependencies": [{"name": "copybook", "kind": null,
                                   "source": null, "path": "/workspace/copybook"}]},
                {"id": "helper", "name": "z-helper", "version": version,
                 "manifest_path": "/workspace/z-helper/Cargo.toml",
                 "dependencies": []}
            ]
        }))?;
        let registry = serde_json::from_value(json!({
            "packages": [
                {"name": "copybook", "publish": true, "boundary": {
                    "role": "primary", "target_disposition": "keep",
                    "compatibility_plan": "retained-primary-package"}},
                {"name": "copybook-rs", "publish": true, "boundary": {
                    "role": "alias", "target_disposition": "keep",
                    "compatibility_plan": "permanent-alias"}},
                {"name": "z-helper", "publish": true, "boundary": {
                    "role": "primary", "target_disposition": "keep",
                    "compatibility_plan": "retained-primary-package"}}
            ]
        }))?;
        Ok((metadata, registry))
    }

    fn names(plan: &[PlanPackage]) -> Vec<&str> {
        plan.iter()
            .map(|package| package.package.as_str())
            .collect()
    }

    fn rejection(metadata: &Metadata, registry: SurfaceRegistry) -> Result<String> {
        match ordered_publish_plan(metadata, registry) {
            Ok(plan) => bail!("expected a rejected publish plan, got {:?}", names(&plan)),
            Err(error) => Ok(error.to_string()),
        }
    }

    #[test]
    fn build_and_optional_dependencies_precede_consumers_on_both_release_lines() -> Result<()> {
        for version in ["0.5.1", "0.6.0"] {
            for kind in [Value::Null, json!("build")] {
                for optional in [false, true] {
                    let (metadata, registry) = fixture(
                        version,
                        json!([{"name": "z-helper", "kind": kind, "optional": optional}]),
                    )?;
                    let plan = ordered_publish_plan(&metadata, registry)?;
                    assert_eq!(names(&plan), ["z-helper", "copybook", "copybook-rs"]);
                }
            }
        }
        Ok(())
    }

    #[test]
    fn renamed_target_build_dependency_uses_package_name_not_alias() -> Result<()> {
        let (metadata, registry) = fixture(
            "0.6.0",
            json!([{"name": "z-helper", "rename": "helper_alias", "kind": "build",
                    "target": "cfg(windows)", "optional": false}]),
        )?;
        let plan = ordered_publish_plan(&metadata, registry)?;
        assert_eq!(names(&plan), ["z-helper", "copybook", "copybook-rs"]);
        Ok(())
    }

    #[test]
    fn role_aware_plan_rejects_omitted_dependencies_and_selected_non_publishable_packages()
    -> Result<()> {
        for kind in [Value::Null, json!("build")] {
            for optional in [false, true] {
                let (metadata, mut registry) = fixture(
                    "0.6.0",
                    json!([{"name": "z-helper", "kind": kind, "optional": optional}]),
                )?;
                for package in &mut registry.packages {
                    if package.name == "z-helper" {
                        package.boundary.target_disposition = "conditional".to_string();
                    }
                }
                let error = rejection(&metadata, registry)?;
                assert!(error.contains("omitted workspace package z-helper"));
                assert!(error.contains("include it in the publish plan or remove"));
            }
        }

        for (role, disposition, compatibility_plan) in [
            ("primary", "keep", "retained-primary-package"),
            ("alias", "keep", "permanent-alias"),
            ("adapter", "keep", "retained-primary-package"),
            ("contract", "keep", "retained-primary-package"),
            ("compat", "collapse", "deprecated-through-0.6"),
        ] {
            let (mut metadata, mut registry) = fixture("0.6.0", json!([]))?;
            for package in &mut metadata.packages {
                if package.name == "z-helper" {
                    package.publish = Some(json!([]));
                }
            }
            for package in &mut registry.packages {
                if package.name == "z-helper" {
                    package.publish = false;
                    package.boundary.role = role.to_string();
                    package.boundary.target_disposition = disposition.to_string();
                    package.boundary.compatibility_plan = compatibility_plan.to_string();
                }
            }

            let error = rejection(&metadata, registry)?;
            assert!(error.contains(&format!(
                "selected {role} package z-helper is not publishable"
            )));
        }
        Ok(())
    }

    #[test]
    fn legacy_plan_rejects_unpublishable_workspace_dependencies() -> Result<()> {
        for kind in [Value::Null, json!("build")] {
            let (mut metadata, mut registry) = fixture(
                "0.5.1",
                json!([{"name": "z-helper", "kind": kind, "optional": true}]),
            )?;
            for package in &mut metadata.packages {
                if package.name == "z-helper" {
                    package.publish = Some(json!([]));
                }
            }
            for package in &mut registry.packages {
                if package.name == "z-helper" {
                    package.publish = false;
                    package.boundary.role = "internal-tool".to_string();
                }
            }
            let error = rejection(&metadata, registry)?;
            assert!(error.contains("omitted workspace package z-helper"));
        }
        Ok(())
    }

    #[test]
    fn primary_build_dependency_cannot_bypass_compatibility_policy() -> Result<()> {
        let (metadata, mut registry) =
            fixture("0.6.0", json!([{"name": "z-helper", "kind": "build"}]))?;
        for package in &mut registry.packages {
            if package.name == "z-helper" {
                package.boundary.role = "compat".to_string();
                package.boundary.target_disposition = "collapse".to_string();
                package.boundary.compatibility_plan = "deprecated-through-0.6".to_string();
            }
        }
        let error = rejection(&metadata, registry)?;
        assert!(error.contains("primary package copybook depends on compat package z-helper"));
        Ok(())
    }

    #[test]
    fn build_cycles_are_rejected_but_dev_back_edges_are_ignored() -> Result<()> {
        for version in ["0.5.1", "0.6.0"] {
            for kind in ["build", "dev"] {
                let (mut metadata, registry) =
                    fixture(version, json!([{"name": "z-helper", "kind": "build"}]))?;
                for package in &mut metadata.packages {
                    if package.name == "z-helper" {
                        package.dependencies = serde_json::from_value(json!([
                            {"name": "copybook", "kind": kind, "source": null,
                             "path": "/workspace/copybook"}
                        ]))?;
                    }
                }
                if kind == "build" {
                    let error = rejection(&metadata, registry)?;
                    assert!(error.contains("dependency cycle"));
                } else {
                    let plan = ordered_publish_plan(&metadata, registry)?;
                    assert_eq!(names(&plan), ["z-helper", "copybook", "copybook-rs"]);
                }
            }
        }
        Ok(())
    }

    #[test]
    fn repeated_edges_are_deduplicated_and_plan_is_order_independent() -> Result<()> {
        for version in ["0.5.1", "0.6.0"] {
            let dependencies = json!([
                {"name": "z-helper", "kind": null, "optional": true},
                {"name": "z-helper", "kind": "build", "target": "cfg(windows)"},
                {"name": "z-helper", "kind": "build", "target": "cfg(unix)"}
            ]);
            let (metadata, registry) = fixture(version, dependencies.clone())?;
            let first = ordered_publish_plan(&metadata, registry)?;
            assert_eq!(names(&first), ["z-helper", "copybook", "copybook-rs"]);
            let (mut metadata, mut registry) = fixture(version, dependencies)?;
            metadata.packages.reverse();
            metadata.workspace_members.reverse();
            registry.packages.reverse();
            for package in &mut metadata.packages {
                package.dependencies.reverse();
            }
            assert_eq!(ordered_publish_plan(&metadata, registry)?, first);
        }
        Ok(())
    }

    #[test]
    fn external_dependencies_and_omitted_roles_do_not_enter_the_graph() -> Result<()> {
        let (metadata, mut registry) = fixture(
            "0.6.0",
            json!([
                {"name": "cc", "kind": "build"},
                {"name": "z-helper", "kind": "dev"}
            ]),
        )?;
        for package in &mut registry.packages {
            if package.name == "z-helper" {
                package.boundary.target_disposition = "conditional".to_string();
            }
        }
        let plan = ordered_publish_plan(&metadata, registry)?;
        assert_eq!(names(&plan), ["copybook", "copybook-rs"]);

        for (role, disposition, compatibility_plan) in [
            ("primary", "conditional", "conditional"),
            ("adapter", "conditional", "conditional"),
            ("contract", "conditional", "conditional"),
            ("compat", "collapse", "removed-in-0.6"),
            (
                "retiring",
                "collapse",
                "leave-0.5.0-available-and-stop-publishing-after-primary-consumers-move",
            ),
            ("internal-tool", "keep-internal", "not-published"),
        ] {
            let (mut metadata, mut registry) = fixture("0.6.0", json!([]))?;
            for package in &mut metadata.packages {
                if package.name == "z-helper" {
                    package.publish = Some(json!([]));
                }
            }
            for package in &mut registry.packages {
                if package.name == "z-helper" {
                    package.publish = false;
                    package.boundary.role = role.to_string();
                    package.boundary.target_disposition = disposition.to_string();
                    package.boundary.compatibility_plan = compatibility_plan.to_string();
                }
            }

            let plan = ordered_publish_plan(&metadata, registry)?;
            assert_eq!(names(&plan), ["copybook", "copybook-rs"]);
        }
        Ok(())
    }

    #[test]
    fn same_named_external_dependencies_do_not_create_edges_or_omission_errors() -> Result<()> {
        for version in ["0.5.1", "0.6.0"] {
            for kind in [Value::Null, json!("build")] {
                for identity in [
                    json!({"source": "registry+https://github.com/rust-lang/crates.io-index"}),
                    json!({"source": "git+https://example.invalid/helper#abc"}),
                    json!({"source": null, "path": "/external/z-helper"}),
                ] {
                    let mut dependency = identity;
                    dependency["name"] = json!("z-helper");
                    dependency["kind"] = kind.clone();
                    let (mut metadata, mut registry) = fixture(version, json!([dependency]))?;
                    for package in &mut metadata.packages {
                        if package.name == "z-helper" {
                            package.publish = Some(json!([]));
                        }
                    }
                    for package in &mut registry.packages {
                        if package.name == "z-helper" {
                            package.publish = false;
                            package.boundary.role = "internal-tool".to_string();
                        }
                    }
                    let plan = ordered_publish_plan(&metadata, registry)?;
                    assert_eq!(names(&plan), ["copybook", "copybook-rs"]);
                }
            }
        }
        Ok(())
    }

    #[test]
    fn same_named_registry_dependency_does_not_create_a_false_cycle() -> Result<()> {
        let (mut metadata, registry) = fixture(
            "0.6.0",
            json!([{"name": "z-helper", "kind": "build",
                    "source": "registry+https://github.com/rust-lang/crates.io-index"}]),
        )?;
        for package in &mut metadata.packages {
            if package.name == "z-helper" {
                package.dependencies = serde_json::from_value(json!([
                    {"name": "copybook", "kind": null, "source": null,
                     "path": "/workspace/copybook"}
                ]))?;
            }
        }
        let plan = ordered_publish_plan(&metadata, registry)?;
        assert_eq!(names(&plan), ["copybook", "copybook-rs", "z-helper"]);
        Ok(())
    }

    #[test]
    fn missing_dependency_identity_fails_closed() -> Result<()> {
        let (mut metadata, registry) = fixture(
            "0.6.0",
            json!([{"name": "z-helper", "kind": "build", "source": null}]),
        )?;
        let error = rejection(&metadata, registry)?;
        assert!(error.contains("no source or path identity"));

        let (_, registry) = fixture("0.6.0", json!([]))?;
        for package in &mut metadata.packages {
            if package.name == "copybook" {
                for dependency in &mut package.dependencies {
                    dependency.path = Some("/workspace/z-helper".into());
                }
            } else if package.name == "z-helper" {
                package.manifest_path = None;
            }
        }
        let error = rejection(&metadata, registry)?;
        assert!(error.contains("workspace manifest path missing"));
        Ok(())
    }
}
