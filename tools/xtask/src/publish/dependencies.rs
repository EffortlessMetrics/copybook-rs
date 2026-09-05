// SPDX-License-Identifier: AGPL-3.0-or-later
//! Publication dependencies, independent of runtime feature selection.

use std::collections::{HashMap, HashSet};

use anyhow::{Result, bail};

use super::Package;

type DependencyGraph = (HashMap<String, usize>, HashMap<String, Vec<String>>);

pub(super) fn build_dependency_graph(
    packages: &[&Package],
    package_roles: &HashMap<String, String>,
    workspace_id_to_name: &HashMap<String, String>,
    workspace_name_to_id: &HashMap<String, String>,
    publishable_ids: &HashSet<String>,
    enforce_role_policy: bool,
) -> Result<DependencyGraph> {
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
            let Some(dep_id) = dependency
                .package
                .as_ref()
                .filter(|id| workspace_id_to_name.contains_key(*id))
                .or_else(|| {
                    dependency
                        .name
                        .as_ref()
                        .and_then(|name| workspace_name_to_id.get(name))
                })
            else {
                // Registry dependencies outside the workspace are not owned by
                // this release plan.
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

    fn fixture(version: &str, dependencies: Value) -> Result<(Metadata, SurfaceRegistry)> {
        // Match cargo metadata --no-deps: dependency names are the real package
        // names, including renamed dependencies; there is no resolved package ID.
        let metadata = serde_json::from_value(json!({
            "workspace_members": ["facade", "alias", "helper"],
            "packages": [
                {"id": "facade", "name": "copybook", "version": version,
                 "dependencies": dependencies},
                {"id": "alias", "name": "copybook-rs", "version": version,
                 "dependencies": [{"name": "copybook", "kind": null}]},
                {"id": "helper", "name": "z-helper", "version": version,
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
        plan.iter().map(|package| package.package.as_str()).collect()
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
    fn omitted_optional_and_build_dependencies_are_rejected() -> Result<()> {
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
        let (metadata, mut registry) = fixture(
            "0.6.0",
            json!([{"name": "z-helper", "kind": "build"}]),
        )?;
        for package in &mut registry.packages {
            if package.name == "z-helper" {
                package.boundary.role = "compat".to_string();
                package.boundary.target_disposition = "collapse".to_string();
                package.boundary.compatibility_plan = "deprecated-through-0.6".to_string();
            }
        }
        let error = rejection(&metadata, registry)?;
        assert!(
            error.contains("primary package copybook depends on compat package z-helper")
        );
        Ok(())
    }

    #[test]
    fn build_cycles_are_rejected_but_dev_back_edges_are_ignored() -> Result<()> {
        for version in ["0.5.1", "0.6.0"] {
            for kind in ["build", "dev"] {
                let (mut metadata, registry) = fixture(
                    version,
                    json!([{"name": "z-helper", "kind": "build"}]),
                )?;
                for package in &mut metadata.packages {
                    if package.name == "z-helper" {
                        package.dependencies = serde_json::from_value(json!([
                            {"name": "copybook", "kind": kind}
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
    fn external_build_and_omitted_dev_dependencies_do_not_enter_the_graph() -> Result<()> {
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
        Ok(())
    }
}
