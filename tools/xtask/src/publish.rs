// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs,
    process::Command,
};

mod dependencies;

use dependencies::build_dependency_graph;

const REGISTRY_PATH: &str = "docs/stability/surface-registry.json";
const TARGET_RELEASE_LINE: &str = "0.6";

#[derive(Debug, Deserialize)]
struct Metadata {
    workspace_members: Vec<String>,
    packages: Vec<Package>,
}

#[derive(Debug, Deserialize)]
struct Package {
    id: String,
    name: String,
    #[serde(default = "default_version")]
    version: String,
    #[serde(default)]
    publish: Option<Value>,
    #[serde(default)]
    dependencies: Vec<Dependency>,
}

#[derive(Debug, Deserialize)]
struct SurfaceRegistry {
    packages: Vec<RegistryPackage>,
}

#[derive(Debug, Deserialize)]
struct RegistryPackage {
    name: String,
    publish: bool,
    boundary: RegistryBoundary,
}

#[derive(Debug, Deserialize)]
struct RegistryBoundary {
    role: String,
    target_disposition: String,
    compatibility_plan: String,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
struct PlanPackage {
    package: String,
    version: String,
    role: String,
    dependency_reason: String,
    compatibility_status: String,
}

type RoleRegistryIndex = (
    HashMap<String, String>,
    HashMap<String, PlanPackage>,
    HashMap<String, String>,
);

#[derive(Debug, Deserialize)]
struct Dependency {
    #[serde(default)]
    kind: Option<String>,
    #[serde(default)]
    name: Option<String>,
    #[serde(default)]
    package: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum PlanFormat {
    Plain,
    Json,
}

/// Compute and emit the publish plan.
///
/// # Errors
///
/// Returns an error if `cargo metadata` fails, if the metadata cannot be parsed, or if
/// the resulting publish plan fails validation.
#[inline]
pub fn run_plan(format: PlanFormat, check_only: bool) -> Result<()> {
    let plan = build_publish_plan()?;
    if check_only {
        validate_publish_plan(&plan)?;
        println!(
            "publish plan validated: {} publishable crate(s)",
            plan.len()
        );
        return Ok(());
    }

    match format {
        PlanFormat::Plain => {
            for package in plan {
                println!("{}", package.package);
            }
        }
        PlanFormat::Json => {
            let json = serde_json::to_string_pretty(&plan)?;
            println!("{json}");
        }
    }

    Ok(())
}

fn build_publish_plan() -> Result<Vec<PlanPackage>> {
    let output = Command::new("cargo")
        .args(["metadata", "--format-version", "1", "--no-deps"])
        .output()
        .context("failed to execute cargo metadata")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("cargo metadata failed:\n{stderr}");
    }

    let metadata_text =
        String::from_utf8(output.stdout).context("cargo metadata output not valid UTF-8")?;
    let metadata: Metadata =
        serde_json::from_str(&metadata_text).context("failed to parse cargo metadata output")?;
    let registry_text =
        fs::read_to_string(REGISTRY_PATH).with_context(|| format!("loading {REGISTRY_PATH}"))?;
    let registry: SurfaceRegistry =
        serde_json::from_str(&registry_text).with_context(|| format!("parsing {REGISTRY_PATH}"))?;

    let plan = ordered_publish_plan(&metadata, registry)?;
    Ok(plan)
}

fn ordered_publish_plan(
    metadata: &Metadata,
    registry: SurfaceRegistry,
) -> Result<Vec<PlanPackage>> {
    if workspace_release_line(metadata).as_deref() == Some("0.5") {
        return ordered_legacy_publish_plan(metadata, registry);
    }

    let workspace_members = metadata
        .workspace_members
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let (package_roles, package_plans, all_id_to_name) =
        index_role_registry(metadata, registry, &workspace_members)?;
    let all_name_to_id = all_id_to_name
        .iter()
        .map(|(id, name)| (name.clone(), id.clone()))
        .collect::<HashMap<_, _>>();
    let publishable_packages = metadata
        .packages
        .iter()
        .filter(|package| package_plans.contains_key(&package.name))
        .collect::<Vec<_>>();

    if publishable_packages.is_empty() {
        return Ok(Vec::new());
    }

    let mut publishable_ids = HashSet::new();
    for package in &publishable_packages {
        publishable_ids.insert(package.id.clone());
    }

    let (in_degree, dependents) = build_dependency_graph(
        &publishable_packages,
        &package_roles,
        &all_id_to_name,
        &all_name_to_id,
        &publishable_ids,
        true,
    )?;
    topological_order(package_plans, in_degree, &dependents)
}

fn workspace_release_line(metadata: &Metadata) -> Option<String> {
    let workspace_members = metadata
        .workspace_members
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let version = metadata
        .packages
        .iter()
        .find(|package| package.name == "copybook" && workspace_members.contains(&package.id))?
        .version
        .as_str();
    let mut components = version.split('.');
    let major = components.next()?;
    let minor = components.next()?;
    Some(format!("{major}.{minor}"))
}

fn ordered_legacy_publish_plan(
    metadata: &Metadata,
    registry: SurfaceRegistry,
) -> Result<Vec<PlanPackage>> {
    let workspace_members = metadata
        .workspace_members
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let registry_by_name = registry
        .packages
        .into_iter()
        .map(|package| (package.name.clone(), package))
        .collect::<HashMap<_, _>>();
    let publishable_packages = metadata
        .packages
        .iter()
        .filter(|package| {
            workspace_members.contains(&package.id)
                && is_publishable_package(package.publish.as_ref())
        })
        .collect::<Vec<_>>();

    if publishable_packages.is_empty() {
        return Ok(Vec::new());
    }

    let mut package_roles = HashMap::new();
    let mut package_plans = HashMap::new();
    for package in &publishable_packages {
        let Some(registry_package) = registry_by_name.get(&package.name) else {
            bail!(
                "{REGISTRY_PATH} is missing workspace package {}",
                package.name
            );
        };
        package_roles.insert(package.name.clone(), registry_package.boundary.role.clone());
        package_plans.insert(
            package.name.clone(),
            PlanPackage {
                package: package.name.clone(),
                version: package.version.clone(),
                role: registry_package.boundary.role.clone(),
                dependency_reason: "manifest publishable package".to_string(),
                compatibility_status: "legacy-release-line".to_string(),
            },
        );
    }

    let all_id_to_name = metadata
        .packages
        .iter()
        .filter(|package| workspace_members.contains(&package.id))
        .map(|package| (package.id.clone(), package.name.clone()))
        .collect::<HashMap<_, _>>();
    let all_name_to_id = all_id_to_name
        .iter()
        .map(|(id, name)| (name.clone(), id.clone()))
        .collect::<HashMap<_, _>>();
    let publishable_ids = publishable_packages
        .iter()
        .map(|package| package.id.clone())
        .collect::<HashSet<_>>();
    let (in_degree, dependents) = build_dependency_graph(
        &publishable_packages,
        &package_roles,
        &all_id_to_name,
        &all_name_to_id,
        &publishable_ids,
        false,
    )?;
    topological_order(package_plans, in_degree, &dependents)
}

fn index_role_registry(
    metadata: &Metadata,
    registry: SurfaceRegistry,
    workspace_members: &HashSet<String>,
) -> Result<RoleRegistryIndex> {
    let registry_package_count = registry.packages.len();
    let registry_by_name = registry
        .packages
        .into_iter()
        .map(|package| (package.name.clone(), package))
        .collect::<BTreeMap<_, _>>();
    if registry_by_name.len() != registry_package_count {
        bail!("{REGISTRY_PATH} contains duplicate package names");
    }

    let metadata_names = metadata
        .packages
        .iter()
        .map(|package| package.name.as_str())
        .collect::<HashSet<_>>();
    if let Some(name) = registry_by_name
        .keys()
        .find(|name| !metadata_names.contains(name.as_str()))
    {
        bail!("{REGISTRY_PATH} names package {name}, which is absent from cargo metadata");
    }

    let all_id_to_name = metadata
        .packages
        .iter()
        .filter(|package| workspace_members.contains(&package.id))
        .map(|package| (package.id.clone(), package.name.clone()))
        .collect::<HashMap<_, _>>();
    let mut package_roles = HashMap::new();
    let mut package_plans = HashMap::new();
    for package in &metadata.packages {
        if !workspace_members.contains(&package.id) {
            continue;
        }
        let Some(registry_package) = registry_by_name.get(&package.name) else {
            bail!(
                "{REGISTRY_PATH} is missing workspace package {}",
                package.name
            );
        };
        let manifest_publishable = is_publishable_package(package.publish.as_ref());
        if manifest_publishable != registry_package.publish {
            bail!(
                "package {} publishability ({manifest_publishable}) disagrees with {REGISTRY_PATH} ({})",
                package.name,
                registry_package.publish
            );
        }
        validate_role(&package.name, &registry_package.boundary.role)?;
        package_roles.insert(package.name.clone(), registry_package.boundary.role.clone());
        if let Some(plan) = plan_package(package, registry_package)? {
            package_plans.insert(package.name.clone(), plan);
        }
    }
    Ok((package_roles, package_plans, all_id_to_name))
}

fn topological_order(
    mut package_plans: HashMap<String, PlanPackage>,
    mut in_degree: HashMap<String, usize>,
    dependents: &HashMap<String, Vec<String>>,
) -> Result<Vec<PlanPackage>> {
    let mut ready = in_degree
        .iter()
        .filter_map(|(name, degree)| (*degree == 0).then_some(name.clone()))
        .collect::<Vec<_>>();
    ready.sort_unstable();

    let mut ordered = Vec::new();
    while let Some(crate_name) = ready.first().cloned() {
        ready.remove(0);
        let Some(package) = package_plans.remove(&crate_name) else {
            bail!("publish plan metadata missing for package {crate_name}");
        };
        ordered.push(package);

        if let Some(children) = dependents.get(&crate_name) {
            for child in children {
                let Some(child_degree) = in_degree.get_mut(child) else {
                    bail!("in-degree missing for dependency {child}");
                };
                if *child_degree > 0 {
                    *child_degree -= 1;
                }
                if *child_degree == 0 {
                    ready.push(child.clone());
                }
            }
        }
        ready.sort_unstable();
        ready.dedup();
    }

    if ordered.len() != in_degree.len() {
        bail!("failed to resolve publish order: dependency cycle or missing dependency");
    }

    Ok(ordered)
}

fn validate_publish_plan(plan: &[PlanPackage]) -> Result<()> {
    let mut cursor = HashMap::<String, usize>::new();
    for (index, package) in plan.iter().enumerate() {
        cursor.insert(package.package.clone(), index);
    }

    if !cursor.contains_key("copybook") {
        bail!("publish plan must include copybook");
    }

    if !cursor.contains_key("copybook-rs") {
        bail!("publish plan must include copybook-rs");
    }

    match (cursor.get("copybook-core"), cursor.get("copybook")) {
        (Some(&core_pos), Some(&facade_pos)) if core_pos > facade_pos => {
            bail!("copybook-core must appear before copybook");
        }
        _ => {}
    }

    match (cursor.get("copybook"), cursor.get("copybook-rs")) {
        (Some(&facade_pos), Some(&rs_pos)) if facade_pos > rs_pos => {
            bail!("copybook must appear before copybook-rs");
        }
        _ => {}
    }

    let mut unique = HashSet::new();
    for package in plan {
        if !unique.insert(&package.package) {
            bail!("duplicate crate in publish plan: {}", package.package);
        }
    }

    Ok(())
}

fn default_version() -> String {
    "0.0.0".to_string()
}

fn validate_role(name: &str, role: &str) -> Result<()> {
    if matches!(
        role,
        "primary" | "alias" | "adapter" | "compat" | "retiring" | "internal-tool" | "contract"
    ) {
        return Ok(());
    }
    bail!("package {name} has unsupported publish role {role}")
}

fn plan_package(
    package: &Package,
    registry_package: &RegistryPackage,
) -> Result<Option<PlanPackage>> {
    let role = registry_package.boundary.role.as_str();
    let (selected, compatibility_status, dependency_reason): (bool, String, &str) = match role {
        "primary" => match registry_package.boundary.target_disposition.as_str() {
            "keep" => (
                true,
                "not-applicable".to_string(),
                "required primary package",
            ),
            "conditional" => return Ok(None),
            disposition => {
                bail!(
                    "primary package {} has unsupported target disposition {disposition}",
                    package.name
                );
            }
        },
        "alias" => {
            if registry_package.boundary.target_disposition != "keep" {
                bail!(
                    "alias package {} must have target disposition keep, found {}",
                    package.name,
                    registry_package.boundary.target_disposition
                );
            }
            (
                true,
                "permanent-alias".to_string(),
                "permanent alias after canonical facade",
            )
        }
        "adapter" => {
            let selected = registry_package.boundary.target_disposition == "keep";
            (
                selected,
                if selected {
                    "selected".to_string()
                } else {
                    "conditional".to_string()
                },
                "selected retained adapter",
            )
        }
        "contract" => {
            let selected = registry_package.boundary.target_disposition == "keep";
            (
                selected,
                if selected {
                    "selected".to_string()
                } else {
                    "conditional".to_string()
                },
                "selected external contract",
            )
        }
        "compat" => {
            let selected = compatibility_window_active(&registry_package.boundary);
            (
                selected,
                if selected {
                    format!("active-through-{TARGET_RELEASE_LINE}")
                } else {
                    "expired".to_string()
                },
                "active 0.6 compatibility window",
            )
        }
        "retiring" | "internal-tool" => return Ok(None),
        _ => bail!(
            "package {} has unsupported publish role {role}",
            package.name
        ),
    };

    Ok(selected.then(|| PlanPackage {
        package: package.name.clone(),
        version: package.version.clone(),
        role: role.to_string(),
        dependency_reason: dependency_reason.to_string(),
        compatibility_status: compatibility_status.clone(),
    }))
}

fn compatibility_window_active(boundary: &RegistryBoundary) -> bool {
    boundary.target_disposition == "collapse"
        && matches!(
            boundary.compatibility_plan.as_str(),
            "implementation-free-forwarder-through-0.6-with-finite-window"
                | "deprecated-through-0.6"
        )
}

fn is_publishable_package(publish: Option<&Value>) -> bool {
    match publish {
        Some(Value::Bool(false)) => false,
        Some(Value::Array(values)) => !values.is_empty(),
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_registry(metadata: &Metadata) -> SurfaceRegistry {
        SurfaceRegistry {
            packages: metadata
                .packages
                .iter()
                .map(|package| {
                    let role = if package.name == "copybook-rs" {
                        "alias"
                    } else if !is_publishable_package(package.publish.as_ref()) {
                        "internal-tool"
                    } else {
                        "primary"
                    };
                    RegistryPackage {
                        name: package.name.clone(),
                        publish: is_publishable_package(package.publish.as_ref()),
                        boundary: RegistryBoundary {
                            role: role.to_string(),
                            target_disposition: "keep".to_string(),
                            compatibility_plan: "retained-primary-package".to_string(),
                        },
                    }
                })
                .collect(),
        }
    }

    fn parse_plan(metadata_json: &str) -> Result<Vec<String>> {
        let metadata: Metadata = serde_json::from_str(metadata_json).unwrap();
        let registry = test_registry(&metadata);
        Ok(ordered_publish_plan(&metadata, registry)?
            .into_iter()
            .map(|package| package.package)
            .collect())
    }

    fn test_entry(package: &str) -> PlanPackage {
        PlanPackage {
            package: package.to_string(),
            version: "0.5.0".to_string(),
            role: "primary".to_string(),
            dependency_reason: "required primary package".to_string(),
            compatibility_status: "not-applicable".to_string(),
        }
    }

    fn test_package(name: &str) -> Package {
        Package {
            id: format!("id-{name}"),
            name: name.to_string(),
            version: "0.5.0".to_string(),
            publish: Some(Value::Array(vec![Value::String("crates-io".to_string())])),
            dependencies: Vec::new(),
        }
    }

    fn test_registry_package(
        name: &str,
        role: &str,
        disposition: &str,
        compatibility: &str,
    ) -> RegistryPackage {
        RegistryPackage {
            name: name.to_string(),
            publish: true,
            boundary: RegistryBoundary {
                role: role.to_string(),
                target_disposition: disposition.to_string(),
                compatibility_plan: compatibility.to_string(),
            },
        }
    }

    #[test]
    fn compatibility_window_requires_active_exact_registry_token() {
        let mut boundary = RegistryBoundary {
            role: "compat".to_string(),
            target_disposition: "collapse".to_string(),
            compatibility_plan: "implementation-free-forwarder-through-0.6-with-finite-window"
                .to_string(),
        };
        assert!(compatibility_window_active(&boundary));

        boundary.compatibility_plan = "deprecated-through-0.6".to_string();
        assert!(compatibility_window_active(&boundary));

        boundary.compatibility_plan = "removed-in-0.6".to_string();
        assert!(!compatibility_window_active(&boundary));
        boundary.compatibility_plan = "forwarder-through-10.6".to_string();
        assert!(!compatibility_window_active(&boundary));
        boundary.target_disposition = "conditional".to_string();
        boundary.compatibility_plan =
            "implementation-free-forwarder-through-0.6-with-finite-window".to_string();
        assert!(!compatibility_window_active(&boundary));
    }

    #[test]
    fn legacy_release_line_keeps_manifest_publish_plan() {
        let metadata: Metadata = serde_json::from_str(
            r#"{
                "workspace_members":["id-facade", "id-compat", "id-rs"],
                "packages":[
                    {"id":"id-facade","name":"copybook","version":"0.5.1","dependencies":[
                        {"name":"copybook-codepage","package":"id-compat"}
                    ]},
                    {"id":"id-compat","name":"copybook-codepage","version":"0.5.1","dependencies":[]},
                    {"id":"id-rs","name":"copybook-rs","version":"0.5.1","dependencies":[
                        {"name":"copybook","package":"id-facade"}
                    ]}
                ]
            }"#,
        )
        .unwrap();
        let mut registry = test_registry(&metadata);
        registry
            .packages
            .iter_mut()
            .find(|package| package.name == "copybook-codepage")
            .unwrap()
            .boundary
            .role = "compat".to_string();

        assert_eq!(workspace_release_line(&metadata), Some("0.5".to_string()));
        let plan = ordered_publish_plan(&metadata, registry).unwrap();
        assert_eq!(
            plan.into_iter()
                .map(|package| package.package)
                .collect::<Vec<_>>(),
            ["copybook-codepage", "copybook", "copybook-rs"]
        );
    }

    #[test]
    fn role_policy_rejects_required_dependency_on_omitted_package() {
        let metadata: Metadata = serde_json::from_str(
            r#"{
                "workspace_members":["id-facade", "id-conditional", "id-rs"],
                "packages":[
                    {"id":"id-facade","name":"copybook","version":"0.6.0","dependencies":[
                        {"name":"copybook-fixed","package":"id-conditional"}
                    ]},
                    {"id":"id-conditional","name":"copybook-fixed","version":"0.6.0","dependencies":[]},
                    {"id":"id-rs","name":"copybook-rs","version":"0.6.0","dependencies":[
                        {"name":"copybook","package":"id-facade"}
                    ]}
                ]
            }"#,
        )
        .unwrap();
        let mut registry = test_registry(&metadata);
        registry
            .packages
            .iter_mut()
            .find(|package| package.name == "copybook-fixed")
            .unwrap()
            .boundary
            .target_disposition = "conditional".to_string();

        let error = ordered_publish_plan(&metadata, registry).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("omitted workspace package copybook-fixed")
        );
    }

    #[test]
    fn role_policy_selects_active_compat_and_omits_conditional_or_retiring_packages() {
        let primary = test_package("copybook-core");
        let compat = test_package("copybook-dialect");
        let conditional = test_package("copybook-fixed");
        let retiring = test_package("copybook-utils");

        assert!(
            plan_package(
                &primary,
                &test_registry_package(
                    "copybook-core",
                    "primary",
                    "keep",
                    "retained-primary-package"
                )
            )
            .unwrap()
            .is_some()
        );
        let compat_plan = plan_package(
            &compat,
            &test_registry_package(
                "copybook-dialect",
                "compat",
                "collapse",
                "implementation-free-forwarder-through-0.6-with-finite-window",
            ),
        )
        .unwrap()
        .unwrap();
        assert_eq!(compat_plan.compatibility_status, "active-through-0.6");
        assert!(
            plan_package(
                &conditional,
                &test_registry_package("copybook-fixed", "primary", "conditional", "conditional")
            )
            .unwrap()
            .is_none()
        );
        assert!(
            plan_package(
                &retiring,
                &test_registry_package(
                    "copybook-utils",
                    "retiring",
                    "collapse",
                    "leave-0.5.0-available-and-stop-publishing-after-primary-consumers-move",
                )
            )
            .unwrap()
            .is_none()
        );
    }

    #[test]
    fn role_policy_rejects_unknown_roles() {
        let package = test_package("copybook-core");
        assert!(validate_role("copybook-core", "unknown").is_err());
        assert!(
            plan_package(
                &package,
                &test_registry_package("copybook-core", "unknown", "keep", "")
            )
            .is_err()
        );
    }

    #[test]
    fn role_policy_rejects_primary_dependency_on_compatibility_package() {
        let metadata: Metadata = serde_json::from_str(
            r#"{
                "workspace_members":["id-facade", "id-compat"],
                "packages":[
                    {"id":"id-facade","name":"copybook","dependencies":[{"name":"copybook-codepage","package":"id-compat"}]},
                    {"id":"id-compat","name":"copybook-codepage","dependencies":[]}
                ]
            }"#,
        )
        .unwrap();
        let mut registry = test_registry(&metadata);
        registry
            .packages
            .iter_mut()
            .find(|package| package.name == "copybook-codepage")
            .unwrap()
            .boundary
            .role = "compat".to_string();
        assert!(ordered_publish_plan(&metadata, registry).is_err());
    }

    #[test]
    fn publish_plan_orders_dependencies() {
        let plan = parse_plan(
            r#"{
                "workspace_members":["id-core", "id-codec", "id-facade", "id-rs", "id-tool"],
                "packages":[
                    {"id":"id-tool","name":"copybook-scripts","publish":false,"dependencies":[]},
                    {"id":"id-core","name":"copybook-core","dependencies":[]},
                    {"id":"id-codec","name":"copybook-codec","publish":["crates-io"],"dependencies":[
                        {"name":"copybook-core","kind":"normal","package":"id-core"}
                    ]},
                    {"id":"id-facade","name":"copybook","dependencies":[
                        {"name":"copybook-core","kind":"normal","package":"id-core"},
                        {"name":"copybook-codec","kind":"normal","package":"id-codec"}
                    ]},
                    {"id":"id-rs","name":"copybook-rs","dependencies":[
                        {"name":"copybook","kind":"normal","package":"id-facade"}
                    ]}
                ]
            }"#,
        )
        .unwrap();

        assert_eq!(
            plan,
            vec![
                "copybook-core".to_string(),
                "copybook-codec".to_string(),
                "copybook".to_string(),
                "copybook-rs".to_string(),
            ]
        );
    }

    #[test]
    fn publish_plan_orders_workspace_dependencies_without_package_field() {
        let plan = parse_plan(
            r#"{
                "workspace_members":["id-core", "id-facade", "id-rs"],
                "packages":[
                    {"id":"id-facade","name":"copybook","publish":["crates-io"],"dependencies":[
                        {"name":"copybook-core"}
                    ]},
                    {"id":"id-core","name":"copybook-core","publish":["crates-io"],"dependencies":[]},
                    {"id":"id-rs","name":"copybook-rs","publish":["crates-io"],"dependencies":[
                        {"name":"copybook"}
                    ]}
                ]
            }"#,
        )
        .unwrap();

        assert_eq!(
            plan,
            vec![
                "copybook-core".to_string(),
                "copybook".to_string(),
                "copybook-rs".to_string(),
            ]
        );
    }

    #[test]
    fn publish_plan_rejects_cycles() {
        let result = parse_plan(
            r#"{
                "workspace_members":["id-a", "id-b"],
                "packages":[
                    {"id":"id-a","name":"crate-a","dependencies":[{"name":"crate-b","kind":"normal","package":"id-b"}]},
                    {"id":"id-b","name":"crate-b","dependencies":[{"name":"crate-a","kind":"normal","package":"id-a"}]}
                ]
            }"#,
        );

        assert!(result.is_err());
    }

    #[test]
    fn publish_plan_excludes_non_publishable() {
        let plan = parse_plan(
            r#"{
                "workspace_members":["id-core", "id-private"],
                "packages":[
                    {"id":"id-core","name":"copybook-core","dependencies":[]},
                    {"id":"id-private","name":"copybook-private","publish":[],"dependencies":[
                        {"name":"copybook-core","kind":"normal","package":"id-core"}
                    ]}
                ]
            }"#,
        )
        .unwrap();

        assert_eq!(plan, vec!["copybook-core".to_string()]);
    }

    #[test]
    fn validate_publish_plan_enforces_facade_order() {
        let plan = vec![
            test_entry("copybook-core"),
            test_entry("copybook"),
            test_entry("copybook-rs"),
        ];
        assert!(validate_publish_plan(&plan).is_ok());
    }

    #[test]
    fn validate_publish_plan_rejects_duplicate_crates() {
        let plan = vec![
            test_entry("copybook-core"),
            test_entry("copybook"),
            test_entry("copybook"),
        ];
        assert!(validate_publish_plan(&plan).is_err());
    }

    #[test]
    fn validate_publish_plan_requires_facades() {
        let plan = vec![test_entry("copybook-core"), test_entry("copybook-codec")];
        assert!(validate_publish_plan(&plan).is_err());
    }
}
