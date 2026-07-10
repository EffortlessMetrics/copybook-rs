// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use serde::Deserialize;
use serde_json::Value;
use std::{
    collections::{HashMap, HashSet},
    process::Command,
};

#[derive(Debug, Deserialize)]
struct Metadata {
    workspace_members: Vec<String>,
    packages: Vec<Package>,
}

#[derive(Debug, Deserialize)]
struct Package {
    id: String,
    #[serde(rename = "name")]
    _name: String,
    #[serde(default)]
    publish: Option<Value>,
    #[serde(default)]
    dependencies: Vec<Dependency>,
}

#[derive(Debug, Deserialize)]
struct Dependency {
    #[serde(rename = "name")]
    _name: String,
    #[serde(default)]
    kind: Option<String>,
    #[serde(default)]
    package: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum PlanFormat {
    Plain,
    Json,
}

/// Build a topologically ordered publish plan, and optionally validate the plan.
///
/// # Errors
///
/// Returns an error if `cargo metadata` fails, if the output cannot be parsed,
/// if the publish graph cannot be sorted, or if required facade ordering
/// constraints are violated.
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
            for crate_name in plan {
                println!("{crate_name}");
            }
        }
        PlanFormat::Json => {
            let json = serde_json::to_string_pretty(&plan)?;
            println!("{json}");
        }
    }

    Ok(())
}

fn build_publish_plan() -> Result<Vec<String>> {
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

    let plan = ordered_publish_plan(metadata)?;
    Ok(plan)
}

fn ordered_publish_plan(metadata: Metadata) -> Result<Vec<String>> {
    let workspace_members = metadata
        .workspace_members
        .into_iter()
        .collect::<HashSet<_>>();

    let publishable_packages = metadata
        .packages
        .into_iter()
        .filter(|package| {
            workspace_members.contains(&package.id)
                && is_publishable_package(package.publish.as_ref())
        })
        .collect::<Vec<_>>();

    if publishable_packages.is_empty() {
        return Ok(Vec::new());
    }

    let mut publishable_ids = HashSet::new();
    let mut id_to_name = HashMap::new();
    for package in &publishable_packages {
        publishable_ids.insert(package.id.clone());
        id_to_name.insert(package.id.clone(), package._name.clone());
    }

    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut dependents: HashMap<String, Vec<String>> = HashMap::new();
    let mut seen_edges = HashSet::new();

    for package in &publishable_packages {
        in_degree.entry(package._name.clone()).or_insert(0);
    }

    for package in &publishable_packages {
        for dependency in package
            .dependencies
            .iter()
            .filter(|dep| dep.kind.as_deref() == Some("normal"))
        {
            let Some(dep_id) = &dependency.package else {
                continue;
            };
            if !publishable_ids.contains(dep_id) {
                continue;
            }

            let Some(dep_name) = id_to_name.get(dep_id) else {
                continue;
            };
            let edge = (dep_name.clone(), package._name.clone());
            if !seen_edges.insert(edge.clone()) {
                continue;
            }
            dependents
                .entry(dep_name.clone())
                .or_default()
                .push(package._name.clone());
            *in_degree.entry(package._name.clone()).or_default() += 1;
        }
    }

    let mut ready = in_degree
        .iter()
        .filter_map(|(name, degree)| (*degree == 0).then_some(name.clone()))
        .collect::<Vec<_>>();
    ready.sort_unstable();

    let mut ordered = Vec::new();
    while let Some(crate_name) = ready.first().cloned() {
        ready.remove(0);
        ordered.push(crate_name.clone());

        if let Some(children) = dependents.get(&crate_name) {
            for child in children {
                let Some(child_degree) = in_degree.get_mut(child) else {
                    bail!("dependency graph missing in-degree for package `{child}`");
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

fn validate_publish_plan(plan: &[String]) -> Result<()> {
    let mut cursor = HashMap::<String, usize>::new();
    for (index, name) in plan.iter().enumerate() {
        cursor.insert(name.clone(), index);
    }

    if !cursor.contains_key("copybook") {
        bail!("publish plan must include copybook");
    }

    if !cursor.contains_key("copybook-rs") {
        bail!("publish plan must include copybook-rs");
    }

    if let (Some(&core_pos), Some(&facade_pos)) =
        (cursor.get("copybook-core"), cursor.get("copybook"))
        && core_pos > facade_pos
    {
        bail!("copybook-core must appear before copybook");
    }

    if let (Some(&facade_pos), Some(&rs_pos)) = (cursor.get("copybook"), cursor.get("copybook-rs"))
        && facade_pos > rs_pos
    {
        bail!("copybook must appear before copybook-rs");
    }

    let mut unique = HashSet::new();
    for crate_name in plan {
        if !unique.insert(crate_name) {
            bail!("duplicate crate in publish plan: {crate_name}");
        }
    }

    Ok(())
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

    fn parse_plan(metadata_json: &str) -> Result<Vec<String>> {
        let metadata: Metadata = serde_json::from_str(metadata_json).unwrap();
        ordered_publish_plan(metadata)
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
            "copybook-core".to_string(),
            "copybook".to_string(),
            "copybook-rs".to_string(),
        ];
        assert!(validate_publish_plan(&plan).is_ok());
    }

    #[test]
    fn validate_publish_plan_rejects_duplicate_crates() {
        let plan = vec![
            "copybook-core".to_string(),
            "copybook".to_string(),
            "copybook".to_string(),
        ];
        assert!(validate_publish_plan(&plan).is_err());
    }

    #[test]
    fn validate_publish_plan_requires_facades() {
        let plan = vec!["copybook-core".to_string(), "copybook-codec".to_string()];
        assert!(validate_publish_plan(&plan).is_err());
    }
}
