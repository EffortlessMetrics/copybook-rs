// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    process::Command,
};

const REGISTRY_PATH: &str = "docs/stability/surface-registry.json";
const DEBT_PATH: &str = "docs/architecture/package-boundary-debt.json";
const ALIAS_SOURCE_PATH: &str = "crates/copybook-rs/src/lib.rs";
const GENERATED_CONSUMER_DATA: &str = "generated-from-cargo-metadata-by-issue-644";

#[derive(Debug, Deserialize)]
struct Metadata {
    workspace_members: Vec<String>,
    packages: Vec<MetadataPackage>,
}

#[derive(Debug, Deserialize)]
struct MetadataPackage {
    id: String,
    name: String,
    #[serde(default)]
    publish: Option<Value>,
    #[serde(default)]
    dependencies: Vec<MetadataDependency>,
    #[serde(default)]
    features: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct MetadataDependency {
    name: String,
    #[serde(default)]
    kind: Option<String>,
}

#[derive(Debug, Deserialize)]
struct SurfaceRegistry {
    schema_version: String,
    packages: Vec<RegistryPackage>,
}

#[derive(Debug, Deserialize)]
struct RegistryPackage {
    name: String,
    publish: bool,
    #[serde(rename = "class")]
    stability_class: String,
    boundary: Option<Boundary>,
}

#[derive(Debug, Deserialize)]
struct Boundary {
    role: String,
    seam_type: String,
    true_owner: String,
    external_user_story: Option<String>,
    invariant_owned: Option<String>,
    preferred_facade_path: Option<String>,
    target_disposition: String,
    compatibility_plan: String,
    consumer_data: Option<String>,
    stability_class: String,
    dependency_direction: String,
    module_insufficiency_reason: String,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
struct Violation {
    id: String,
    message: String,
    packages: Vec<String>,
}

#[derive(Debug, Serialize)]
struct Report {
    schema_version: u8,
    registry_schema_version: String,
    packages: Vec<ReportPackage>,
    violations: Vec<Violation>,
}

#[derive(Debug, Serialize)]
struct ReportPackage {
    package: String,
    role: String,
    seam_type: String,
    owner: String,
    publishable: bool,
    stability_class: String,
    dependency_direction: String,
    module_insufficiency_reason: String,
    normal_dependencies: Vec<String>,
    normal_consumers: Vec<String>,
    preferred_facade_path: Option<String>,
    target_disposition: String,
    compatibility_plan: String,
    violations: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize)]
struct DebtFile {
    schema_version: u8,
    entries: Vec<DebtEntry>,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct DebtEntry {
    id: String,
    owner_issue: u64,
}

/// Validate the current package graph against the role registry and exact debt baseline.
///
/// # Errors
///
/// Returns an error when metadata cannot be loaded, a boundary rule is violated without an
/// exact baseline entry, or a resolved debt entry remains in the baseline.
#[inline]
pub fn run_check() -> Result<()> {
    let report = analyze()?;
    let debt = read_debt()?;
    validate_debt_file(&debt)?;
    validate_debt(&report.violations, &debt.entries)?;
    println!(
        "architecture check passed: {} package(s), {} tracked violation(s)",
        report.packages.len(),
        report.violations.len()
    );
    Ok(())
}

/// Print the current package architecture report.
///
/// # Errors
///
/// Returns an error when Cargo metadata or the role registry cannot be loaded.
#[inline]
pub fn run_report(json: bool) -> Result<()> {
    let report = analyze()?;
    if json {
        println!("{}", serde_json::to_string_pretty(&report)?);
        return Ok(());
    }

    println!(
        "{:<34} {:<14} {:<30} {:<9} {:<16}",
        "package", "role", "owner", "publish", "disposition"
    );
    for package in &report.packages {
        println!(
            "{:<34} {:<14} {:<30} {:<9} {:<16}",
            package.package,
            package.role,
            package.owner,
            package.publishable,
            package.target_disposition
        );
    }

    if report.violations.is_empty() {
        println!("\nNo architecture violations.");
    } else {
        println!("\nTracked architecture violations:");
        for violation in &report.violations {
            println!("- {}: {}", violation.id, violation.message);
        }
    }
    Ok(())
}

/// Generate the initial exact package-boundary debt baseline.
///
/// This is an explicit maintenance command. Normal validation never refreshes the baseline.
///
/// # Errors
///
/// Returns an error when architecture analysis fails or the baseline cannot be written.
#[inline]
pub fn run_debt_generate() -> Result<()> {
    let report = analyze()?;
    let mut entries = report
        .violations
        .iter()
        .map(|violation| DebtEntry {
            id: violation.id.clone(),
            owner_issue: owner_issue(&violation.id),
        })
        .collect::<Vec<_>>();
    entries.sort();
    let debt = DebtFile {
        schema_version: 1,
        entries,
    };
    let path = Path::new(DEBT_PATH);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, format!("{}\n", serde_json::to_string_pretty(&debt)?))?;
    println!(
        "wrote {} exact architecture debt entries to {DEBT_PATH}",
        debt.entries.len()
    );
    Ok(())
}

fn analyze() -> Result<Report> {
    let metadata = load_metadata()?;
    let registry = load_registry()?;
    let workspace_ids = metadata
        .workspace_members
        .iter()
        .cloned()
        .collect::<BTreeSet<_>>();
    let workspace_packages = metadata
        .packages
        .iter()
        .filter(|package| workspace_ids.contains(&package.id))
        .collect::<Vec<_>>();
    let workspace_names = workspace_packages
        .iter()
        .map(|package| package.name.clone())
        .collect::<BTreeSet<_>>();

    let mut violations = Vec::new();
    let registry_by_name = index_registry(&registry, &mut violations);
    validate_registry_coverage(
        &workspace_names,
        &registry_by_name,
        &mut violations,
    );
    let consumers = build_consumers(&workspace_packages, &workspace_names);

    for package in &workspace_packages {
        validate_package(
            package,
            &workspace_names,
            &registry_by_name,
            &mut violations,
        );
    }
    validate_alias(&workspace_packages, &registry_by_name, &mut violations)?;

    let violations = normalize_violations(violations);
    let packages = build_report_rows(
        &workspace_packages,
        &registry_by_name,
        &consumers,
        &violations,
    );

    Ok(Report {
        schema_version: 2,
        registry_schema_version: registry.schema_version,
        packages,
        violations,
    })
}

fn index_registry<'a>(
    registry: &'a SurfaceRegistry,
    violations: &mut Vec<Violation>,
) -> BTreeMap<String, &'a RegistryPackage> {
    let mut by_name = BTreeMap::new();
    for package in &registry.packages {
        if by_name.contains_key(&package.name) {
            violations.push(Violation::for_package(
                format!("registry-duplicate:{}", package.name),
                format!("role registry contains duplicate package {}", package.name),
                &package.name,
            ));
            continue;
        }
        by_name.insert(package.name.clone(), package);
    }
    by_name
}

fn validate_registry_coverage(
    workspace_names: &BTreeSet<String>,
    registry_by_name: &BTreeMap<String, &RegistryPackage>,
    violations: &mut Vec<Violation>,
) {
    for name in workspace_names {
        if !registry_by_name.contains_key(name) {
            violations.push(Violation::for_package(
                format!("registry-missing:{name}"),
                format!("workspace package {name} has no role registry entry"),
                name,
            ));
        }
    }
    for name in registry_by_name.keys() {
        if !workspace_names.contains(name) {
            violations.push(Violation::for_package(
                format!("registry-stale:{name}"),
                format!("role registry contains non-workspace package {name}"),
                name,
            ));
        }
    }
}

fn build_consumers(
    workspace_packages: &[&MetadataPackage],
    workspace_names: &BTreeSet<String>,
) -> BTreeMap<String, BTreeSet<String>> {
    let mut consumers = BTreeMap::<String, BTreeSet<String>>::new();
    for package in workspace_packages {
        for dependency in normal_dependencies(package) {
            if workspace_names.contains(&dependency) {
                consumers
                    .entry(dependency)
                    .or_default()
                    .insert(package.name.clone());
            }
        }
    }
    consumers
}

fn validate_package(
    package: &MetadataPackage,
    workspace_names: &BTreeSet<String>,
    registry_by_name: &BTreeMap<String, &RegistryPackage>,
    violations: &mut Vec<Violation>,
) {
    let Some(registry_package) = registry_by_name.get(&package.name) else {
        return;
    };
    let Some(boundary) = registry_package.boundary.as_ref() else {
        violations.push(Violation::for_package(
            format!("boundary-missing:{}", package.name),
            format!("package {} has no boundary metadata", package.name),
            &package.name,
        ));
        return;
    };

    validate_boundary_contract(
        package,
        registry_package,
        boundary,
        workspace_names,
        violations,
    );
    validate_dependency_rules(package, boundary, registry_by_name, violations);
}

fn validate_boundary_contract(
    package: &MetadataPackage,
    registry_package: &RegistryPackage,
    boundary: &Boundary,
    workspace_names: &BTreeSet<String>,
    violations: &mut Vec<Violation>,
) {
    let name = package.name.as_str();
    if !valid_role(&boundary.role) {
        violations.push(Violation::for_package(
            format!("role-invalid:{name}:{}", boundary.role),
            format!("package {name} uses unknown role {}", boundary.role),
            name,
        ));
    }
    if !valid_disposition(&boundary.target_disposition) {
        violations.push(Violation::for_package(
            format!("disposition-invalid:{name}:{}", boundary.target_disposition),
            format!(
                "package {name} uses unknown target disposition {}",
                boundary.target_disposition
            ),
            name,
        ));
    }
    if valid_role(&boundary.role)
        && valid_disposition(&boundary.target_disposition)
        && !role_disposition_is_valid(&boundary.role, &boundary.target_disposition)
    {
        violations.push(Violation::for_package(
            format!("role-disposition-mismatch:{name}"),
            format!(
                "package {name} role {} is incompatible with disposition {}",
                boundary.role, boundary.target_disposition
            ),
            name,
        ));
    }

    validate_stability_contract(name, registry_package, boundary, violations);
    validate_required_boundary_text(name, boundary, violations);

    if !workspace_names.contains(&boundary.true_owner) {
        violations.push(Violation::for_package(
            format!("owner-missing:{name}:{}", boundary.true_owner),
            format!(
                "package {name} names non-workspace owner {}",
                boundary.true_owner
            ),
            name,
        ));
    }
    if matches!(boundary.role.as_str(), "compat" | "retiring")
        && boundary.true_owner == name
    {
        violations.push(Violation::for_package(
            format!("owner-self:{name}"),
            format!("{name} cannot be its own owner while marked {}", boundary.role),
            name,
        ));
    }

    let manifest_publishable = is_publishable(package.publish.as_ref());
    if manifest_publishable != registry_package.publish {
        violations.push(Violation::for_package(
            format!("publish-mismatch:{name}"),
            format!(
                "package {name} manifest publishability ({manifest_publishable}) disagrees with registry ({})",
                registry_package.publish
            ),
            name,
        ));
    }
    if registry_package.publish
        && (blank(boundary.external_user_story.as_deref())
            || blank(boundary.invariant_owned.as_deref()))
    {
        violations.push(Violation::for_package(
            format!("published-contract-incomplete:{name}"),
            format!(
                "published package {name} must declare an external or migration story and owned invariant"
            ),
            name,
        ));
    }
    if boundary.role == "internal-tool" && registry_package.publish {
        violations.push(Violation::for_package(
            format!("internal-tool-published:{name}"),
            format!("internal tool package {name} must not be publishable"),
            name,
        ));
    }
    if boundary.role != "internal-tool" && !registry_package.publish {
        violations.push(Violation::for_package(
            format!("non-tool-unpublished:{name}"),
            format!(
                "non-tool package {name} must either be publishable or reclassified as internal-tool"
            ),
            name,
        ));
    }

    if let Some(expected) = expected_dependency_direction(&boundary.role)
        && boundary.dependency_direction != expected
    {
        violations.push(Violation::for_package(
            format!("dependency-direction-mismatch:{name}"),
            format!(
                "package {name} role {} requires dependency direction {expected}, observed {}",
                boundary.role, boundary.dependency_direction
            ),
            name,
        ));
    }
}

fn validate_stability_contract(
    name: &str,
    registry_package: &RegistryPackage,
    boundary: &Boundary,
    violations: &mut Vec<Violation>,
) {
    if !valid_stability_class(&registry_package.stability_class) {
        violations.push(Violation::for_package(
            format!(
                "stability-class-invalid:{name}:{}",
                registry_package.stability_class
            ),
            format!(
                "package {name} uses unknown stability class {}",
                registry_package.stability_class
            ),
            name,
        ));
    }
    if boundary.stability_class != registry_package.stability_class {
        violations.push(Violation::for_package(
            format!("stability-class-mismatch:{name}"),
            format!(
                "package {name} boundary stability class {} disagrees with package class {}",
                boundary.stability_class, registry_package.stability_class
            ),
            name,
        ));
    }
}

fn validate_required_boundary_text(
    name: &str,
    boundary: &Boundary,
    violations: &mut Vec<Violation>,
) {
    let required = [
        ("seam_type", boundary.seam_type.as_str()),
        ("true_owner", boundary.true_owner.as_str()),
        ("target_disposition", boundary.target_disposition.as_str()),
        ("compatibility_plan", boundary.compatibility_plan.as_str()),
        ("stability_class", boundary.stability_class.as_str()),
        ("dependency_direction", boundary.dependency_direction.as_str()),
        (
            "module_insufficiency_reason",
            boundary.module_insufficiency_reason.as_str(),
        ),
    ];
    for (field, value) in required {
        if value.trim().is_empty() {
            violations.push(Violation::for_package(
                format!("boundary-field-empty:{name}:{field}"),
                format!("package {name} boundary field {field} must not be empty"),
                name,
            ));
        }
    }
    if boundary.consumer_data.as_deref() != Some(GENERATED_CONSUMER_DATA) {
        violations.push(Violation::for_package(
            format!("consumer-data-invalid:{name}"),
            format!(
                "package {name} must declare consumer_data as {GENERATED_CONSUMER_DATA}"
            ),
            name,
        ));
    }
}

fn validate_dependency_rules(
    package: &MetadataPackage,
    boundary: &Boundary,
    registry_by_name: &BTreeMap<String, &RegistryPackage>,
    violations: &mut Vec<Violation>,
) {
    let dependencies = normal_dependencies(package);
    for dependency in &dependencies {
        let Some(dependency_registry) = registry_by_name.get(dependency) else {
            continue;
        };
        let dependency_role = dependency_registry
            .boundary
            .as_ref()
            .map(|value| value.role.as_str())
            .unwrap_or_default();
        if boundary.role == "primary" && matches!(dependency_role, "compat" | "retiring") {
            violations.push(Violation::for_packages(
                format!("primary-dep:{}->{dependency}", package.name),
                format!(
                    "primary package {} depends on {dependency_role} package {dependency}",
                    package.name
                ),
                [&package.name, dependency],
            ));
        }
    }

    validate_core_direction(package, &dependencies, violations);
    validate_edge_direction(package, &dependencies, violations);

    if boundary.seam_type != "application"
        && dependencies.iter().any(|dependency| dependency == "clap")
    {
        violations.push(Violation::for_package(
            format!("library-cli-framework:{}->clap", package.name),
            format!("library package {} depends directly on Clap", package.name),
            &package.name,
        ));
    }
    if boundary.seam_type != "application" && package.features.contains_key("clap") {
        violations.push(Violation::for_package(
            format!("library-cli-feature:{}:clap", package.name),
            format!("library package {} exposes a Clap feature", package.name),
            &package.name,
        ));
    }
}

fn validate_core_direction(
    package: &MetadataPackage,
    dependencies: &[String],
    violations: &mut Vec<Violation>,
) {
    if package.name != "copybook-core" {
        return;
    }
    for dependency in dependencies {
        if dependency == "copybook-codec"
            || dependency == "copybook-cli"
            || dependency == "copybook-arrow"
            || dependency.starts_with("copybook-governance")
        {
            violations.push(Violation::for_packages(
                format!("core-upward:copybook-core->{dependency}"),
                format!("copybook-core has upward dependency on {dependency}"),
                [&package.name, dependency],
            ));
        }
    }
}

fn validate_edge_direction(
    package: &MetadataPackage,
    dependencies: &[String],
    violations: &mut Vec<Violation>,
) {
    if !matches!(
        package.name.as_str(),
        "copybook-charset" | "copybook-fixed" | "copybook-rdw"
    ) {
        return;
    }
    for dependency in dependencies {
        if dependency == "copybook-core"
            || dependency == "copybook-codec"
            || dependency == "copybook-cli"
            || dependency == "clap"
            || dependency.starts_with("copybook-governance")
        {
            violations.push(Violation::for_packages(
                format!("edge-upward:{}->{dependency}", package.name),
                format!("edge package {} depends upward on {dependency}", package.name),
                [&package.name, dependency],
            ));
        }
    }
}

fn build_report_rows(
    workspace_packages: &[&MetadataPackage],
    registry_by_name: &BTreeMap<String, &RegistryPackage>,
    consumers: &BTreeMap<String, BTreeSet<String>>,
    violations: &[Violation],
) -> Vec<ReportPackage> {
    let mut violations_by_package = BTreeMap::<String, Vec<String>>::new();
    for violation in violations {
        for package in &violation.packages {
            violations_by_package
                .entry(package.clone())
                .or_default()
                .push(violation.id.clone());
        }
    }

    let mut rows = Vec::new();
    for package in workspace_packages {
        let Some(registry_package) = registry_by_name.get(&package.name) else {
            continue;
        };
        let Some(boundary) = registry_package.boundary.as_ref() else {
            continue;
        };
        rows.push(ReportPackage {
            package: package.name.clone(),
            role: boundary.role.clone(),
            seam_type: boundary.seam_type.clone(),
            owner: boundary.true_owner.clone(),
            publishable: is_publishable(package.publish.as_ref()),
            stability_class: registry_package.stability_class.clone(),
            dependency_direction: boundary.dependency_direction.clone(),
            module_insufficiency_reason: boundary.module_insufficiency_reason.clone(),
            normal_dependencies: normal_dependencies(package),
            normal_consumers: consumers
                .get(&package.name)
                .cloned()
                .unwrap_or_default()
                .into_iter()
                .collect(),
            preferred_facade_path: boundary.preferred_facade_path.clone(),
            target_disposition: boundary.target_disposition.clone(),
            compatibility_plan: boundary.compatibility_plan.clone(),
            violations: violations_by_package
                .get(&package.name)
                .cloned()
                .unwrap_or_default(),
        });
    }
    rows.sort_by(|left, right| left.package.cmp(&right.package));
    rows
}

fn load_metadata() -> Result<Metadata> {
    let output = Command::new("cargo")
        .args([
            "metadata",
            "--locked",
            "--format-version",
            "1",
            "--no-deps",
        ])
        .output()
        .context("failed to execute cargo metadata")?;
    if !output.status.success() {
        bail!(
            "cargo metadata failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    serde_json::from_slice(&output.stdout).context("failed to parse cargo metadata")
}

fn load_registry() -> Result<SurfaceRegistry> {
    let text = fs::read_to_string(REGISTRY_PATH)
        .with_context(|| format!("failed to read {REGISTRY_PATH}"))?;
    serde_json::from_str(&text).with_context(|| format!("failed to parse {REGISTRY_PATH}"))
}

fn read_debt() -> Result<DebtFile> {
    let text = fs::read_to_string(DEBT_PATH).with_context(|| {
        format!(
            "missing architecture debt baseline {DEBT_PATH}; run architecture debt-generate explicitly"
        )
    })?;
    serde_json::from_str(&text).with_context(|| format!("failed to parse {DEBT_PATH}"))
}

fn normal_dependencies(package: &MetadataPackage) -> Vec<String> {
    package
        .dependencies
        .iter()
        .filter(|dependency| matches!(dependency.kind.as_deref(), None | Some("normal")))
        .map(|dependency| dependency.name.clone())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

fn is_publishable(publish: Option<&Value>) -> bool {
    match publish {
        Some(Value::Bool(false)) => false,
        Some(Value::Array(values)) => !values.is_empty(),
        _ => true,
    }
}

fn validate_alias(
    packages: &[&MetadataPackage],
    registry_by_name: &BTreeMap<String, &RegistryPackage>,
    violations: &mut Vec<Violation>,
) -> Result<()> {
    let Some(alias) = packages
        .iter()
        .find(|package| package.name == "copybook-rs")
    else {
        violations.push(Violation::for_package(
            "alias-missing:copybook-rs",
            "copybook-rs alias package is absent from the workspace",
            "copybook-rs",
        ));
        return Ok(());
    };

    let dependencies = normal_dependencies(alias);
    if dependencies != ["copybook"] {
        violations.push(Violation::for_package(
            "alias-dependencies:copybook-rs",
            format!(
                "copybook-rs must depend only on copybook; observed {}",
                dependencies.join(", ")
            ),
            "copybook-rs",
        ));
    }
    if !alias.features.is_empty() {
        violations.push(Violation::for_package(
            "alias-features:copybook-rs",
            "copybook-rs must not define independent features",
            "copybook-rs",
        ));
    }

    if let Some(boundary) = registry_by_name
        .get("copybook-rs")
        .and_then(|package| package.boundary.as_ref())
        && (boundary.role != "alias"
            || boundary.true_owner != "copybook"
            || boundary.target_disposition != "keep"
            || boundary.compatibility_plan != "permanent-alias")
    {
        violations.push(Violation::for_package(
            "alias-contract:copybook-rs",
            "copybook-rs must remain the permanent alias owned by copybook",
            "copybook-rs",
        ));
    }

    let source = fs::read_to_string(ALIAS_SOURCE_PATH)
        .with_context(|| format!("failed to read {ALIAS_SOURCE_PATH}"))?;
    if !alias_source_is_redirect_only(&source) {
        violations.push(Violation::for_package(
            "alias-implementation:copybook-rs",
            "copybook-rs must remain a redirect-only `pub use copybook::*;` package",
            "copybook-rs",
        ));
    }
    Ok(())
}

fn alias_source_is_redirect_only(source: &str) -> bool {
    let code = source
        .lines()
        .map(str::trim)
        .filter(|line| {
            !line.is_empty() && !line.starts_with("//") && !line.starts_with("#![")
        })
        .collect::<Vec<_>>();
    code == ["pub use copybook::*;"]
}

fn validate_debt_file(debt: &DebtFile) -> Result<()> {
    if debt.schema_version != 1 {
        bail!(
            "unsupported architecture debt schema version {}; expected 1",
            debt.schema_version
        );
    }

    let mut seen = BTreeSet::new();
    let mut previous: Option<&str> = None;
    for entry in &debt.entries {
        if !seen.insert(entry.id.as_str()) {
            bail!("duplicate architecture debt id: {}", entry.id);
        }
        if let Some(previous_id) = previous
            && previous_id >= entry.id.as_str()
        {
            bail!("architecture debt entries must be strictly sorted by id");
        }
        let expected = owner_issue(&entry.id);
        if entry.owner_issue != expected {
            bail!(
                "architecture debt {} is assigned to issue {}, expected issue {expected}",
                entry.id,
                entry.owner_issue
            );
        }
        previous = Some(&entry.id);
    }
    Ok(())
}

fn validate_debt(violations: &[Violation], debt: &[DebtEntry]) -> Result<()> {
    let current = violations
        .iter()
        .map(|violation| violation.id.clone())
        .collect::<BTreeSet<_>>();
    let recorded = debt
        .iter()
        .map(|entry| entry.id.clone())
        .collect::<BTreeSet<_>>();
    let added = current.difference(&recorded).cloned().collect::<Vec<_>>();
    let stale = recorded.difference(&current).cloned().collect::<Vec<_>>();

    if !added.is_empty() || !stale.is_empty() {
        let mut message = String::from("package architecture debt does not match current graph");
        if !added.is_empty() {
            message.push_str("\nnew violations (do not refresh the baseline):\n  - ");
            message.push_str(&added.join("\n  - "));
        }
        if !stale.is_empty() {
            message.push_str("\nresolved/stale entries (remove them explicitly):\n  - ");
            message.push_str(&stale.join("\n  - "));
        }
        bail!(message);
    }
    Ok(())
}

fn normalize_violations(violations: Vec<Violation>) -> Vec<Violation> {
    let mut by_id = BTreeMap::<String, Violation>::new();
    for violation in violations {
        if let Some(existing) = by_id.get_mut(&violation.id) {
            existing.packages.extend(violation.packages);
            existing.packages.sort();
            existing.packages.dedup();
        } else {
            by_id.insert(violation.id.clone(), violation);
        }
    }
    by_id.into_values().collect()
}

fn valid_role(role: &str) -> bool {
    matches!(
        role,
        "primary" | "alias" | "adapter" | "contract" | "compat" | "retiring" | "internal-tool"
    )
}

fn valid_disposition(disposition: &str) -> bool {
    matches!(
        disposition,
        "keep" | "keep-internal" | "conditional" | "collapse"
    )
}

fn valid_stability_class(class: &str) -> bool {
    matches!(class, "stable" | "beta" | "experimental" | "internal-dev-only")
}

fn role_disposition_is_valid(role: &str, disposition: &str) -> bool {
    match role {
        "primary" | "adapter" | "contract" => matches!(disposition, "keep" | "conditional"),
        "alias" => disposition == "keep",
        "compat" | "retiring" => disposition == "collapse",
        "internal-tool" => disposition == "keep-internal",
        _ => false,
    }
}

fn expected_dependency_direction(role: &str) -> Option<&'static str> {
    match role {
        "alias" => Some("depends-only-on-copybook"),
        "adapter" | "contract" => Some("outer-surface-depends-inward-never-owned-by-core"),
        "compat" => Some("depends-only-on-true-owner;never-consumed-by-primary"),
        "retiring" => Some("no-primary-consumers-after-owner-migration"),
        "internal-tool" => Some("may-depend-on-product;never-consumed-by-product"),
        _ => None,
    }
}

fn blank(value: Option<&str>) -> bool {
    value.is_none_or(|text| text.trim().is_empty())
}

fn owner_issue(id: &str) -> u64 {
    if id.contains("copybook-dialect") || id.contains("copybook-lexer") {
        647
    } else if id.contains("copybook-codepage") || id.contains("copybook-charset") {
        648
    } else if id.contains("copybook-cli-determinism") {
        649
    } else if id.contains("copybook-fixed") {
        650
    } else if id.contains("copybook-rdw") && !id.contains("record-io") {
        651
    } else if id.contains("copybook-record-io") {
        652
    } else if id.contains("copybook-options")
        || id.contains("copybook-zoned-format")
        || id.contains("copybook-overpunch")
    {
        653
    } else if id.contains("copybook-determinism")
        || id.contains("copybook-codec-memory")
        || id.contains("copybook-sequence-ring")
    {
        654
    } else if id.contains("reporter")
        || id.contains("corruption")
        || id.contains("safe-")
        || id.contains("copybook-utils")
        || id.contains("copybook-overflow")
    {
        655
    } else if id.contains("contract") || id.contains("governance") || id.contains("audit") {
        656
    } else if id.contains("copybook->") || id.contains("facade") {
        657
    } else {
        640
    }
}

impl Violation {
    fn for_package(
        id: impl Into<String>,
        message: impl Into<String>,
        package: &str,
    ) -> Self {
        Self {
            id: id.into(),
            message: message.into(),
            packages: vec![package.to_owned()],
        }
    }

    fn for_packages(
        id: impl Into<String>,
        message: impl Into<String>,
        packages: [&str; 2],
    ) -> Self {
        let mut packages = packages.map(str::to_owned).to_vec();
        packages.sort();
        packages.dedup();
        Self {
            id: id.into(),
            message: message.into(),
            packages,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debt_validation_rejects_new_and_stale_entries() {
        let violations = vec![
            Violation::for_package("a", "a", "copybook"),
            Violation::for_package("b", "b", "copybook"),
        ];
        let exact = vec![
            DebtEntry {
                id: "a".into(),
                owner_issue: 640,
            },
            DebtEntry {
                id: "b".into(),
                owner_issue: 640,
            },
        ];
        assert!(validate_debt(&violations, &exact).is_ok());

        let stale = vec![DebtEntry {
            id: "a".into(),
            owner_issue: 640,
        }];
        assert!(validate_debt(&violations, &stale).is_err());
    }

    #[test]
    fn debt_file_requires_sorted_unique_owner_routed_entries() {
        let valid = DebtFile {
            schema_version: 1,
            entries: vec![
                DebtEntry {
                    id: "a".into(),
                    owner_issue: 640,
                },
                DebtEntry {
                    id: "b".into(),
                    owner_issue: 640,
                },
            ],
        };
        assert!(validate_debt_file(&valid).is_ok());

        let duplicate = DebtFile {
            schema_version: 1,
            entries: vec![
                DebtEntry {
                    id: "a".into(),
                    owner_issue: 640,
                },
                DebtEntry {
                    id: "a".into(),
                    owner_issue: 640,
                },
            ],
        };
        assert!(validate_debt_file(&duplicate).is_err());

        let wrong_owner = DebtFile {
            schema_version: 1,
            entries: vec![DebtEntry {
                id: "primary-dep:copybook-core->copybook-lexer".into(),
                owner_issue: 640,
            }],
        };
        assert!(validate_debt_file(&wrong_owner).is_err());
    }

    #[test]
    fn alias_source_accepts_only_crate_metadata_and_redirect() {
        let valid = "// license\n#![forbid(unsafe_code)]\n\npub use copybook::*;\n";
        assert!(alias_source_is_redirect_only(valid));
        assert!(!alias_source_is_redirect_only(
            "pub use copybook::*;\npub fn divergent() {}\n"
        ));
    }

    #[test]
    fn role_dispositions_are_explicit() {
        assert!(role_disposition_is_valid("primary", "keep"));
        assert!(role_disposition_is_valid("primary", "conditional"));
        assert!(role_disposition_is_valid("compat", "collapse"));
        assert!(role_disposition_is_valid("internal-tool", "keep-internal"));
        assert!(!role_disposition_is_valid("compat", "keep"));
    }

    #[test]
    fn owner_issue_routes_known_convergence_seams() {
        assert_eq!(
            owner_issue("primary-dep:copybook-core->copybook-lexer"),
            647
        );
        assert_eq!(
            owner_issue("library-cli-framework:copybook-options->clap"),
            653
        );
        assert_eq!(owner_issue("edge-upward:copybook-rdw->copybook-core"), 651);
        assert_eq!(owner_issue("primary-dep:copybook->copybook-utils"), 655);
    }
}
