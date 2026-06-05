use super::*;
use std::collections::HashMap;
impl BaselineRepository {
    pub fn new() -> Self {
        Self {
            storage_backend: StorageBackend::FileSystem {
                root_path: std::env::var("BASELINE_STORAGE_PATH")
                    .unwrap_or_else(|_| "/tmp/copybook-baselines".to_string()),
            },
            baseline_metadata: HashMap::new(),
            retention_policy: BaselineRetentionPolicy {
                max_baselines_per_branch: 10,
                retention_days: 90,
                archive_after_days: 30,
            },
        }
    }

    pub fn store_baseline(
        &mut self,
        baseline: BaselineMetadata,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let baseline_id = baseline.baseline_id.clone();
        self.baseline_metadata.insert(baseline_id.clone(), baseline);
        Ok(baseline_id)
    }

    pub fn load_baseline(
        &self,
        baseline_id: &str,
    ) -> Result<BaselineMetadata, Box<dyn std::error::Error>> {
        self.baseline_metadata
            .get(baseline_id)
            .cloned()
            .ok_or_else(|| format!("Baseline {} not found", baseline_id).into())
    }

    pub fn find_compatible_baseline(
        &self,
        environment: &EnvironmentInfo,
    ) -> Result<Option<BaselineMetadata>, Box<dyn std::error::Error>> {
        // Find baseline with matching environment characteristics
        for baseline in self.baseline_metadata.values() {
            if self.is_environment_compatible(&baseline.environment_info, environment) {
                return Ok(Some(baseline.clone()));
            }
        }
        Ok(None)
    }

    fn is_environment_compatible(
        &self,
        baseline_env: &EnvironmentInfo,
        current_env: &EnvironmentInfo,
    ) -> bool {
        // Simplified compatibility check
        baseline_env.target_triple == current_env.target_triple
            && matches!(
                (
                    &baseline_env.build_configuration.optimization_level,
                    &current_env.build_configuration.optimization_level
                ),
                (OptimizationLevel::Release, OptimizationLevel::Release)
                    | (OptimizationLevel::Debug, OptimizationLevel::Debug)
            )
    }
}
