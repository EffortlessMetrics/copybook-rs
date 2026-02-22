//! Governance grid linking COBOL support-matrix entries to feature flags.
//!
//! The grid is intentionally explicit so changes in parser capability, runtime feature flags,
//! and docs can be tracked as a first-class compatibility surface.

use crate::{feature_flags::Feature, support_matrix::FeatureId};

#[derive(Debug, Clone, Copy)]
pub struct GovernedFeatureBinding {
    /// Support-matrix feature identity.
    pub support_id: FeatureId,
    /// Runtime flags that enable or are explicitly related to this support item.
    pub feature_flags: &'static [Feature],
    /// Short rationale describing how the linkage is managed.
    pub rationale: &'static str,
}

static SIGN_SEPARATE_MAPPING: [Feature; 1] = [Feature::SignSeparate];
static RENAMES_R4R6_MAPPING: [Feature; 1] = [Feature::RenamesR4R6];
static COMP12_MAPPING: [Feature; 2] = [Feature::Comp1, Feature::Comp2];

static GOVERNANCE_BINDINGS: [GovernedFeatureBinding; 7] = [
    GovernedFeatureBinding {
        support_id: FeatureId::Level88Conditions,
        feature_flags: &[],
        rationale: "Level-88 condition values are parsed by core grammar; no explicit runtime toggle.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::Level66Renames,
        feature_flags: &RENAMES_R4R6_MAPPING,
        rationale: "RENAMES advanced scenarios map to `RenamesR4R6` gate.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::OccursDepending,
        feature_flags: &[],
        rationale: "OCCURS DEPENDING support is governed by layout behavior flags in parser options.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::EditedPic,
        feature_flags: &[],
        rationale: "Edited PIC support is part of parser semantics; no dedicated feature flag.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::Comp1Comp2,
        feature_flags: &COMP12_MAPPING,
        rationale: "COMP-1 and COMP-2 support are toggled by `comp_1` and `comp_2` for compatibility control.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::SignSeparate,
        feature_flags: &SIGN_SEPARATE_MAPPING,
        rationale: "SIGN SEPARATE clause support is controlled by `sign_separate`.",
    },
    GovernedFeatureBinding {
        support_id: FeatureId::NestedOdo,
        feature_flags: &[],
        rationale: "Nested ODO support status is a capability state, not yet exposed as a runtime toggle.",
    },
];

/// All explicit governance bindings.
#[inline]
#[must_use]
pub const fn governance_bindings() -> &'static [GovernedFeatureBinding] {
    &GOVERNANCE_BINDINGS
}

/// Look up the flag linkage for a support feature.
#[inline]
#[must_use]
pub fn feature_flags_for_support_id(support_id: FeatureId) -> Option<&'static [Feature]> {
    GOVERNANCE_BINDINGS
        .iter()
        .find(|entry| entry.support_id == support_id)
        .map(|entry| entry.feature_flags)
}

#[derive(Debug)]
pub struct GovernanceSummary {
    pub total_support_features: usize,
    pub mapped_support_features: usize,
    pub total_linked_feature_flags: usize,
}

impl GovernanceSummary {
    /// Whether every support feature has at least one governance row.
    #[inline]
    #[must_use]
    pub const fn all_features_known(&self) -> bool {
        self.total_support_features == self.mapped_support_features
    }

    /// Total explicit feature-flag bindings from the grid.
    #[inline]
    #[must_use]
    pub const fn explicit_bindings(&self) -> usize {
        self.total_linked_feature_flags
    }
}

/// Summarize governance mapping health for the current set of support and feature entries.
#[inline]
#[must_use]
pub fn summarize_governance() -> GovernanceSummary {
    let support_count = crate::support_matrix::all_features().len();
    let mut mapped_count = 0usize;
    let mut linked_flags = 0usize;

    for binding in &GOVERNANCE_BINDINGS {
        mapped_count += 1;
        linked_flags += binding.feature_flags.len();
    }

    GovernanceSummary {
        total_support_features: support_count,
        mapped_support_features: mapped_count,
        total_linked_feature_flags: linked_flags,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::support_matrix::all_features;

    #[test]
    fn test_governance_bindings_are_complete() {
        let support_ids = all_features()
            .iter()
            .map(|feature| feature.id)
            .collect::<Vec<_>>();
        let binding_ids = GOVERNANCE_BINDINGS
            .iter()
            .map(|b| b.support_id)
            .collect::<Vec<_>>();

        assert_eq!(
            support_ids.len(),
            binding_ids.len(),
            "all support entries should be represented"
        );

        for support_id in support_ids {
            assert!(
                binding_ids.contains(&support_id),
                "missing governance binding for support id {support_id:?}"
            );
        }
    }

    #[test]
    fn test_sign_separate_and_comp12_mapped_to_flags() {
        let sign_flags = feature_flags_for_support_id(FeatureId::SignSeparate)
            .expect("sign-separate should have mapping");
        assert!(sign_flags.contains(&Feature::SignSeparate));

        let comp12_flags = feature_flags_for_support_id(FeatureId::Comp1Comp2)
            .expect("comp-1-comp-2 should have mapping");
        assert_eq!(comp12_flags.len(), 2);
    }
}
