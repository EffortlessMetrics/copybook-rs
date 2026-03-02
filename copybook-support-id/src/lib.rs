// SPDX-License-Identifier: AGPL-3.0-or-later
//! Canonical support-matrix feature identifiers.

use core::str::FromStr;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum FeatureId {
    #[serde(rename = "level-88")]
    Level88Conditions,
    #[serde(rename = "level-66-renames")]
    Level66Renames,
    #[serde(rename = "occurs-depending")]
    OccursDepending,
    #[serde(rename = "edited-pic")]
    EditedPic,
    #[serde(rename = "comp-1-comp-2")]
    Comp1Comp2,
    #[serde(rename = "sign-separate")]
    SignSeparate,
    #[serde(rename = "nested-odo")]
    NestedOdo,
}

impl FeatureId {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Level88Conditions => "level-88",
            Self::Level66Renames => "level-66-renames",
            Self::OccursDepending => "occurs-depending",
            Self::EditedPic => "edited-pic",
            Self::Comp1Comp2 => "comp-1-comp-2",
            Self::SignSeparate => "sign-separate",
            Self::NestedOdo => "nested-odo",
        }
    }
}

impl FromStr for FeatureId {
    type Err = serde_plain::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_plain::from_str(s)
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn as_str_matches_serde_encoding() {
        for id in [
            FeatureId::Level88Conditions,
            FeatureId::Level66Renames,
            FeatureId::OccursDepending,
            FeatureId::EditedPic,
            FeatureId::Comp1Comp2,
            FeatureId::SignSeparate,
            FeatureId::NestedOdo,
        ] {
            let encoded = serde_plain::to_string(&id).expect("must encode");
            assert_eq!(encoded, id.as_str());
            assert_eq!(FeatureId::from_str(id.as_str()).ok(), Some(id));
        }
    }
}
