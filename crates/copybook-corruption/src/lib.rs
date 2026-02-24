// SPDX-License-Identifier: AGPL-3.0-or-later
//! Transfer-corruption detection façade.
//!
//! This crate keeps a narrow public surface: all corruption heuristics are
//! delegated to dedicated microcrates and re-exported here for stable call sites.

pub use copybook_corruption_detectors::{
    detect_ebcdic_corruption, detect_packed_corruption,
};
pub use copybook_corruption_rdw::detect_rdw_ascii_corruption;

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn facade_detects_rdw_ascii_corruption() {
        let rdw_with_ascii = [b'1', b'2', 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&rdw_with_ascii)
            .expect("expected ASCII corruption to be detected");
        assert_eq!(result.code, copybook_core::ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }
}
