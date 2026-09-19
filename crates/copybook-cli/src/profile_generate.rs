// SPDX-License-Identifier: AGPL-3.0-or-later
//! Draft an interpretation profile from a healthy diagnosis.
//!
//! `doctor --emit-profile` turns established diagnosis evidence into a
//! profile TOML the operator reviews. Every key is labeled `PINNED`
//! (explicitly flagged or confirmed by the probes) or `REVIEW` (a default
//! or leading candidate the operator must decide on); the file is a draft
//! until no `REVIEW` line remains. Assembly refuses (`None`) when framing
//! or codepage were never established, which only happens alongside
//! failing findings, so callers emit nothing and keep the diagnosis exit.

use crate::cli_config::DialectPreference;
use copybook::codec::diagnose::DiagnosisEvidence;
#[cfg(test)]
use copybook::codec::options::profile::ReservedPolicy;
use copybook::codec::options::profile::{
    DEFAULT_PROFILE_ERRORS, FramingKind, InterpretationProfile, MAX_PROFILE_RECORD_LENGTH,
    SourceDialect,
};
use copybook::codec::{Codepage, RecordFormat};
use copybook::core::dialect::Dialect;

/// A drafted profile plus its per-key provenance notes.
pub(crate) struct DraftedProfile {
    /// The profile value.
    pub profile: InterpretationProfile,
    /// `PINNED`/`REVIEW` provenance notes, one per decided key.
    pub notes: Vec<String>,
    /// Whether any note still needs review.
    pub needs_review: bool,
}

/// Draft a profile from diagnosis evidence and the explicit dialect flag.
///
/// Returns `None` when framing or codepage were never established; callers
/// treat that as "do not emit" and keep the diagnosis exit code.
pub(crate) fn assemble(
    evidence: &DiagnosisEvidence,
    dialect_flag: Option<DialectPreference>,
) -> Option<DraftedProfile> {
    let format = evidence.format?;
    let codepage = evidence.codepage?;
    let mut drafter = Drafter {
        evidence,
        dialect_flag,
        notes: Vec::new(),
        needs_review: false,
    };
    drafter.note_framing(format);
    drafter.note_codepage(codepage);
    let dialect = drafter.resolve_dialect();
    drafter.note_reserved();
    let maximum_record_length = drafter.resolve_record_length();
    drafter.note_defaults();
    // Struct literals are not part of the profile contract (the structs are
    // #[non_exhaustive]): draft from the product defaults and overwrite every
    // decided field, so additive profile fields keep compiling here.
    let mut profile = InterpretationProfile::product_defaults();
    profile.source.dialect = SourceDialect::from(dialect);
    profile.framing.kind = FramingKind::from(format);
    profile.decode.codepage = codepage;
    profile.limits.maximum_record_length = maximum_record_length;
    Some(DraftedProfile {
        profile,
        notes: drafter.notes,
        needs_review: drafter.needs_review,
    })
}

/// One draft in progress: evidence plus the provenance notes so far.
struct Drafter<'a> {
    evidence: &'a DiagnosisEvidence,
    dialect_flag: Option<DialectPreference>,
    notes: Vec<String>,
    needs_review: bool,
}

impl Drafter<'_> {
    /// Record a decided key with no remaining review.
    fn pin(&mut self, note: &str) {
        self.notes.push(format!("PINNED {note}"));
    }

    /// Record a key the operator must still decide on.
    fn review(&mut self, note: &str) {
        self.needs_review = true;
        self.notes.push(format!("REVIEW {note}"));
    }

    fn note_framing(&mut self, format: RecordFormat) {
        if self.evidence.format_explicit {
            self.pin(&format!("framing.kind={format} (explicit --format flag)"));
        } else {
            self.pin(&format!("framing.kind={format} (single probe fit)"));
        }
    }

    fn note_codepage(&mut self, codepage: Codepage) {
        if self.evidence.codepage_pinned {
            if self.evidence.codepage_explicit {
                self.pin(&format!(
                    "decode.codepage={codepage} (explicit --codepage flag)"
                ));
            } else {
                self.pin(&format!(
                    "decode.codepage={codepage} (confident probe winner, trial-corroborated)"
                ));
            }
        } else {
            self.review(&format!(
                "decode.codepage={codepage} is only the leading candidate; rerun doctor --codepage {codepage} to pin it"
            ));
        }
    }

    fn resolve_dialect(&mut self) -> Dialect {
        if let Some(flag) = self.dialect_flag {
            let dialect = Dialect::from(flag);
            self.pin(&format!(
                "source.dialect={} (explicit --dialect flag)",
                SourceDialect::from(dialect)
            ));
            return dialect;
        }
        if self.evidence.variable_layout {
            self.review(
                "source.dialect=normative is a default; variable-length layout means ODO min_count interpretation may matter (n, 0, 1)",
            );
        } else {
            self.pin("source.dialect=normative (default; inert for fixed layouts)");
        }
        Dialect::Normative
    }

    fn note_reserved(&mut self) {
        if self.evidence.reserved_nonzero_observed {
            self.review(
                "framing.reserved_bytes=lenient despite non-zero reserved bytes in the probe; switch to strict only if the feed guarantees zero reserved bytes",
            );
        } else {
            self.pin("framing.reserved_bytes=lenient (no non-zero reserved bytes in the probe)");
        }
    }

    fn resolve_record_length(&mut self) -> u64 {
        match self.evidence.record_length {
            Some(established) if established <= MAX_PROFILE_RECORD_LENGTH => {
                if self.evidence.record_length_exact {
                    self.pin(&format!(
                        "limits.maximum_record_length={established} (exact fixed LRECL)"
                    ));
                } else {
                    self.pin(&format!(
                        "limits.maximum_record_length={established} (largest observed RDW wire record)"
                    ));
                }
                established
            }
            Some(oversize) => {
                self.review(&format!(
                    "limits.maximum_record_length capped at {MAX_PROFILE_RECORD_LENGTH}: observed {oversize} exceeds the profile bound"
                ));
                MAX_PROFILE_RECORD_LENGTH
            }
            None => {
                self.review(&format!(
                    "limits.maximum_record_length={MAX_PROFILE_RECORD_LENGTH} is uncapped: no record length established (VB framing or no trial); set the feed's cap"
                ));
                MAX_PROFILE_RECORD_LENGTH
            }
        }
    }

    fn note_defaults(&mut self) {
        self.pin(&format!(
            "limits.maximum_errors={DEFAULT_PROFILE_ERRORS} (default budget; tune to the feed)"
        ));
        self.pin("decode.unmappable=error (default; no probe distinguishes policies)");
        self.pin("decode.json_numbers=lossless (default; decode-only)");
    }
}

/// Render a drafted profile: a provenance header plus canonical TOML.
///
/// The header is comments, so the file stays a valid profile input while
/// telling the reviewer exactly what is decided and what is not.
pub(crate) fn render(drafted: &DraftedProfile) -> String {
    let mut rendered = String::from(
        "# Drafted by `copybook doctor --emit-profile`.\n\
         # Keys marked REVIEW need an operator decision before this file\n\
         # counts as reviewed intent; keys marked PINNED were explicitly\n\
         # flagged or confirmed by the diagnosis.\n",
    );
    for note in &drafted.notes {
        rendered.push_str("# ");
        rendered.push_str(note);
        rendered.push('\n');
    }
    match drafted.profile.to_canonical_toml() {
        Ok(toml) => rendered.push_str(&toml),
        Err(error) => {
            use std::fmt::Write as _;
            let _ = writeln!(rendered, "# ERROR rendering profile TOML: {error}");
        }
    }
    if !rendered.ends_with('\n') {
        rendered.push('\n');
    }
    rendered
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook::codec::{Codepage, RecordFormat};

    fn healthy_fixed() -> DiagnosisEvidence {
        DiagnosisEvidence {
            format: Some(RecordFormat::Fixed),
            format_explicit: true,
            codepage: Some(Codepage::CP037),
            codepage_pinned: true,
            codepage_explicit: true,
            record_length: Some(50),
            record_length_exact: true,
            reserved_nonzero_observed: false,
            variable_layout: false,
            trial_succeeded: true,
            trial_records: 1,
        }
    }

    #[test]
    fn fully_pinned_evidence_needs_no_review() {
        let drafted = assemble(&healthy_fixed(), None).expect("established evidence assembles");
        assert!(!drafted.needs_review);
        assert_eq!(drafted.profile.framing.kind, FramingKind::Fixed);
        assert_eq!(drafted.profile.decode.codepage, Codepage::CP037);
        assert_eq!(drafted.profile.limits.maximum_record_length, 50);
        let rendered = render(&drafted);
        assert!(rendered.contains("PINNED framing.kind=fixed"));
        // The rendered file parses back to the same profile.
        let parsed = InterpretationProfile::parse(
            rendered
                .lines()
                .filter(|line| !line.starts_with('#'))
                .collect::<Vec<_>>()
                .join("\n")
                .as_str(),
        )
        .expect("rendered profile parses");
        assert_eq!(parsed, drafted.profile);
    }

    #[test]
    fn leading_codepage_marks_review() {
        let mut evidence = healthy_fixed();
        evidence.codepage_pinned = false;
        evidence.codepage_explicit = false;
        evidence.trial_succeeded = false;
        evidence.trial_records = 0;
        let drafted = assemble(&evidence, None).expect("leading codepage still assembles");
        assert!(drafted.needs_review);
        assert!(
            drafted
                .notes
                .iter()
                .any(|note| note.starts_with("REVIEW decode.codepage"))
        );
    }

    #[test]
    fn variable_layout_marks_dialect_review() {
        let mut evidence = healthy_fixed();
        evidence.variable_layout = true;
        let drafted = assemble(&evidence, None).expect("variable layout assembles");
        assert!(drafted.needs_review);
        assert!(
            drafted
                .notes
                .iter()
                .any(|note| note.starts_with("REVIEW source.dialect"))
        );
    }

    #[test]
    fn explicit_dialect_pins_despite_variable_layout() {
        let mut evidence = healthy_fixed();
        evidence.variable_layout = true;
        let drafted =
            assemble(&evidence, Some(DialectPreference::Zero)).expect("explicit dialect assembles");
        assert!(
            drafted
                .notes
                .iter()
                .any(|note| note.starts_with("PINNED source.dialect=zero-tolerant"))
        );
        assert_eq!(drafted.profile.source.dialect, SourceDialect::ZeroTolerant);
    }

    #[test]
    fn nonzero_reserved_marks_review() {
        let mut evidence = healthy_fixed();
        evidence.reserved_nonzero_observed = true;
        let drafted = assemble(&evidence, None).expect("reserved observation assembles");
        assert!(drafted.needs_review);
        assert!(
            drafted
                .notes
                .iter()
                .any(|note| note.starts_with("REVIEW framing.reserved_bytes"))
        );
        assert_eq!(
            drafted.profile.framing.reserved_bytes,
            ReservedPolicy::Lenient
        );
    }

    #[test]
    fn unestablished_framing_refuses() {
        let mut evidence = healthy_fixed();
        evidence.format = None;
        assert!(assemble(&evidence, None).is_none());
    }

    #[test]
    fn oversize_record_length_clamps_with_review() {
        let mut evidence = healthy_fixed();
        evidence.record_length = Some(MAX_PROFILE_RECORD_LENGTH + 1);
        let drafted = assemble(&evidence, None).expect("oversize clamps");
        assert!(drafted.needs_review);
        assert_eq!(
            drafted.profile.limits.maximum_record_length,
            MAX_PROFILE_RECORD_LENGTH
        );
    }
}
