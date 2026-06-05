// SPDX-License-Identifier: AGPL-3.0-or-later
//! Metrics integration for record processing.

#[cfg(feature = "metrics")]
mod imp {
    use crate::options::{Codepage, DecodeOptions, RecordFormat, ZonedEncodingFormat};
    use metrics::{counter, gauge, histogram};

    #[inline]
    pub fn record_read(bytes: usize, options: &DecodeOptions) {
        let format_label = format_label(options.format);
        let codepage_label = codepage_label(options.codepage);
        let zero_policy_label = zero_policy_label(options);

        counter!(
            "copybook_records_total",
            "format" => format_label,
            "codepage" => codepage_label,
            "zero_policy" => zero_policy_label
        )
        .increment(1);
        counter!(
            "copybook_bytes_total",
            "format" => format_label,
            "codepage" => codepage_label,
            "zero_policy" => zero_policy_label
        )
        .increment(bytes as u64);
    }

    #[inline]
    pub fn record_error(family: &'static str) {
        counter!("copybook_decode_errors_total", "family" => family).increment(1);
    }

    #[inline]
    pub fn record_completion(
        duration_seconds: f64,
        throughput_mibps: f64,
        options: &DecodeOptions,
    ) {
        let format_label = format_label(options.format);
        let codepage_label = codepage_label(options.codepage);

        if duration_seconds.is_finite() && duration_seconds >= 0.0 {
            histogram!(
                "copybook_decode_seconds",
                "format" => format_label,
                "codepage" => codepage_label
            )
            .record(duration_seconds);
        }

        if throughput_mibps.is_finite() {
            gauge!(
                "copybook_throughput_mibps",
                "format" => format_label,
                "codepage" => codepage_label
            )
            .set(throughput_mibps);
        }
    }

    #[inline]
    fn zero_policy_label(options: &DecodeOptions) -> &'static str {
        if options.preserve_zoned_encoding {
            "preserved"
        } else if options.preferred_zoned_encoding != ZonedEncodingFormat::Auto {
            "override"
        } else {
            "preferred"
        }
    }

    #[inline]
    fn format_label(format: RecordFormat) -> &'static str {
        match format {
            RecordFormat::Fixed => "fixed",
            RecordFormat::RDW => "rdw",
        }
    }

    #[inline]
    fn codepage_label(codepage: Codepage) -> &'static str {
        match codepage {
            Codepage::ASCII => "ascii",
            Codepage::CP037 => "cp037",
            Codepage::CP273 => "cp273",
            Codepage::CP500 => "cp500",
            Codepage::CP1047 => "cp1047",
            Codepage::CP1140 => "cp1140",
        }
    }
}

#[cfg(not(feature = "metrics"))]
mod imp {
    use crate::options::DecodeOptions;

    #[inline]
    pub fn record_read(_bytes: usize, _options: &DecodeOptions) {}

    #[inline]
    pub fn record_error(_family: &'static str) {}

    #[inline]
    pub fn record_completion(
        _duration_seconds: f64,
        _throughput_mibps: f64,
        _options: &DecodeOptions,
    ) {
    }
}

pub use imp::{record_completion, record_error, record_read};
