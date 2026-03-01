// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for RDW (Record Descriptor Word) framing.
//!
//! Tests that header construction is reversible, that writer output is
//! readable, and that invalid headers never cause panics.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_rdw::{
    RDW_HEADER_LEN, RDW_MAX_PAYLOAD_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, RdwHeader,
    rdw_payload_len_to_u16,
};
use proptest::collection::vec;
use proptest::prelude::*;
use std::io::Cursor;

use super::config::DEFAULT_CASES;

// ---------------------------------------------------------------------------
// Header construction roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Any valid payload length → RDW header → parsed length == original.
    #[test]
    fn prop_header_length_roundtrip(
        payload_len in 0usize..=RDW_MAX_PAYLOAD_LEN,
        reserved in any::<u16>(),
    ) {
        let header = RdwHeader::from_payload_len(payload_len, reserved)
            .expect("valid payload length");
        prop_assert_eq!(header.length() as usize, payload_len);
        prop_assert_eq!(header.reserved(), reserved);
    }

    /// from_bytes(header.bytes()) is an identity.
    #[test]
    fn prop_header_bytes_roundtrip(
        payload_len in 0usize..=RDW_MAX_PAYLOAD_LEN,
        reserved in any::<u16>(),
    ) {
        let header = RdwHeader::from_payload_len(payload_len, reserved)
            .expect("valid payload length");
        let rebuilt = RdwHeader::from_bytes(header.bytes());
        prop_assert_eq!(rebuilt.length(), header.length());
        prop_assert_eq!(rebuilt.reserved(), header.reserved());
    }
}

// ---------------------------------------------------------------------------
// RDWRecord: header + payload → read → payload extracted == original
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Write a record with RDWRecordWriter then read it back with
    /// RDWRecordReader; payload must be identical.
    #[test]
    fn prop_writer_reader_roundtrip(
        payload in vec(any::<u8>(), 0..=4096),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None)
            .expect("bounded payload should encode");

        let mut reader = RDWRecordReader::new(Cursor::new(encoded), false);
        let record = reader
            .read_record()
            .expect("read should succeed")
            .expect("one record expected");

        prop_assert_eq!(record.payload.as_slice(), payload.as_slice());
        prop_assert!(reader.read_record().expect("EOF").is_none());
    }

    /// RDWRecord::try_new then as_bytes has correct header length.
    #[test]
    fn prop_rdw_record_as_bytes_length(
        payload in vec(any::<u8>(), 0..=2048),
    ) {
        let record = RDWRecord::try_new(payload.clone())
            .expect("bounded payload");
        let bytes = record.as_bytes();
        prop_assert_eq!(bytes.len(), RDW_HEADER_LEN + payload.len());
        let header = RdwHeader::from_bytes(bytes[..RDW_HEADER_LEN].try_into().unwrap());
        prop_assert_eq!(header.length() as usize, payload.len());
    }
}

// ---------------------------------------------------------------------------
// Multiple records roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Writing N records then reading them back yields the same payloads.
    #[test]
    fn prop_multi_record_roundtrip(
        payloads in vec(vec(any::<u8>(), 0..=512), 1..=8),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for payload in &payloads {
            writer.write_record_from_payload(payload, None)
                .expect("write");
        }

        let mut reader = RDWRecordReader::new(Cursor::new(encoded), false);
        for expected in &payloads {
            let record = reader
                .read_record()
                .expect("read")
                .expect("record expected");
            prop_assert_eq!(record.payload.as_slice(), expected.as_slice());
        }
        prop_assert!(reader.read_record().expect("EOF").is_none());
    }
}

// ---------------------------------------------------------------------------
// Invalid inputs never panic
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary 4-byte header never panics when parsed.
    #[test]
    fn prop_arbitrary_header_no_panic(bytes in any::<[u8; 4]>()) {
        let header = RdwHeader::from_bytes(bytes);
        let _ = header.length();
        let _ = header.reserved();
        let _ = header.looks_ascii_corrupt();
    }

    /// rdw_payload_len_to_u16 never panics for any usize.
    #[test]
    fn prop_payload_len_to_u16_never_panics(len in any::<usize>()) {
        let result = rdw_payload_len_to_u16(len);
        if len <= RDW_MAX_PAYLOAD_LEN {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap() as usize, len);
        } else {
            prop_assert!(result.is_err());
        }
    }

    /// Attempting to read from arbitrary bytes as RDW never panics (may error).
    #[test]
    fn prop_reader_arbitrary_bytes_no_panic(
        data in vec(any::<u8>(), 0..=128),
    ) {
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        // Read until EOF or error; must never panic.
        loop {
            match reader.read_record() {
                Ok(Some(_)) => continue,
                Ok(None) | Err(_) => break,
            }
        }
    }

    /// Payload length above u16::MAX always fails RDWRecord::try_new.
    #[test]
    fn prop_oversized_payload_rejected(
        extra in 1usize..=4096,
    ) {
        let len = RDW_MAX_PAYLOAD_LEN.saturating_add(extra);
        if len > RDW_MAX_PAYLOAD_LEN {
            let payload = vec![0u8; len];
            let result = RDWRecord::try_new(payload);
            prop_assert!(result.is_err());
        }
    }
}
