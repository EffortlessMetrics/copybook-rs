#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_rdw::{
    RDW_HEADER_LEN, RdwHeader, rdw_payload_len_to_u16, rdw_read_len, rdw_slice_body,
    rdw_try_peek_len, rdw_validate_and_finish,
};
use libfuzzer_sys::fuzz_target;
use std::io::{BufRead, Cursor};

fuzz_target!(|data: &[u8]| {
    let _ = rdw_payload_len_to_u16(data.len());

    if data.len() >= RDW_HEADER_LEN {
        let mut bytes = [0u8; RDW_HEADER_LEN];
        bytes.copy_from_slice(&data[..RDW_HEADER_LEN]);
        let header = RdwHeader::from_bytes(bytes);
        let _ = header.length();
        let _ = header.reserved();
        let _ = header.looks_ascii_corrupt();
        let _ = RdwHeader::from_payload_len(header.length() as usize, header.reserved());
    }

    let mut peek_cursor = Cursor::new(data);
    if rdw_try_peek_len(&mut peek_cursor).ok().flatten().is_some() {
        let mut read_cursor = Cursor::new(data);
        if let Ok(len) = rdw_read_len(&mut read_cursor) {
            read_cursor.consume(2);
            if let Ok(body) = rdw_slice_body(&mut read_cursor, len) {
                let _ = rdw_validate_and_finish(body);
            }
        }
    }
});
