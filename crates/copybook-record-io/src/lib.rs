#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility forwarding surface for the retired record-dispatch package.
//!
//! Operation-level dispatch is owned by [`copybook_codec::file::dispatch`].
//! This 0.5 package remains resolvable for existing users, but contains no
//! independent routing or framing implementation.

pub use copybook_codec::file::dispatch::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter,
    read_rdw_record, read_record, write_record,
};
