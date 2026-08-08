// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record framing facade.
//!
//! `copybook-codec` owns operation-level fixed-vs-RDW dispatch. This module
//! remains the stable public path for callers that used the earlier record
//! facade; the implementation lives in [`crate::file::dispatch`].

pub use crate::file::dispatch::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter,
    read_rdw_record, read_record, write_record,
};
