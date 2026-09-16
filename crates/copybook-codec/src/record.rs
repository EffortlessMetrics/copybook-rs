// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record framing facade.
//!
//! `copybook-codec` owns operation-level fixed-vs-RDW dispatch. This module
//! remains the stable public path for callers that used the earlier record
//! facade; the implementation lives in [`crate::file::dispatch`].

pub use crate::file::dispatch::{
    BDW_HEADER_LEN, BDW_MAX_BLOCK_LEN, BdwHeader, FixedRecordReader, FixedRecordWriter,
    RDW_HEADER_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, VB_MAX_RECORD_LEN, VbBlockReader,
    VbBlockWriter, VbRecord, read_rdw_record, read_record, write_record,
};
