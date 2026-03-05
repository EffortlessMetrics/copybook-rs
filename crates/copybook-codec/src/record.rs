// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record framing facade.
//!
//! `copybook-codec` keeps this module as a stable compatibility surface while
//! delegating fixed-vs-RDW dispatch and framing primitives to the dedicated
//! `copybook-record-io` microcrate.

pub use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
