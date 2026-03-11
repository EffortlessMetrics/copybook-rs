// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edited PIC (numeric editing) decode and encode support.
//!
//! This module is a compatibility façade that re-exports the dedicated
//! `copybook-edited-pic` microcrate.

pub use copybook_edited_pic::{
    NumericValue, PicToken, Sign, decode_edited_numeric, encode_edited_numeric, tokenize_edited_pic,
};
