// SPDX-License-Identifier: AGPL-3.0-or-later
//! Arrow IPC (Feather v2) file writer

use arrow::array::RecordBatch;
use arrow::datatypes::Schema as ArrowSchema;
use arrow::ipc::writer::FileWriter;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::Arc;

use crate::{ArrowError, Result};

/// Write Arrow `RecordBatch` objects to an Arrow IPC file.
///
/// # Errors
///
/// Returns an error if file creation or writing fails.
#[inline]
pub fn write_ipc<P: AsRef<Path>>(
    path: P,
    batches: &[RecordBatch],
    schema: &ArrowSchema,
) -> Result<()> {
    let file = File::create(path)?;
    let buf_writer = BufWriter::new(file);

    let mut writer = FileWriter::try_new(buf_writer, &Arc::new(schema.clone()))
        .map_err(|e| ArrowError::IpcWrite(format!("Failed to create IPC writer: {e}")))?;

    for batch in batches {
        writer
            .write(batch)
            .map_err(|e| ArrowError::IpcWrite(format!("Failed to write batch: {e}")))?;
    }

    writer
        .finish()
        .map_err(|e| ArrowError::IpcWrite(format!("Failed to finish IPC writer: {e}")))?;

    Ok(())
}
