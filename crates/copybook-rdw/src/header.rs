use copybook_error::{Error, ErrorCode, Result};

use crate::RDW_HEADER_LEN;

/// Parsed RDW header (`length + reserved`).
///
/// A 4-byte Record Descriptor Word containing a 2-byte big-endian payload
/// length and 2 reserved bytes.
///
/// # Examples
///
/// ```
/// use copybook_rdw::RdwHeader;
///
/// let header = RdwHeader::from_bytes([0x00, 0x50, 0x00, 0x00]);
/// assert_eq!(header.length(), 80);
/// assert_eq!(header.reserved(), 0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RdwHeader {
    bytes: [u8; RDW_HEADER_LEN],
}

impl RdwHeader {
    /// Construct from raw 4-byte header bytes.
    #[must_use]
    #[inline]
    pub const fn from_bytes(bytes: [u8; RDW_HEADER_LEN]) -> Self {
        Self { bytes }
    }

    /// Construct from payload length and reserved bytes.
    ///
    /// # Errors
    /// Returns `CBKF102_RECORD_LENGTH_INVALID` when `payload_len > u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn from_payload_len(payload_len: usize, reserved: u16) -> Result<Self> {
        let len = rdw_payload_len_to_u16(payload_len)?;
        let len_bytes = len.to_be_bytes();
        let reserved_bytes = reserved.to_be_bytes();
        Ok(Self {
            bytes: [
                len_bytes[0],
                len_bytes[1],
                reserved_bytes[0],
                reserved_bytes[1],
            ],
        })
    }

    /// Return raw bytes.
    #[must_use]
    #[inline]
    pub const fn bytes(self) -> [u8; RDW_HEADER_LEN] {
        self.bytes
    }

    /// Extract payload length.
    #[must_use]
    #[inline]
    pub const fn length(self) -> u16 {
        u16::from_be_bytes([self.bytes[0], self.bytes[1]])
    }

    /// Extract reserved bytes.
    #[must_use]
    #[inline]
    pub const fn reserved(self) -> u16 {
        u16::from_be_bytes([self.bytes[2], self.bytes[3]])
    }

    /// ASCII-corruption heuristic for the length bytes.
    ///
    /// Returns `true` when both length bytes look like ASCII digits.
    #[must_use]
    #[inline]
    pub const fn looks_ascii_corrupt(self) -> bool {
        rdw_is_suspect_ascii_corruption(self.bytes)
    }
}

/// Convert payload length to `u16` with RDW error mapping.
///
/// # Errors
/// Returns `CBKF102_RECORD_LENGTH_INVALID` when `len > u16::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_payload_len_to_u16(len: usize) -> Result<u16> {
    u16::try_from(len).map_err(|_| {
        Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "RDW payload too large: {} bytes exceeds maximum of {}",
                len,
                u16::MAX
            ),
        )
    })
}

/// Heuristic to detect ASCII-corrupted RDW headers.
///
/// Returns `true` when both RDW length bytes are ASCII digits (`0x30..=0x39`).
#[must_use]
#[inline]
pub const fn rdw_is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
    copybook_rdw_predicates::rdw_is_suspect_ascii_corruption(rdw_header)
}
