use crate::{RDW_HEADER_LEN, RdwHeader};
use copybook_error::Result;

/// An RDW record with header and payload bytes.
#[derive(Debug, Clone)]
pub struct RDWRecord {
    /// 4-byte RDW header (length + reserved).
    pub header: [u8; RDW_HEADER_LEN],
    /// Record payload bytes.
    pub payload: Vec<u8>,
}

impl RDWRecord {
    /// Create a new RDW record from payload (fallible constructor).
    ///
    /// Constructs an [`RDWRecord`] with a computed header (payload length + zero reserved bytes).
    ///
    /// # Examples
    ///
    /// ```
    /// use copybook_rdw::RDWRecord;
    ///
    /// let record = RDWRecord::try_new(vec![0xC8; 80]).unwrap();
    /// assert_eq!(record.length(), 80);
    /// assert_eq!(record.payload.len(), 80);
    /// ```
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_new(payload: Vec<u8>) -> Result<Self> {
        let header = RdwHeader::from_payload_len(payload.len(), 0)?.bytes();
        Ok(Self { header, payload })
    }

    /// Create a new RDW record from payload.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_new() instead for fallible construction"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    #[must_use]
    pub fn new(payload: Vec<u8>) -> Self {
        Self::try_new(payload).expect("RDW payload exceeds maximum size (65535 bytes)")
    }

    /// Create an RDW record preserving reserved bytes (fallible constructor).
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_with_reserved(payload: Vec<u8>, reserved: u16) -> Result<Self> {
        let header = RdwHeader::from_payload_len(payload.len(), reserved)?.bytes();
        Ok(Self { header, payload })
    }

    /// Create an RDW record preserving reserved bytes.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_with_reserved() instead for fallible construction"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    #[must_use]
    pub fn with_reserved(payload: Vec<u8>, reserved: u16) -> Self {
        Self::try_with_reserved(payload, reserved)
            .expect("RDW payload exceeds maximum size (65535 bytes)")
    }

    /// Get payload length from header.
    #[inline]
    #[must_use]
    pub fn length(&self) -> u16 {
        RdwHeader::from_bytes(self.header).length()
    }

    /// Get reserved bytes from header.
    #[inline]
    #[must_use]
    pub fn reserved(&self) -> u16 {
        RdwHeader::from_bytes(self.header).reserved()
    }

    /// Recompute the header length field from payload length.
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_recompute_length(&mut self) -> Result<()> {
        self.header = RdwHeader::from_payload_len(self.payload.len(), self.reserved())?.bytes();
        Ok(())
    }

    /// Recompute the header length field from payload length.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_recompute_length() instead for fallible operation"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    pub fn recompute_length(&mut self) {
        self.try_recompute_length()
            .expect("RDW payload exceeds maximum size (65535 bytes)");
    }

    /// Serialize record as `header + payload`.
    #[inline]
    #[must_use]
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(RDW_HEADER_LEN + self.payload.len());
        result.extend_from_slice(&self.header);
        result.extend_from_slice(&self.payload);
        result
    }
}
