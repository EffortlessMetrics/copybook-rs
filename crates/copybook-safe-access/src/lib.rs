// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe access traits for `Option`, `Vec`, and slices.

use copybook_error::{Error, ErrorCode};

/// Result type alias using `copybook-error`'s Error.
pub type Result<T> = std::result::Result<T, Error>;

/// Extension trait for `Option<T>` providing panic-safe unwrapping with context.
pub trait OptionExt<T> {
    /// Unwrap an option safely, returning a structured error with context if None.
    ///
    /// # Errors
    /// Returns the specified error code and message if the option is `None`.
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T>;

    /// Unwrap an option safely with a specific error context.
    ///
    /// # Errors
    /// Returns the provided error if the option is `None`.
    fn ok_or_error(self, error: Error) -> Result<T>;
}

impl<T> OptionExt<T> for Option<T> {
    #[inline]
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        match self {
            Some(value) => Ok(value),
            None => Err(Error::new(code, message.into())),
        }
    }

    #[inline]
    fn ok_or_error(self, error: Error) -> Result<T> {
        match self {
            Some(value) => Ok(value),
            None => Err(error),
        }
    }
}

/// Extension trait for `Vec<T>` providing panic-safe access operations.
pub trait VecExt<T> {
    /// Pop from vector safely, returning a structured error if empty.
    ///
    /// # Errors
    /// Returns the specified error code and message if the vector is empty.
    fn pop_or_cbkp_error(&mut self, code: ErrorCode, message: impl Into<String>) -> Result<T>;

    /// Get last element safely, returning a structured error if empty.
    ///
    /// # Errors
    /// Returns the specified error code and message if the vector is empty.
    fn last_or_cbkp_error(&self, code: ErrorCode, message: impl Into<String>) -> Result<&T>;

    /// Get last mutable element safely, returning a structured error if empty.
    ///
    /// # Errors
    /// Returns the specified error code and message if the vector is empty.
    fn last_mut_or_cbkp_error(
        &mut self,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T>;
}

impl<T> VecExt<T> for Vec<T> {
    #[inline]
    fn pop_or_cbkp_error(&mut self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        match self.pop() {
            Some(value) => Ok(value),
            None => Err(Error::new(code, message.into())),
        }
    }

    #[inline]
    fn last_or_cbkp_error(&self, code: ErrorCode, message: impl Into<String>) -> Result<&T> {
        if !self.is_empty() {
            Ok(&self[self.len() - 1])
        } else {
            Err(Error::new(code, message.into()))
        }
    }

    #[inline]
    fn last_mut_or_cbkp_error(
        &mut self,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        if !self.is_empty() {
            let len = self.len();
            Ok(&mut self[len - 1])
        } else {
            Err(Error::new(code, message.into()))
        }
    }
}

/// Extension trait for slice indexing providing panic-safe access.
pub trait SliceExt<T> {
    /// Get element at index safely, returning a structured error if out of bounds.
    ///
    /// # Errors
    /// Returns the specified error code and message if the index is out of bounds.
    fn get_or_cbkp_error(
        &self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&T>;

    /// Get mutable element at index safely, returning a structured error if out of bounds.
    ///
    /// # Errors
    /// Returns the specified error code and message if the index is out of bounds.
    fn get_mut_or_cbkp_error(
        &mut self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T>;
}

impl<T> SliceExt<T> for [T] {
    #[inline]
    fn get_or_cbkp_error(
        &self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&T> {
        if index < self.len() {
            Ok(&self[index])
        } else {
            Err(Error::new(code, message.into()))
        }
    }

    #[inline]
    fn get_mut_or_cbkp_error(
        &mut self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        if index < self.len() {
            Ok(&mut self[index])
        } else {
            Err(Error::new(code, message.into()))
        }
    }
}
