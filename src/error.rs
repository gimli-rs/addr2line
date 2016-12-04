//! Holds custom error types and `Result` wrapper for addr2line

use std::io;

#[derive(Debug)]
/// Set of possible errors from constructing and using a `Mapping`.
pub enum MappingError {
    /// The given executable path could not be opened.
    BadPath(io::Error),
    /// The given executable did not contain sufficient information to enable address translation.
    MissingDebugInfo(&'static str),
}

/// Wrapper for `Result` whose error is a `MappingError`.
pub type MappingResult<T> = Result<T, MappingError>;
