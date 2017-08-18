//! This crate provides a cross-platform library and binary for translating addresses into file
//! names and line numbers. Given an address in an executable or an offset in a section of a
//! relocatable object, it uses the debugging information to figure out which file name and line
//! number are associated with it.
//!
//! When used as a library, executables are loaded using `Mapping::new`, and users can the use
//! `Mapping::locate` to find the corresponding file path and line number. If the `Mapping` was
//! created with `with_functions`, `Mapping::locate` may also return information about the function
//! containing the given address. `addr2line` avoids re-executing work where it can so that looking
//! up multiple addresses for a single file does not require re-parsing the executable's debug
//! symbols. The library makes heavy use of [gimli](https://github.com/gimli-rs/gimli), which
//! provides zero-copy access to the DWARF debugging format used by most modern compilers.
//!
//! The initial implementation of the library is heavily influenced by the original `addr2line`
//! example in gimli, which was removed in
//! [5da3e19dd4cfae30b82053868ed7ab3fdd3cf026](https://github.com/gimli-rs/gimli/commit/5da3e19dd4cfae30b82053868ed7ab3fdd3cf026) in favor of this repository.
//!
//! This crate also provides a thing CLI wrapper around the library which provides some of the
//! functionality of the `addr2line` command line tool distributed with [GNU
//! binutils](https://www.gnu.org/software/binutils/). The executable or relocatable object to use
//! is specified with the -e option. The default is the file a.out.
#![deny(missing_docs)]

extern crate gimli;
extern crate memmap;
extern crate object;
extern crate owning_ref;
extern crate fallible_iterator;

#[cfg(feature = "rustc-demangle")]
extern crate rustc_demangle;

#[cfg(feature = "cpp_demangle")]
extern crate cpp_demangle;

#[macro_use]
extern crate error_chain;

use owning_ref::OwningHandle;
use fallible_iterator::FallibleIterator;

use std::fmt;
use std::path;
use std::error;
use std::borrow::Cow;

/// An error occured while traversing the debug symbols in the provided executable.
#[derive(Debug)]
pub enum DebugInfoError {
    /// DebugLine refers to a file that does not exist
    InvalidDebugLineTarget,
    /// A unit was completely empty (i.e., did not contain a compilation unit)
    MissingComplilationUnit,
    /// The first entry in a unit is not a compilation unit
    UnitWithoutCompilationUnit,
    /// Entry offset points to empty entry
    DanglingEntryOffset,
    /// A range was inverted (high > low)
    RangeInverted,
}

impl fmt::Display for DebugInfoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DebugInfoError::InvalidDebugLineTarget => {
                write!(f, "DebugLine referst to a file that does not exist")
            }
            DebugInfoError::MissingComplilationUnit => write!(
                f,
                "A unit was completely empty (i.e., did not contain a compilation unit)"
            ),
            DebugInfoError::UnitWithoutCompilationUnit => {
                write!(f, "The first entry in a unit is not a compilation unit")
            }
            DebugInfoError::DanglingEntryOffset => write!(f, "Entry offset points to empty entry"),
            DebugInfoError::RangeInverted => write!(f, "A range was inverted (high > low)"),
        }
    }
}

impl error::Error for DebugInfoError {
    fn description(&self) -> &str {
        "An error occured while traversing debug symbols"
    }
}

// The `error_chain!` macro spits about a bunch of these warnings on nightly.
#[allow(unknown_lints)]
#[allow(unused_doc_comment)]
mod errors {
    use gimli;
    use std::io;
    use super::DebugInfoError;

    error_chain! {
        foreign_links {
            Gimli(gimli::Error)
            /// An error given by the `gimli` library while parsing the DWARF
            /// debug symbols.
                ;

            BadPath(io::Error)
            /// The path given could not be used to extract debug symbols.
                ;

            InvalidDebugSymbols(DebugInfoError)
            /// An error occured while traversing the debug symbols in the
            /// provided executable.
                ;
        }

        errors {
            /// A necessary debug section is missing from the provided executable.
            MissingDebugSection(s: &'static str) {
                description("missing debug section")
                display("missing debug section: '.{}'", s)
            }
        }
    }
}
pub use errors::*;

/// A builder for configuring a `Mapping` or `BufferMapping`.
///
/// ```
/// # fn foo() -> addr2line::Result<()> {
/// use addr2line::Options;
///
/// let mapping = Options::default()
///     .with_functions()
///     .build("path/to/some/executable")?;
/// # let _ = mapping;
/// # Ok(())
/// # }
/// ```
///
/// If `with_functions` is not added to these options, then `Mapping::locate()`
/// will always return `None` for the function name.
///
/// Likewise, if `with_demangling` is not added to these options, then all
/// function names (if any) returned by `Mapping::locate()` will not be
/// demangled.
#[derive(Clone, Copy, Default)]
pub struct Options {
    with_functions: bool,
    with_demangling: bool,
}

impl Options {
    /// Make the `Mapping` also include debug information for functions,
    /// enabling `Mapping::locate()` to also indicate what function a given
    /// address appears in. This comes at some parsing and lookup cost.
    pub fn with_functions(mut self) -> Self {
        self.with_functions = true;
        self
    }

    /// Make the `Mapping` attempt to demangle Rust and/or C++ symbols. This
    /// option implies `with_functions`.
    #[cfg(any(feature = "cpp_demangle", feature = "rustc-demangle"))]
    pub fn with_demangling(mut self) -> Self {
        self.with_demangling = true;
        self.with_functions()
    }

    /// Finish configuration and build a `Mapping`.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out
    /// the necessary debug symbols, without copying data when possible.
    pub fn build<P>(self, file_path: P) -> Result<Mapping>
    where
        P: AsRef<path::Path>,
    {
        Mapping::new_inner(file_path.as_ref(), self)
    }

    /// Finish configuration and build a `BufferMapping`.
    pub fn build_from_buffer<'input>(self, buffer: &'input [u8]) -> Result<BufferMapping<'input>> {
        BufferMapping::new_inner(buffer, self)
    }
}

/// A `Mapping` locates and maintains the state necessary to perform address to line translation.
///
/// This mapping manages reading the data from the file, and ensures this data is valid as long
/// as the mapping exists.
///
/// Constructing a `Mapping` is somewhat costly, so users should aim to re-use created `Mapping`s
/// when performing lookups for many addresses over the same executable.
pub struct Mapping {
    // we use `OwningHandle` so that we can store both the Mmap and the parsed debug info.
    // the lifetime of EndianDebugInfo here isn't *technically* static, it's *our* lifetime,
    // but there isn't a good way to express that as far as I am aware? suggestions welcome.
    inner: OwningHandle<Box<memmap::Mmap>, Box<EndianDebugInfo<'static>>>,
}

/// A `BufferMapping` locates and maintains the state necessary to perform address to line
/// translation for a given buffer containing the executable file data.
///
/// This mapping requires the caller to read the data from the file and ensure the lifetime
/// of the buffer encompasses the lifetime of the mapping.
///
/// Constructing a `BufferMapping` is somewhat costly, so users should aim to re-use created
/// `BufferMapping`s when performing lookups for many addresses over the same executable.
pub struct BufferMapping<'input>(EndianDebugInfo<'input>);

enum EndianDebugInfo<'input> {
    LEInfo(DebugInfo<'input, gimli::LittleEndian>),
    BEInfo(DebugInfo<'input, gimli::BigEndian>),
}

/// `DebugInfo` holds the debug information derived from an input buffer.
struct DebugInfo<'input, Endian>
where
    Endian: gimli::Endianity,
{
    units: Vec<Unit<'input, Endian>>,
    opts: Options,
}

impl Mapping {
    /// Construct a new `Mapping` with the default `Options`.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out the necessary debug
    /// symbols, without copying data when possible.
    pub fn new<P>(file_path: P) -> Result<Mapping>
    where
        P: AsRef<path::Path>,
    {
        Options::default().build(file_path)
    }

    fn new_inner(file_path: &path::Path, opts: Options) -> Result<Mapping> {
        let file = memmap::Mmap::open_path(file_path, memmap::Protection::Read)
            .map_err(|e| ErrorKind::BadPath(e))?;

        OwningHandle::try_new(Box::new(file), |mmap| -> Result<_> {
            let mmap: &memmap::Mmap = unsafe { &*mmap };
            let bytes = unsafe { mmap.as_slice() };
            EndianDebugInfo::new(bytes, opts)
                .chain_err(|| "failed to analyze debug information")
                .map(|di| Box::new(di))
        }).map(|di| Mapping { inner: di })
    }

    /// Locate the source file and line corresponding to the given virtual memory address.
    ///
    /// If the `Mapping` was constructed with `with_functions`, information about the containing
    /// function may also be returned when available.
    pub fn locate(
        &mut self,
        addr: u64,
    ) -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<str>>)>> {
        self.inner.locate(addr)
    }
}

impl<'input> BufferMapping<'input> {
    /// Construct a new `BufferMapping` with the default `Options`.
    pub fn new(bytes: &'input [u8]) -> Result<BufferMapping<'input>> {
        Self::new_inner(bytes, Options::default())
    }

    fn new_inner(bytes: &'input [u8], opts: Options) -> Result<BufferMapping<'input>> {
        Ok(BufferMapping(EndianDebugInfo::new(bytes, opts)?))
    }

    /// Locate the source file and line corresponding to the given virtual memory address.
    ///
    /// If the `BufferMapping` was constructed with `with_functions`, information about the
    /// containing function may also be returned when available.
    pub fn locate(
        &mut self,
        addr: u64,
    ) -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<'input, str>>)>> {
        self.0.locate(addr)
    }
}

impl<'input> EndianDebugInfo<'input> {
    fn new(bytes: &'input [u8], opts: Options) -> Result<EndianDebugInfo<'input>> {
        let file = object::File::parse(bytes)?;
        if file.is_little_endian() {
            Ok(EndianDebugInfo::LEInfo(
                DebugInfo::new(&file, opts, gimli::LittleEndian)?,
            ))
        } else {
            Ok(EndianDebugInfo::BEInfo(
                DebugInfo::new(&file, opts, gimli::BigEndian)?,
            ))
        }
    }

    fn locate(
        &mut self,
        addr: u64,
    ) -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<'input, str>>)>> {
        match *self {
            EndianDebugInfo::LEInfo(ref mut dbg) => dbg.locate(addr),
            EndianDebugInfo::BEInfo(ref mut dbg) => dbg.locate(addr),
        }
    }
}

impl<'input, Endian> DebugInfo<'input, Endian>
where
    Endian: gimli::Endianity,
{
    fn new(
        file: &object::File<'input>,
        opts: Options,
        endian: Endian,
    ) -> Result<DebugInfo<'input, Endian>> {
        let debug_info = file.get_section(".debug_info")
            .ok_or(ErrorKind::MissingDebugSection("debug_info"))?;
        let debug_info = gimli::DebugInfo::new(debug_info, endian);
        let debug_abbrev = file.get_section(".debug_abbrev")
            .ok_or(ErrorKind::MissingDebugSection("debug_abbrev"))?;
        let debug_abbrev = gimli::DebugAbbrev::new(debug_abbrev, endian);
        let debug_line = file.get_section(".debug_line")
            .ok_or(ErrorKind::MissingDebugSection("debug_line"))?;
        let debug_line = gimli::DebugLine::new(debug_line, endian);
        let debug_ranges = file.get_section(".debug_ranges").unwrap_or(&[]);
        let debug_ranges = gimli::DebugRanges::new(debug_ranges, endian);
        let debug_str = file.get_section(".debug_str").unwrap_or(&[]);
        let debug_str = gimli::DebugStr::new(debug_str, endian);

        let mut units = Vec::new();
        let mut headers = debug_info.units();
        while let Some(header) = headers.next().chain_err(|| "couldn't get DIE header")? {
            let unit = Unit::parse(
                &debug_abbrev,
                &debug_ranges,
                &debug_line,
                &debug_str,
                &header,
                opts,
            );
            let unit = unit.chain_err(|| "encountered invalid compilation unit")?;
            if let Some(unit) = unit {
                units.push(unit);
            }
        }

        Ok(DebugInfo {
            units: units,
            opts: opts,
        })
    }

    pub fn locate(
        &mut self,
        addr: u64,
    ) -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<'input, str>>)>> {
        // First, find the compilation unit for the given address
        for unit in &mut self.units {
            if !unit.maybe_contains_address(addr) {
                continue;
            }

            unit.lines.read_sequences();
            let row = unit.lines.locate(addr);
            if row.is_none() {
                continue;
            }
            let row = row.unwrap();
            let header = unit.lines.program_rows.header();

            let file = header
                .file(row.file_index)
                .ok_or_else(|| {
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::InvalidDebugLineTarget)
                })?;

            let mut path = path::PathBuf::new();
            if let Some(directory) = file.directory(header) {
                let directory = directory.to_string_lossy();
                if !directory.starts_with('/') {
                    if let Some(comp_dir) = unit.comp_dir() {
                        path.push(&*comp_dir.to_string_lossy());
                    }
                }
                path.push(&*directory);
            }
            path.push(&*file.path_name().to_string_lossy());

            let line = row.line;
            if unit.programs.is_empty() {
                return Ok(Some((path, line, None)));
            }

            // The unit also has programs, so let's look for the function wrapping this address.
            let mut func: Option<(&Program<Endian>, &gimli::Range, u64)> = None;
            for p in &unit.programs {
                if !p.contains_address(addr) {
                    continue;
                }

                // This program covers the given address -- calculate how well it matches
                let (range, dist) = p.ranges
                    .iter()
                    .filter(|range| addr >= range.begin && addr < range.end)
                    .map(|range| (range, addr - range.begin))
                    .min_by_key(|&(_, dist)| dist)
                    .expect("p.contains_address() is true, but no matching range found");

                if let Some((prev, prange, pdist)) = func.take() {
                    // are we a better match?
                    func = if dist == pdist {
                        // we're equally good -- are we tighter?
                        if range.end <= prange.end {
                            // If range.end < prange.end, then we're a tighter match.
                            //
                            // Otherwise, we found two equally good ranges for this address.
                            // This probably happened because of a function like:
                            //
                            //   fn foo() { bar() }
                            //
                            // where bar() was inlined.
                            //
                            // Currently we just take the later one, which should be the
                            // "innermost" function (because we populated `programs` in DFS order),
                            // which is what we want. In the future, we may want to expose the full
                            // call chain in this case (and the others where there is a range
                            // conflcit).
                            Some((p, range, dist))
                        } else {
                            // no
                            Some((prev, prange, pdist))
                        }
                    } else if dist < pdist {
                        // we're just better
                        Some((p, range, dist))
                    } else {
                        // current is better
                        Some((prev, prange, pdist))
                    };
                } else {
                    func = Some((p, range, dist));
                }

                // Unfortunately, we're not done even if we've already found loc, because there
                // *may* be another subprogram (specifically, one that has been inlined) that
                // matches us better. We need to keep going until we find the best one.
            }

            let with_demangling = self.opts.with_demangling;
            let func = func.map(|u| {
                if unit.language.is_some() {
                    debug_assert!(
                        with_demangling,
                        "We shouldn't even bother finding the DW_AT_language if we \
                         aren't demangling"
                    );
                }
                match unit.language {
                    Some(gimli::DW_LANG_C_plus_plus) |
                    Some(gimli::DW_LANG_C_plus_plus_03) |
                    Some(gimli::DW_LANG_C_plus_plus_11) => demangle_cpp_symbol(u.0.name.buf()),
                    Some(gimli::DW_LANG_Rust) => demangle_rust_symbol(u.0.name.buf()),
                    _ => u.0.name.to_string_lossy(),
                }
            });

            return Ok(Some((path, line, func)));
        }
        Ok(None)
    }
}

#[cfg(feature = "cpp_demangle")]
fn demangle_cpp_symbol(mangled: &[u8]) -> Cow<str> {
    if let Ok(sym) = cpp_demangle::Symbol::new(mangled) {
        Cow::from(format!("{}", sym))
    } else {
        String::from_utf8_lossy(mangled)
    }
}

#[cfg(not(feature = "cpp_demangle"))]
fn demangle_cpp_symbol(mangled: &[u8]) -> Cow<str> {
    String::from_utf8_lossy(mangled)
}

#[cfg(feature = "rustc-demangle")]
fn demangle_rust_symbol(mangled: &[u8]) -> Cow<str> {
    Cow::from(format!(
        "{}",
        rustc_demangle::demangle(String::from_utf8_lossy(mangled).as_ref())
    ))
}

#[cfg(not(feature = "rustc-demangle"))]
fn demangle_rust_symbol(mangled: &[u8]) -> Cow<str> {
    String::from_utf8_lossy(mangled)
}

struct Unit<'input, Endian>
where
    Endian: gimli::Endianity,
{
    range: Option<gimli::Range>,
    lines: Lines<'input, Endian>,
    comp_dir: Option<gimli::EndianBuf<'input, Endian>>,
    programs: Vec<Program<'input, Endian>>,
    language: Option<gimli::DwLang>,
}

impl<'input, Endian> Unit<'input, Endian>
where
    Endian: gimli::Endianity,
{
    fn parse(
        debug_abbrev: &gimli::DebugAbbrev<gimli::EndianBuf<Endian>>,
        debug_ranges: &gimli::DebugRanges<gimli::EndianBuf<Endian>>,
        debug_line: &gimli::DebugLine<gimli::EndianBuf<'input, Endian>>,
        debug_str: &gimli::DebugStr<gimli::EndianBuf<'input, Endian>>,
        header: &gimli::CompilationUnitHeader<gimli::EndianBuf<'input, Endian>>,
        opts: Options,
    ) -> Result<Option<Unit<'input, Endian>>> {

        // We first want to parse out the compilation unit, and then any contained subprograms.
        let abbrev = header
            .abbreviations(debug_abbrev)
            .chain_err(|| "compilation unit refers to non-existing abbreviations")?;

        let mut entries = header.entries(&abbrev);
        let base_address;
        let mut unit = {
            // Scoped so that we can continue using entries for the loop below
            let (_, entry) = entries
                .next_dfs()
                .chain_err(|| "compilation unit is broken")?
                .ok_or_else(|| {
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::UnitWithoutCompilationUnit)
                })?;

            if entry.tag() != gimli::DW_TAG_compile_unit {
                return Err(
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::MissingComplilationUnit).into(),
                );
            }

            base_address = match entry.attr_value(gimli::DW_AT_low_pc) {
                Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
                Err(e) => {
                    return Err(Error::from(ErrorKind::Gimli(e)))
                        .chain_err(|| "invalid low_pc attribute")
                }
                _ => match entry.attr_value(gimli::DW_AT_entry_pc) {
                    Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
                    Err(e) => {
                        return Err(Error::from(ErrorKind::Gimli(e)))
                            .chain_err(|| "invalid entry_pc attribute")
                    }
                    _ => 0,
                },
            };

            // Where does our compilation unit live?
            let range = Self::parse_contiguous_range(entry)
                .chain_err(|| "compilation unit has invalid low_pc and/or high_pc")?;

            // Extract source file and line information about the compilation unit
            let line_offset = match entry.attr_value(gimli::DW_AT_stmt_list) {
                Ok(Some(gimli::AttributeValue::DebugLineRef(offset))) => offset,
                Err(e) => {
                    return Err(Error::from(ErrorKind::Gimli(e)))
                        .chain_err(|| "invalid compilation unit statement list")
                }
                _ => return Ok(None),
            };
            let comp_dir = entry
                .attr(gimli::DW_AT_comp_dir)
                .map_err(|e| Error::from(ErrorKind::Gimli(e)))
                .chain_err(|| "invalid compilation unit directory")?
                .and_then(|attr| attr.string_value(debug_str));
            let comp_name = entry
                .attr(gimli::DW_AT_name)
                .map_err(|e| Error::from(ErrorKind::Gimli(e)))
                .chain_err(|| "invalid compilation unit name")?
                .and_then(|attr| attr.string_value(debug_str));
            let language = if opts.with_demangling {
                entry
                    .attr(gimli::DW_AT_language)
                    .map_err(|e| Error::from(ErrorKind::Gimli(e)))?
                    .and_then(|attr| match attr.value() {
                        gimli::AttributeValue::Language(lang) => Some(lang),
                        _ => None,
                    })
            } else {
                None
            };

            let lines = Lines::new(
                debug_line,
                line_offset,
                header.address_size(),
                comp_dir,
                comp_name,
            )?;

            Unit {
                range: range,
                lines: lines,
                comp_dir,
                programs: vec![],
                language: language,
            }
        };

        // Do we also need to extract function information?
        if !opts.with_functions {
            return Ok(Some(unit));
        }

        while let Some((_, entry)) = entries
            .next_dfs()
            .chain_err(|| "tree below compilation unit yielded invalid entry")?
        {

            // We only care about functions
            match entry.tag() {
                gimli::DW_TAG_inlined_subroutine | gimli::DW_TAG_subprogram => (),
                _ => continue,
            }

            // Where does this function live?
            let ranges = Self::get_ranges(entry, debug_ranges, header.address_size(), base_address)
                .chain_err(|| "subroutine has invalid ranges")?;
            if ranges.is_empty() {
                continue;
            }

            // When resolving an address, the code first looks for which compilation units have
            // ranges that cover the address in question, and then only search within those. This
            // relies on the assumption that all program ranges within a compilation unit are fully
            // contained within compilation unit's range. That is
            //
            //   ∀r ∈ cu.ranges (cu.begin <= r <= cu.end)
            //
            // It turns out that this is not true: https://github.com/gimli-rs/addr2line/issues/30.
            // In such programs, we will fail to resolve addresses located in units whose range
            // fall outside the range of the containing compilation unit. This is not a hard error,
            // since we can still generally resolve addresses, and tools like binutils' addr2line
            // exhibit the same behavior, but it is something we should aim to eventually work
            // around. Hence: TODO

            let maybe_name = Self::resolve_name(entry, header, debug_str, &abbrev)
                .chain_err(|| {
                    format!(
                        "failed to resolve name for subroutine at <{:x}><{:x}>",
                        header.offset().0,
                        entry.offset().0
                    )
                })?;

            if let Some(name) = maybe_name {
                unit.programs.push(Program {
                    ranges: ranges,
                    inlined: entry.tag() == gimli::DW_TAG_inlined_subroutine,
                    name: name,
                });
            }
        }

        Ok(Some(unit))
    }

    fn resolve_name<'a, 'b>(
        entry: &gimli::DebuggingInformationEntry<'a, 'b, gimli::EndianBuf<'input, Endian>>,
        header: &gimli::CompilationUnitHeader<gimli::EndianBuf<'input, Endian>>,
        debug_str: &gimli::DebugStr<gimli::EndianBuf<'input, Endian>>,
        abbrev: &gimli::Abbreviations,
    ) -> Result<Option<gimli::EndianBuf<'input, Endian>>> {

        // For naming, we prefer the linked name, if available
        if let Some(name) = entry
            .attr(gimli::DW_AT_linkage_name)
            .map_err(|e| Error::from(ErrorKind::Gimli(e)))
            .chain_err(|| "invalid subprogram linkage name")?
            .and_then(|attr| attr.string_value(debug_str))
        {
            return Ok(Some(name));
        }
        if let Some(name) = entry
            .attr(gimli::DW_AT_MIPS_linkage_name)
            .map_err(|e| Error::from(ErrorKind::Gimli(e)))
            .chain_err(|| "invalid subprogram linkage name")?
            .and_then(|attr| attr.string_value(debug_str))
        {
            return Ok(Some(name));
        }

        // Linked name is not available, so fall back to just plain old name, if that's available.
        if let Some(name) = entry
            .attr(gimli::DW_AT_name)
            .map_err(|e| Error::from(ErrorKind::Gimli(e)))
            .chain_err(|| "invalid subprogram name")?
            .and_then(|attr| attr.string_value(debug_str))
        {
            return Ok(Some(name));
        }

        // If we don't have the link name, check if this function refers to another
        if let Some(abstract_origin) =
            Self::get_entry(entry, header, abbrev, gimli::DW_AT_abstract_origin)
                .chain_err(|| "invalid subprogram abstract origin")?
        {
            let name = Self::resolve_name(&abstract_origin, header, debug_str, abbrev)
                .chain_err(|| "abstract origin does not resolve to a name")?;
            return Ok(name);
        }
        if let Some(specification) =
            Self::get_entry(entry, header, abbrev, gimli::DW_AT_specification)
                .chain_err(|| "invalid subprogram specification")?
        {
            let name = Self::resolve_name(&specification, header, debug_str, abbrev)
                .chain_err(|| "specification does not resolve to a name")?;
            return Ok(name);
        }

        Ok(None)
    }

    fn get_entry<'a>(
        entry: &gimli::DebuggingInformationEntry<'a, 'a, gimli::EndianBuf<'input, Endian>>,
        header: &'a gimli::CompilationUnitHeader<gimli::EndianBuf<'input, Endian>>,
        abbrev: &'a gimli::Abbreviations,
        attr: gimli::DwAt,
    ) -> Result<Option<gimli::DebuggingInformationEntry<'a, 'a, gimli::EndianBuf<'input, Endian>>>> {
        if let Some(gimli::AttributeValue::UnitRef(offset)) = entry
            .attr_value(attr)
            .map_err(|e| Error::from(ErrorKind::Gimli(e)))?
        {
            let mut entries = header.entries_at_offset(abbrev, offset)?;
            let (_, entry) = entries
                .next_dfs()?
                .ok_or_else(|| {
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::DanglingEntryOffset)
                })?;
            return Ok(Some(entry.clone()));
        }

        Ok(None)
    }

    fn get_ranges(
        entry: &gimli::DebuggingInformationEntry<gimli::EndianBuf<Endian>>,
        debug_ranges: &gimli::DebugRanges<gimli::EndianBuf<Endian>>,
        address_size: u8,
        base_address: u64,
    ) -> Result<Vec<gimli::Range>> {
        if let Some(range) =
            Self::parse_noncontiguous_ranges(entry, debug_ranges, address_size, base_address)?
        {
            return Ok(range);
        }
        if let Some(range) = Self::parse_contiguous_range(entry)?
            .map(|range| vec![range])
        {
            return Ok(range);
        }
        return Ok(vec![]);
    }

    // This must be checked before `parse_contiguous_range`.
    fn parse_noncontiguous_ranges(
        entry: &gimli::DebuggingInformationEntry<gimli::EndianBuf<Endian>>,
        debug_ranges: &gimli::DebugRanges<gimli::EndianBuf<Endian>>,
        address_size: u8,
        base_address: u64,
    ) -> Result<Option<Vec<gimli::Range>>> {
        let offset = match entry.attr_value(gimli::DW_AT_ranges) {
            Ok(Some(gimli::AttributeValue::DebugRangesRef(offset))) => offset,
            Err(e) => {
                return Err(Error::from(ErrorKind::Gimli(e)))
                    .chain_err(|| "invalid ranges attribute")
            }
            _ => return Ok(None),
        };

        let ranges = debug_ranges
            .ranges(offset, address_size, base_address)
            .chain_err(|| "range offsets are not valid")?;
        let ranges = ranges.collect().chain_err(|| "range could not be parsed")?;
        Ok(Some(ranges))
    }

    fn parse_contiguous_range(
        entry: &gimli::DebuggingInformationEntry<gimli::EndianBuf<Endian>>,
    ) -> Result<Option<gimli::Range>> {
        let low_pc = match entry.attr_value(gimli::DW_AT_low_pc) {
            Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
            Err(e) => {
                return Err(Error::from(ErrorKind::Gimli(e)))
                    .chain_err(|| "invalid low_pc attribute")
            }
            _ => return Ok(None),
        };

        let high_pc = match entry.attr_value(gimli::DW_AT_high_pc) {
            Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
            Ok(Some(gimli::AttributeValue::Udata(size))) => low_pc.wrapping_add(size),
            Err(e) => {
                return Err(Error::from(ErrorKind::Gimli(e)))
                    .chain_err(|| "invalid high_pc attribute")
            }
            _ => return Ok(None),
        };

        if low_pc == 0 {
            // https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blob;f=gdb/dwarf2read.c;h=ed10e03812f381ccdb5c51e1c689df8d61ab87f6;hb=HEAD#l16000
            // TODO: *technically* there could be a relocatable section placed at VA 0
            return Ok(None);
        }

        if low_pc == high_pc {
            // https://sourceware.org/ml/gdb-patches/2011-03/msg00739.html
            return Ok(None);
        }

        if low_pc > high_pc {
            return Err(
                ErrorKind::InvalidDebugSymbols(DebugInfoError::RangeInverted).into(),
            );
        }

        Ok(Some(gimli::Range {
            begin: low_pc,
            end: high_pc,
        }))
    }

    fn maybe_contains_address(&self, address: u64) -> bool {
        match self.range {
            Some(range) => address >= range.begin && address < range.end,
            None => true,
        }
    }

    fn comp_dir(&self) -> Option<gimli::EndianBuf<'input, Endian>> {
        self.comp_dir
    }
}

struct Program<'input, Endian>
where
    Endian: gimli::Endianity,
{
    ranges: Vec<gimli::Range>,
    name: gimli::EndianBuf<'input, Endian>,
    #[allow(dead_code)]
    inlined: bool,
}

impl<'input, Endian> Program<'input, Endian>
where
    Endian: gimli::Endianity,
{
    fn contains_address(&self, address: u64) -> bool {
        self.ranges
            .iter()
            .any(|range| address >= range.begin && address < range.end)
    }
}

struct Lines<'input, Endian>
where
    Endian: gimli::Endianity,
{
    program_rows: gimli::StateMachine<
        gimli::EndianBuf<'input, Endian>,
        gimli::IncompleteLineNumberProgram<gimli::EndianBuf<'input, Endian>>,
    >,
    sequences: Vec<Sequence>,
    read_sequences: bool,
}

impl<'input, Endian> Lines<'input, Endian>
where
    Endian: gimli::Endianity,
{
    fn new(
        debug_line: &gimli::DebugLine<gimli::EndianBuf<'input, Endian>>,
        line_offset: gimli::DebugLineOffset,
        address_size: u8,
        comp_dir: Option<gimli::EndianBuf<'input, Endian>>,
        comp_name: Option<gimli::EndianBuf<'input, Endian>>,
    ) -> Result<Self> {
        let program = debug_line
            .program(line_offset, address_size, comp_dir, comp_name)?;
        Ok(Lines {
            program_rows: program.rows(),
            sequences: Vec::new(),
            read_sequences: false,
        })
    }

    fn read_sequences(&mut self) {
        if self.read_sequences {
            return;
        }
        let mut sequences = Vec::new();
        let mut sequence_rows: Vec<Row> = Vec::new();
        let mut prev_address = 0;
        while let Ok(Some((_, &program_row))) = self.program_rows.next_row() {
            let address = program_row.address();
            if program_row.end_sequence() {
                if !sequence_rows.is_empty() {
                    let low_address = sequence_rows[0].address;
                    let high_address = if address < prev_address {
                        prev_address + 1
                    } else {
                        address
                    };
                    let mut rows = Vec::new();
                    std::mem::swap(&mut rows, &mut sequence_rows);
                    sequences.push(Sequence {
                        low_address,
                        high_address,
                        rows,
                    });
                }
                prev_address = 0;
            } else if address < prev_address {
                // The standard says:
                // "Within a sequence, addresses and operation pointers may only increase."
                // So this row is invalid, we can ignore it.
                //
                // If we wanted to handle this, we could start a new sequence
                // here, but let's wait until that is needed.
            } else {
                let file_index = program_row.file_index();
                let line = program_row.line();
                let mut duplicate = false;
                if let Some(last_row) = sequence_rows.last_mut() {
                    if last_row.address == address {
                        last_row.file_index = file_index;
                        last_row.line = line;
                        duplicate = true;
                    }
                }
                if !duplicate {
                    sequence_rows.push(Row {
                        address,
                        file_index,
                        line,
                    });
                }
                prev_address = address;
            }
        }
        if !sequence_rows.is_empty() {
            // A sequence without an end_sequence row.
            // Let's assume the last row covered 1 byte.
            let low_address = sequence_rows[0].address;
            let high_address = prev_address + 1;
            sequences.push(Sequence {
                low_address,
                high_address,
                rows: sequence_rows,
            });
        }
        // Sort so we can binary search.
        sequences.sort_by(|a, b| a.low_address.cmp(&b.low_address));
        self.sequences = sequences;
        self.read_sequences = true;
    }

    fn locate(&self, address: u64) -> Option<&Row> {
        debug_assert!(self.read_sequences);
        let idx = self.sequences
            .binary_search_by(|sequence| if address < sequence.low_address {
                std::cmp::Ordering::Greater
            } else if address < sequence.high_address {
                std::cmp::Ordering::Equal
            } else {
                std::cmp::Ordering::Less
            })
            .ok();
        idx.and_then(|idx| self.sequences[idx].locate(address))
    }
}

#[derive(Debug)]
struct Sequence {
    low_address: u64,
    high_address: u64,
    rows: Vec<Row>,
}

impl Sequence {
    fn locate(&self, address: u64) -> Option<&Row> {
        match self.rows.binary_search_by(|row| row.address.cmp(&address)) {
            Ok(idx) => self.rows.get(idx),
            Err(0) => None,
            Err(idx) => self.rows.get(idx - 1),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Row {
    address: u64,
    file_index: u64,
    line: Option<u64>,
}
