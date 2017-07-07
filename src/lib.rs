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
use std::sync;
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
    /// A subroutine (unit offset, routine offset) has no name
    SubroutineMissingName(usize, usize),
    /// Entry offset points to empty entry
    DanglingEntryOffset,
    /// Asked to parse non-contiguous range as contiguous.
    RangeBothContiguousAndNot,
    /// A range was inverted (high > low)
    RangeInverted,
}

impl fmt::Display for DebugInfoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DebugInfoError::InvalidDebugLineTarget => {
                write!(f, "DebugLine referst to a file that does not exist")
            }
            DebugInfoError::MissingComplilationUnit => {
                write!(f,
                       "A unit was completely empty (i.e., did not contain a compilation unit)")
            }
            DebugInfoError::UnitWithoutCompilationUnit => {
                write!(f, "The first entry in a unit is not a compilation unit")
            }
            DebugInfoError::SubroutineMissingName(u, r) => {
                write!(f, "A subroutine (<{:x}><{:x}>) has no name", u, r)
            }
            DebugInfoError::DanglingEntryOffset => write!(f, "Entry offset points to empty entry"),
            DebugInfoError::RangeBothContiguousAndNot => {
                write!(f, "Asked to parse non-contiguous range as contiguous.")
            }
            DebugInfoError::RangeInverted => write!(f, "A range was inverted (high > low)"),
        }
    }
}

impl error::Error for DebugInfoError {
    fn description(&self) -> &str {
        "An error occured while traversing debug symbols"
    }
}

mod errors {
    use gimli;
    use std::io;
    use super::DebugInfoError;

    error_chain! {
        foreign_links {
            Gimli(gimli::Error) #[doc="An error given by the `gimli` library while parsing the DWARF debug symbols."];
            BadPath(io::Error) #[doc="The path given could not be used to extract debug symbols."];
            InvalidDebugSymbols(DebugInfoError) #[doc="An error occured while traversing the debug symbols in the provided executable."];
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

/// A builder for configuring a `Mapping`.
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

    /// Finish configuration and build the `Mapping`.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out
    /// the necessary debug symbols, without copying data when possible.
    pub fn build<P>(self, file_path: P) -> Result<Mapping>
        where P: AsRef<path::Path>
    {
        Mapping::new_inner(file_path.as_ref(), self)
    }
}

/// A `Mapping` locates and maintains the state necessary to perform address to line translation.
///
/// Constructing a `Mapping` is somewhat costly, so users should aim to re-use created `Mapping`s
/// when performing lookups for many addresses over the same executable.
pub struct Mapping {
    // we use `OwningHandle` so that we can store both the Mmap and its associated object::File.
    // the lifetime of MmapDeriver here isn't *technically* static, it's *out* lifetime, but there
    // isn't a good way to express that as far as I am aware? suggestions welcome.
    inner: OwningHandle<Box<memmap::Mmap>, Box<MmapDerived<'static>>>,
}

/// `MmapDerived` uses an `OwningHandle` to allow an `object::File` and the data structures we
/// derive from it to be co-owned.
struct MmapDerived<'mmap> {
    inner: OwningHandle<Box<object::File<'mmap>>, Box<EndianDebugInfo<'mmap>>>,
}

enum EndianDebugInfo<'object> {
    LEInfo(DebugInfo<'object, gimli::LittleEndian>),
    BEInfo(DebugInfo<'object, gimli::BigEndian>),
}

/// `DebugInfo` holds the debug information derived from a wrapping `object::File`.
struct DebugInfo<'object, Endian>
    where Endian: gimli::Endianity
{
    debug_line: gimli::DebugLine<'object, Endian>,
    units: Vec<Unit<'object, Endian>>,
    opts: Options,
}

impl Mapping {
    /// Construct a new `Mapping` with the default `Options`.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out the necessary debug
    /// symbols, without copying data when possible.
    pub fn new<P>(file_path: P) -> Result<Mapping>
        where P: AsRef<path::Path>
    {
        Options::default().build(file_path)
    }

    fn new_inner(file_path: &path::Path, opts: Options) -> Result<Mapping> {
        let file = memmap::Mmap::open_path(file_path, memmap::Protection::Read)
            .map_err(|e| ErrorKind::BadPath(e))?;

        OwningHandle::try_new(Box::new(file), |mmap| -> Result<_> {
            let mmap: &memmap::Mmap = unsafe { &*mmap };
            let file = object::File::parse(unsafe { mmap.as_slice() })?;
            OwningHandle::try_new(Box::new(file), |file| -> Result<_> {
                let file: &object::File = unsafe { &*file };
                Self::symbolicate(file, opts)
                    .chain_err(|| "failed to analyze debug information")
                    .map(|di| Box::new(di))
            })
                    .map(|di| Box::new(MmapDerived { inner: di }))
        })
                .map(|di| Mapping { inner: di })
    }

    /// Locate the source file and line corresponding to the given virtual memory address.
    ///
    /// If the `Mapping` was constructed with `with_functions`, information about the containing
    /// function may also be returned when available.
    pub fn locate(&self,
                  addr: u64)
                  -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<str>>)>> {
        self.inner.locate(addr)
    }

    fn symbolicate<'a>(file: &'a object::File, opts: Options) -> Result<EndianDebugInfo<'a>> {
        if file.is_little_endian() {
            Ok(EndianDebugInfo::LEInfo(DebugInfo::new(file, opts)?))
        } else {
            Ok(EndianDebugInfo::BEInfo(DebugInfo::new(file, opts)?))
        }
    }
}

impl<'object> EndianDebugInfo<'object> {
    fn locate(&self, addr: u64) -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<str>>)>> {
        match *self {
            EndianDebugInfo::LEInfo(ref dbg) => dbg.locate(addr),
            EndianDebugInfo::BEInfo(ref dbg) => dbg.locate(addr),
        }
    }
}

impl<'object, Endian> DebugInfo<'object, Endian>
    where Endian: gimli::Endianity
{
    fn new<'a>(file: &'a object::File, opts: Options) -> Result<DebugInfo<'a, Endian>> {
        let debug_info = file.get_section(".debug_info")
            .ok_or(ErrorKind::MissingDebugSection("debug_info"))?;
        let debug_info = gimli::DebugInfo::<Endian>::new(debug_info);
        let debug_abbrev = file.get_section(".debug_abbrev")
            .ok_or(ErrorKind::MissingDebugSection("debug_abbrev"))?;
        let debug_abbrev = gimli::DebugAbbrev::<Endian>::new(debug_abbrev);
        let debug_line = file.get_section(".debug_line")
            .ok_or(ErrorKind::MissingDebugSection("debug_line"))?;
        let debug_line = gimli::DebugLine::<Endian>::new(debug_line);
        let debug_ranges = file.get_section(".debug_ranges").unwrap_or(&[]);
        let debug_ranges = gimli::DebugRanges::<Endian>::new(debug_ranges);
        let debug_str = file.get_section(".debug_str").unwrap_or(&[]);
        let debug_str = gimli::DebugStr::<Endian>::new(debug_str);

        let mut units = Vec::new();
        let mut headers = debug_info.units();
        while let Some(header) = headers.next().chain_err(|| "couldn't get DIE header")? {
            let unit = Unit::parse(&debug_abbrev,
                                   &debug_ranges,
                                   &debug_line,
                                   &debug_str,
                                   &header,
                                   opts);
            let unit = unit.chain_err(|| "encountered invalid compilation unit")?;
            if let Some(unit) = unit {
                units.push(unit);
            }
        }

        Ok(DebugInfo {
               debug_line: debug_line,
               units: units,
               opts: opts,
           })
    }

    pub fn locate(&self,
                  addr: u64)
                  -> Result<Option<(path::PathBuf, Option<u64>, Option<Cow<str>>)>> {
        // First, find the compilation unit for the given address
        for unit in &self.units {
            if !unit.contains_address(addr) {
                continue;
            }

            let mut rowi = 0;
            let mut current = None;

            // Okay, this is the right unit. Check our DebugLine rows.
            let rows =
                unit.cache_every.and_then(|cache_every| {
                    unit.skiplist.read().ok().and_then(|skiplist| {
                        match skiplist.binary_search_by_key(&addr, |&(raddr, _, _)| raddr) {
                            Ok(i) => {
                                // we have a state machine for this address!
                                current = Some(skiplist[i].2);
                                rowi = (i + 1) * cache_every + 1;
                                Some(skiplist[i].1.clone())
                            }
                            Err(i) if i == 0 => {
                                // i is the entry in the skiplist where addr *would* appear. Thus,
                                // we don't have a state machine for *this* address, but we do know
                                // that:
                                //
                                //  - if i == 0, we have no state machine, and must from scratch
                                //  - if i == skiplist.len(), we scan from the last state machine
                                //  - otherwise, the machine from i-1 eventually encounters addr
                                None
                            }
                            Err(i) => {
                                current = Some(skiplist[i - 1].2);
                                // NOTE
                                // we need to use i * cache_every here, not (i+1) as above.
                                // this is because i is the i *after* the one we're chooseing to
                                // start from (skiplist[i-1] above).
                                rowi = i * cache_every + 1;
                                Some(skiplist[i - 1].1.clone())
                            }
                        }
                    })
                });

            let mut rows = if let Some(rows) = rows {
                rows
            } else {
                // fall back to linear scan
                unit.line_rows(&self.debug_line)
                    .map_err(|e| Error::from(ErrorKind::Gimli(e)))
                    .chain_err(|| "cannot get line rows for unit")?
            };

            // Now, find the last row before a row with a higher address than the one we seek.
            let mut praddr = 0;
            let mut skipseq = false;
            while let Ok(Some((_, &row))) = rows.next_row() {
                if row.end_sequence() {
                    current = None;
                    skipseq = false;
                    continue;
                }

                if skipseq {
                    continue;
                }

                let raddr = row.address();
                if raddr < praddr {
                    // NOTE:
                    // We currently skip these sequences, but we *should* of course handle them
                    // correctly. It's unclear how the interplay between this and the skiplist
                    // should work.
                    // unimplemented!();
                    skipseq = true;
                    continue;
                }
                praddr = raddr;

                if raddr <= addr {
                    // Might be the right row, but we won't know until we see the next one.
                    // The .clone is needed so we can keep iterating
                    current = Some(row);

                    // Add every cache_every'th non-empty row to the skiplist
                    if let Some(cache_every) = unit.cache_every {
                        if rowi != 0 && rowi % cache_every == 0 {
                            if let Ok(mut skiplist) = unit.skiplist.write() {
                                let i = rowi / cache_every - 1;
                                if i >= skiplist.len() {
                                    debug_assert!(i == skiplist.len(),
                                                  "we somehow didn't cache a StateMachine for a \
                                                   previous iteration step!");
                                    // cache this StateMachine
                                    skiplist.push((row.address(), rows.clone(), row));
                                }
                            }
                        }
                    }
                    rowi += 1;
                    continue;
                }
                break;
            }

            // The row we just last iterated to is *after* the address, to the previous row we
            // saw (stored in current) is the one we want. If there is no current, then we have to
            // give up on locating the address in this unit (and thus in the program too).
            //
            // TODO
            // Can we return partial information here by giving, say, information from the
            // compilation unit itself?
            if current.is_none() {
                return Ok(None);
            }
            let row = current.unwrap();
            let header = rows.header();

            let file = row.file(header)
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

            let line = row.line();
            if unit.programs.is_empty() {
                return Ok(Some((path, line, None)));
            }

            // The unit also has programs, so let's look for the function wrapping this address.
            let mut func: Option<(&Program, &gimli::Range, u64)> = None;
            for p in &unit.programs {
                if !p.contains_address(addr) {
                    continue;
                }

                // This program covers the given address -- calculate how well it matches
                let (range, dist) =
                    p.ranges
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

            let func = func.map(|u| {
                if unit.language.is_some() {
                    debug_assert!(self.opts.with_demangling,
                                  "We shouldn't even bother finding the DW_AT_language if we \
                                   aren't demangling");
                }
                match unit.language {
                    Some(gimli::DW_LANG_C_plus_plus) |
                    Some(gimli::DW_LANG_C_plus_plus_03) |
                    Some(gimli::DW_LANG_C_plus_plus_11) => demangle_cpp_symbol(u.0.name),
                    Some(gimli::DW_LANG_Rust) => demangle_rust_symbol(u.0.name),
                    _ => u.0.name.to_string_lossy(),
                }
            });

            return Ok(Some((path, line, func)));
        }
        Ok(None)
    }
}

#[cfg(feature = "cpp_demangle")]
fn demangle_cpp_symbol(mangled: &std::ffi::CStr) -> Cow<str> {
    if let Ok(sym) = cpp_demangle::Symbol::new(mangled.to_bytes()) {
        Cow::from(format!("{}", sym))
    } else {
        mangled.to_string_lossy()
    }
}

#[cfg(not(feature = "cpp_demangle"))]
fn demangle_cpp_symbol(mangled: &std::ffi::CStr) -> Cow<str> {
    mangled.to_string_lossy()
}

#[cfg(feature = "rustc-demangle")]
fn demangle_rust_symbol(mangled: &std::ffi::CStr) -> Cow<str> {
    Cow::from(format!("{}",
                      rustc_demangle::demangle(mangled.to_string_lossy().as_ref())))
}

#[cfg(not(feature = "rustc-demangle"))]
fn demangle_rust_symbol(mangled: &std::ffi::CStr) -> Cow<str> {
    mangled.to_string_lossy()
}

// TODO: most of this should be moved to the main library.
use std::marker::PhantomData;
struct Unit<'input, Endian>
    where Endian: gimli::Endianity
{
    skiplist: sync::RwLock<Vec<(u64, gimli::StateMachine<'input, gimli::IncompleteLineNumberProgram<'input, Endian>, Endian>, gimli::LineNumberRow)>>,

    cache_every: Option<usize>,
    address_size: u8,
    base_address: u64,
    ranges: Vec<gimli::Range>,
    line_offset: gimli::DebugLineOffset,
    comp_dir: Option<&'input std::ffi::CStr>,
    comp_name: Option<&'input std::ffi::CStr>,
    programs: Vec<Program<'input>>,
    language: Option<gimli::DwLang>,
    phantom: PhantomData<Endian>,
}

impl<'input, Endian> Unit<'input, Endian>
    where Endian: gimli::Endianity
{
    fn parse(debug_abbrev: &gimli::DebugAbbrev<Endian>,
             debug_ranges: &gimli::DebugRanges<Endian>,
             debug_line: &gimli::DebugLine<'input, Endian>,
             debug_str: &gimli::DebugStr<'input, Endian>,
             header: &gimli::CompilationUnitHeader<'input, Endian>,
             opts: Options)
             -> Result<Option<Unit<'input, Endian>>> {

        // We first want to parse out the compilation unit, and then any contained subprograms.
        let abbrev = header.abbreviations(*debug_abbrev)
            .chain_err(|| "compilation unit refers to non-existing abbreviations")?;

        let mut entries = header.entries(&abbrev);
        let mut unit = {
            // Scoped so that we can continue using entries for the loop below
            let (_, entry) = entries.next_dfs()
                .chain_err(|| "compilation unit is broken")?
                .ok_or_else(|| {
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::UnitWithoutCompilationUnit)
                })?;

            if entry.tag() != gimli::DW_TAG_compile_unit {
                return Err(ErrorKind::InvalidDebugSymbols(DebugInfoError::MissingComplilationUnit)
                               .into());
            }

            let base_address = match entry.attr_value(gimli::DW_AT_low_pc) {
                Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
                Err(e) => {
                    return Err(Error::from(ErrorKind::Gimli(e)))
                        .chain_err(|| "invalid low_pc attribute")
                }
                _ => {
                    match entry.attr_value(gimli::DW_AT_entry_pc) {
                        Ok(Some(gimli::AttributeValue::Addr(addr))) => addr,
                        Err(e) => {
                            return Err(Error::from(ErrorKind::Gimli(e)))
                                .chain_err(|| "invalid entry_pc attribute")
                        }
                        _ => 0,
                    }
                }
            };

            // Where does our compilation unit live?
            let ranges = Self::get_ranges(entry, debug_ranges, header.address_size(), base_address)
                .chain_err(|| "compilation unit has invalid ranges")?;
            if ranges.is_empty() {
                return Ok(None);
            }

            // Extract source file and line information about the compilation unit
            let line_offset = match entry.attr_value(gimli::DW_AT_stmt_list) {
                Ok(Some(gimli::AttributeValue::DebugLineRef(offset))) => offset,
                Err(e) => {
                    return Err(Error::from(ErrorKind::Gimli(e)))
                        .chain_err(|| "invalid compilation unit statement list")
                }
                _ => return Ok(None),
            };
            let comp_dir = entry.attr(gimli::DW_AT_comp_dir)
                .map_err(|e| Error::from(ErrorKind::Gimli(e)))
                .chain_err(|| "invalid compilation unit directory")?
                .and_then(|attr| attr.string_value(debug_str));
            let comp_name = entry.attr(gimli::DW_AT_name)
                .map_err(|e| Error::from(ErrorKind::Gimli(e)))
                .chain_err(|| "invalid compilation unit name")?
                .and_then(|attr| attr.string_value(debug_str));
            let language = if opts.with_demangling {
                entry.attr(gimli::DW_AT_language)
                    .map_err(|e| Error::from(ErrorKind::Gimli(e)))?
                    .and_then(|attr| match attr.value() {
                                  gimli::AttributeValue::Language(lang) => Some(lang),
                                  _ => None,
                              })
            } else {
                None
            };

            let linep = debug_line.program(line_offset, header.address_size(), comp_dir, comp_name)
                .chain_err(|| "invalid compilation unit line rows")?;

            // We want to cache every sqrt(#rows).
            // Unfortunately we don't know the number of rows (and we don't want to scan all of
            // line rows to find it). However, we *do* know the number of bytes of debug line
            // information, which we can use as a proxy.
            //
            // Based on some empirical data from a couple of applications, the relationship seems
            // to be about 5.5 bytes/row for units with a decent number of rows. The values
            // vary more for smaller units, but there cache_every also matters less.
            let nrows = linep.header().raw_program_buf().len() as f64 / 5.5;
            // If a unit only has a very small number of rows, we can avoid the skiplist
            // altogether (also, our estimate is more likely to be wrong).
            let cache_every = if nrows >= 100.0 {
                Some(nrows.sqrt() as usize)
            } else {
                None
            };

            Unit {
                skiplist: sync::RwLock::default(),
                cache_every: cache_every,

                address_size: header.address_size(),
                base_address: base_address,
                ranges: ranges,
                line_offset: line_offset,
                comp_dir: comp_dir,
                comp_name: comp_name,
                programs: vec![],
                language: language,
                phantom: PhantomData,
            }
        };

        // Do we also need to extract function information?
        if !opts.with_functions {
            return Ok(Some(unit));
        }

        while let Some((_, entry)) =
            entries.next_dfs().chain_err(|| "tree below compilation unit yielded invalid entry")? {

            // We only care about functions
            match entry.tag() {
                gimli::DW_TAG_inlined_subroutine |
                gimli::DW_TAG_subprogram => (),
                _ => continue,
            }

            // Where does this function live?
            let ranges =
                Self::get_ranges(entry,
                                 debug_ranges,
                                 header.address_size(),
                                 unit.base_address).chain_err(|| "subroutine has invalid ranges")?;
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

            let maybe_name = Self::resolve_name(entry, header, debug_str, &abbrev).chain_err(|| {
                               format!("failed to resolve name for subroutine at <{:x}><{:x}>",
                                       header.offset().0,
                                       entry.offset().0)
                           })?;

            let name = maybe_name.ok_or_else(|| {
                    ErrorKind::InvalidDebugSymbols(DebugInfoError::SubroutineMissingName(header.offset().0, entry.offset().0))
                })?;

            unit.programs.push(Program {
                                   ranges: ranges,
                                   inlined: entry.tag() == gimli::DW_TAG_inlined_subroutine,
                                   name: name,
                               });
        }

        Ok(Some(unit))
    }

    fn resolve_name<'a, 'b>(entry: &gimli::DebuggingInformationEntry<'input, 'a, 'b, Endian>,
                            header: &gimli::CompilationUnitHeader<'input, Endian>,
                            debug_str: &gimli::DebugStr<'input, Endian>,
                            abbrev: &gimli::Abbreviations)
                            -> Result<Option<&'input std::ffi::CStr>> {

        // For naming, we prefer the linked name, if available
        if let Some(name) = entry.attr(gimli::DW_AT_linkage_name)
               .map_err(|e| Error::from(ErrorKind::Gimli(e)))
               .chain_err(|| "invalid subprogram linkage name")?
               .and_then(|attr| attr.string_value(debug_str)) {
            return Ok(Some(name));
        }
        if let Some(name) = entry.attr(gimli::DW_AT_MIPS_linkage_name)
               .map_err(|e| Error::from(ErrorKind::Gimli(e)))
               .chain_err(|| "invalid subprogram linkage name")?
               .and_then(|attr| attr.string_value(debug_str)) {
            return Ok(Some(name));
        }

        // Linked name is not available, so fall back to just plain old name, if that's available.
        if let Some(name) = entry.attr(gimli::DW_AT_name)
               .map_err(|e| Error::from(ErrorKind::Gimli(e)))
               .chain_err(|| "invalid subprogram name")?
               .and_then(|attr| attr.string_value(debug_str)) {
            return Ok(Some(name));
        }

        // If we don't have the link name, check if this function refers to another
        if let Some(abstract_origin) = Self::get_entry(entry, header, abbrev, gimli::DW_AT_abstract_origin)
                .chain_err(|| "invalid subprogram abstract origin")? {
            let name = Self::resolve_name(&abstract_origin, header, debug_str, abbrev)
                .chain_err(|| "abstract origin does not resolve to a name")?;
            return Ok(name);
        }
        if let Some(specification) = Self::get_entry(entry, header, abbrev, gimli::DW_AT_specification)
                .chain_err(|| "invalid subprogram specification")? {
            let name = Self::resolve_name(&specification, header, debug_str, abbrev)
                .chain_err(|| "specification does not resolve to a name")?;
            return Ok(name);
        }

        Ok(None)
    }

    fn get_entry<'a>
        (entry: &gimli::DebuggingInformationEntry<'input, 'a, 'a, Endian>,
         header: &'a gimli::CompilationUnitHeader<'input, Endian>,
         abbrev: &'a gimli::Abbreviations,
         attr: gimli::DwAt)
         -> Result<Option<gimli::DebuggingInformationEntry<'input, 'a, 'a, Endian>>> {
        if let Some(gimli::AttributeValue::UnitRef(offset)) =
            entry.attr_value(attr).map_err(|e| Error::from(ErrorKind::Gimli(e)))? {
            let mut entries = header.entries_at_offset(abbrev, offset)?;
            let (_, entry) = entries.next_dfs()?
            .ok_or_else(|| {
                ErrorKind::InvalidDebugSymbols(DebugInfoError::DanglingEntryOffset)
            })?;
            return Ok(Some(entry.clone()));
        }

        Ok(None)
    }

    fn get_ranges(entry: &gimli::DebuggingInformationEntry<Endian>,
                  debug_ranges: &gimli::DebugRanges<Endian>,
                  address_size: u8,
                  base_address: u64)
                  -> Result<Vec<gimli::Range>> {
        if let Some(range) = Self::parse_noncontiguous_ranges(entry,
                                                              debug_ranges,
                                                              address_size,
                                                              base_address)? {
            return Ok(range);
        }
        if let Some(range) = Self::parse_contiguous_range(entry)?.map(|range| vec![range]) {
            return Ok(range);
        }
        return Ok(vec![]);
    }

    // This must be checked before `parse_contiguous_range`.
    fn parse_noncontiguous_ranges(entry: &gimli::DebuggingInformationEntry<Endian>,
                                  debug_ranges: &gimli::DebugRanges<Endian>,
                                  address_size: u8,
                                  base_address: u64)
                                  -> Result<Option<Vec<gimli::Range>>> {
        let offset = match entry.attr_value(gimli::DW_AT_ranges) {
            Ok(Some(gimli::AttributeValue::DebugRangesRef(offset))) => offset,
            Err(e) => {
                return Err(Error::from(ErrorKind::Gimli(e)))
                    .chain_err(|| "invalid ranges attribute")
            }
            _ => return Ok(None),
        };

        let ranges = debug_ranges.ranges(offset, address_size, base_address)
            .chain_err(|| "range offsets are not valid")?;
        let ranges = ranges.collect().chain_err(|| "range could not be parsed")?;
        Ok(Some(ranges))
    }

    fn parse_contiguous_range(entry: &gimli::DebuggingInformationEntry<Endian>)
                              -> Result<Option<gimli::Range>> {

        if let Ok(Some(..)) = entry.attr_value(gimli::DW_AT_ranges) {
            return Err(ErrorKind::InvalidDebugSymbols(DebugInfoError::RangeBothContiguousAndNot)
                           .into());
        }

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
            Ok(None) => low_pc.wrapping_add(1),
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
            return Err(ErrorKind::InvalidDebugSymbols(DebugInfoError::RangeInverted).into());
        }

        Ok(Some(gimli::Range {
                    begin: low_pc,
                    end: high_pc,
                }))
    }

    fn contains_address(&self, address: u64) -> bool {
        self.ranges.iter().any(|range| address >= range.begin && address < range.end)
    }

    fn line_rows(&self,
                 debug_line: &gimli::DebugLine<'input, Endian>)
                 -> gimli::Result<gimli::StateMachine<'input, gimli::IncompleteLineNumberProgram<'input, Endian>, Endian>> {
        debug_line.program(self.line_offset,
                           self.address_size,
                           self.comp_dir,
                           self.comp_name)
            .map(|h| h.rows())
    }

    fn comp_dir(&self) -> Option<&std::ffi::CStr> {
        self.comp_dir
    }
}

struct Program<'input> {
    ranges: Vec<gimli::Range>,
    name: &'input std::ffi::CStr,
    #[allow(dead_code)]
    inlined: bool,
}

impl<'input> Program<'input> {
    fn contains_address(&self, address: u64) -> bool {
        self.ranges.iter().any(|range| address >= range.begin && address < range.end)
    }
}

// https://github.com/Kimundi/owning-ref-rs/issues/18
use std::ops::Deref;
impl<'mmap> Deref for MmapDerived<'mmap> {
    type Target = EndianDebugInfo<'mmap>;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}
