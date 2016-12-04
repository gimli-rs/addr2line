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

use object::Object;
use owning_ref::OwningHandle;
use fallible_iterator::FallibleIterator;

use std::path;
use std::borrow::Cow;

pub mod error;
pub use error::*;

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
}

impl Mapping {
    /// Construct a new `Mapping` from the debug symbols in the given executable.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out the necessary debug
    /// symbols, without copying data when possible.
    ///
    /// The returned `Mapping` will also include debug information for functions, enabling
    /// `Mapping::locate()` to also indicate what function a given address appears in. This comes
    /// at some parsing and lookup cost.
    pub fn with_functions(file_path: &path::Path) -> MappingResult<Mapping> {
        Self::new_inner(file_path, true)
    }

    /// Construct a new `Mapping` from the debug symbols in the given executable.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out the necessary debug
    /// symbols, without copying data when possible.
    ///
    /// Note that this constructor will *not* include function mapping information, and thus
    /// `Mapping::locate()` will always return `None` for the function mapping of addresses.
    pub fn new(file_path: &path::Path) -> MappingResult<Mapping> {
        Self::new_inner(file_path, false)
    }

    fn new_inner(file_path: &path::Path, with_functions: bool) -> MappingResult<Mapping> {
        let file = memmap::Mmap::open_path(file_path, memmap::Protection::Read)
            .map_err(|e| MappingError::BadPath(e))?;

        Ok(Mapping {
            inner: OwningHandle::new(Box::new(file), |mmap| {
                use std::mem;
                let mmap: &memmap::Mmap = unsafe { mem::transmute(mmap) };
                let file = object::File::parse(unsafe { mmap.as_slice() });
                Box::new(MmapDerived {
                    inner: OwningHandle::new(Box::new(file), |file| {
                        let file: &object::File = unsafe { mem::transmute(file) };

                        // unwrap here is currently necessary
                        // awaiting https://github.com/Kimundi/owning-ref-rs/issues/19
                        Box::new(Self::symbolicate(file, with_functions)
                            .expect("not all necessary debug symbols are present"))
                    }),
                })
            }),
        })
    }

    /// Locate the source file and line corresponding to the given virtual memory address.
    ///
    /// If the `Mapping` was constructed with `with_functions`, information about the containing
    /// function may also be returned when available.
    pub fn locate(&self, addr: u64) -> Option<(path::PathBuf, u64, Option<Cow<str>>)> {
        self.inner.locate(addr)
    }

    fn symbolicate<'a>(file: &'a object::File,
                       with_functions: bool)
                       -> MappingResult<EndianDebugInfo<'a>> {
        if file.is_little_endian() {
            Ok(EndianDebugInfo::LEInfo(DebugInfo::new(file, with_functions)?))
        } else {
            Ok(EndianDebugInfo::BEInfo(DebugInfo::new(file, with_functions)?))
        }
    }
}

impl<'object> EndianDebugInfo<'object> {
    fn locate(&self, addr: u64) -> Option<(path::PathBuf, u64, Option<Cow<str>>)> {
        match *self {
            EndianDebugInfo::LEInfo(ref dbg) => dbg.locate(addr),
            EndianDebugInfo::BEInfo(ref dbg) => dbg.locate(addr),
        }
    }
}

impl<'object, Endian> DebugInfo<'object, Endian>
    where Endian: gimli::Endianity
{
    fn new<'a>(file: &'a object::File,
               with_functions: bool)
               -> MappingResult<DebugInfo<'a, Endian>> {
        let debug_info = file.get_section(".debug_info")
            .ok_or(MappingError::MissingDebugInfo(".debug_info"))?;
        let debug_info = gimli::DebugInfo::<Endian>::new(debug_info);
        let debug_abbrev = file.get_section(".debug_abbrev")
            .ok_or(MappingError::MissingDebugInfo(".debug_abbrev"))?;
        let debug_abbrev = gimli::DebugAbbrev::<Endian>::new(debug_abbrev);
        let debug_line = file.get_section(".debug_line")
            .ok_or(MappingError::MissingDebugInfo(".debug_line"))?;
        let debug_line = gimli::DebugLine::<Endian>::new(debug_line);
        let debug_ranges = file.get_section(".debug_ranges").unwrap_or(&[]);
        let debug_ranges = gimli::DebugRanges::<Endian>::new(debug_ranges);
        let debug_str = file.get_section(".debug_str").unwrap_or(&[]);
        let debug_str = gimli::DebugStr::<Endian>::new(debug_str);

        let mut units = Vec::new();
        let mut headers = debug_info.units();
        while let Some(header) = headers.next()
            .map_err(|_| MappingError::MissingDebugInfo("couldn't get DIE header"))? {
            let unit = Unit::parse(&debug_abbrev,
                                   &debug_ranges,
                                   &debug_str,
                                   &header,
                                   with_functions);
            if let Some(unit) = unit {
                units.push(unit);
            }
        }

        Ok(DebugInfo {
            debug_line: debug_line,
            units: units,
        })
    }

    pub fn locate(&self, addr: u64) -> Option<(path::PathBuf, u64, Option<Cow<str>>)> {
        // First, find the compilation unit for the given address
        for unit in &self.units {
            if !unit.contains_address(addr) {
                continue;
            }

            // Okay, this is the right unit. Check our DebugLine rows.
            let rows = unit.line_rows(&self.debug_line);
            if rows.is_err() {
                return None;
            }
            let mut rows = rows.unwrap();

            // Now, find the last row before a row with a higher address than the one we seek.
            let mut current = None;
            while let Ok(Some((_, row))) = rows.next_row() {
                if row.address() <= addr {
                    if row.end_sequence() {
                        current = None;
                    } else {
                        // Might be the right row, but we won't know until we see the next one.
                        // The .clone is needed so we can keep iterating
                        current = Some(row.clone());
                    }
                    continue;
                }
                break;
            }

            // The row we just last iterated to is *after* the address, to the previous row we
            // saw (stored in current) is the one we want. There better be one...
            let row = current.unwrap();
            let header = rows.header();

            let mut path = path::PathBuf::new();
            let file = row.file(&header).unwrap();
            if let Some(directory) = file.directory(&header) {
                let directory = directory.to_string_lossy();
                if !directory.starts_with("/") {
                    if let Some(comp_dir) = unit.comp_dir() {
                        path.push(&*comp_dir.to_string_lossy());
                    }
                }
                path.push(&*directory);
            }
            path.push(&*file.path_name().to_string_lossy());

            let line = row.line().unwrap();
            if unit.programs.is_empty() {
                return Some((path, line, None));
            }

            // The unit also has programs, so let's look for the function wrapping this address.
            let mut func: Option<(&Program, &gimli::Range, u64)> = None;
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
                    .unwrap();

                if let Some((prev, prange, pdist)) = func.take() {
                    // are we a better match?
                    func = if dist == pdist {
                        // we're equally good -- are we tighter?
                        if range.end < prange.end {
                            // yes!
                            Some((p, range, dist))
                        } else if range.end == prange.end {
                            // We found two equally good ranges for this address.
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

            return Some((path, line, func.map(|u| u.0.name.to_string_lossy())));
        }
        None
    }
}

// TODO: most of this should be moved to the main library.
use std::marker::PhantomData;
struct Unit<'input, Endian> {
    address_size: u8,
    ranges: Vec<gimli::Range>,
    line_offset: gimli::DebugLineOffset,
    comp_dir: Option<&'input std::ffi::CStr>,
    comp_name: Option<&'input std::ffi::CStr>,
    programs: Vec<Program<'input>>,
    phantom: PhantomData<Endian>,
}

impl<'input, Endian> Unit<'input, Endian>
    where Endian: gimli::Endianity
{
    fn parse(debug_abbrev: &gimli::DebugAbbrev<Endian>,
             debug_ranges: &gimli::DebugRanges<Endian>,
             debug_str: &gimli::DebugStr<'input, Endian>,
             header: &gimli::CompilationUnitHeader<'input, Endian>,
             with_functions: bool)
             -> Option<Unit<'input, Endian>> {

        // We first want to parse out the compilation unit, and then any contained subprograms.
        let abbrev = header.abbreviations(*debug_abbrev).expect("Fail");
        let mut entries = header.entries(&abbrev);
        let mut unit = {
            // Scoped so that we can continue using entries for the loop below
            let first = entries.next_dfs();
            let (_, entry) = first.expect("Should parse first entry OK")
                .expect("And first entry should exist!");
            assert_eq!(entry.tag(), gimli::DW_TAG_compile_unit);

            // Where does our compilation unit live?
            let ranges = Self::get_ranges(entry, debug_ranges, header.address_size());
            if ranges.is_empty() {
                return None;
            }

            // Extract source file and line information about the compilation unit
            let line_offset = match entry.attr_value(gimli::DW_AT_stmt_list) {
                Some(gimli::AttributeValue::DebugLineRef(offset)) => offset,
                _ => return None,
            };
            let comp_dir = entry.attr(gimli::DW_AT_comp_dir)
                .and_then(|attr| attr.string_value(debug_str));
            let comp_name = entry.attr(gimli::DW_AT_name)
                .and_then(|attr| attr.string_value(debug_str));

            Unit {
                address_size: header.address_size(),
                ranges: ranges,
                line_offset: line_offset,
                comp_dir: comp_dir,
                comp_name: comp_name,
                programs: vec![],
                phantom: PhantomData,
            }
        };

        // Do we also need to extract function information?
        if !with_functions {
            return Some(unit);
        }

        while let Ok(Some((_, entry))) = entries.next_dfs() {
            // We only care about functions
            match entry.tag() {
                gimli::DW_TAG_inlined_subroutine |
                gimli::DW_TAG_subprogram => (),
                _ => continue,
            }

            // Where does this function live?
            let ranges = Self::get_ranges(entry, debug_ranges, header.address_size());
            if ranges.is_empty() {
                continue;
            }

            if cfg!(debug_assertions) {
                // All programs should be fully contained within their compilation unit
                for r in &ranges {
                    assert!(unit.ranges
                        .iter()
                        .any(|range| range.begin <= r.begin && range.end >= r.end));
                }
            }

            unit.programs.push(Program {
                ranges: ranges,
                inlined: entry.tag() == gimli::DW_TAG_inlined_subroutine,
                name: Self::resolve_name(entry, header, debug_str, &abbrev)
                    .expect(&format!("subprogram {:x} has neither (linkage_,)name, nor abstract \
                                     origin",
                                     entry.offset().0)),
            });
        }

        Some(unit)
    }

    fn resolve_name<'a, 'b>(entry: &gimli::DebuggingInformationEntry<'input, 'a, 'b, Endian>,
                            header: &gimli::CompilationUnitHeader<'input, Endian>,
                            debug_str: &gimli::DebugStr<'input, Endian>,
                            abbrev: &gimli::Abbreviations)
                            -> Option<&'input std::ffi::CStr> {

        // For naming, we prefer the linked name, if available
        if let Some(name) = entry.attr(gimli::DW_AT_linkage_name)
            .and_then(|attr| attr.string_value(debug_str)) {
            return Some(name);
        }

        // Linked name is not available, so fall back to just plain old name, if that's available.
        if let Some(name) = entry.attr(gimli::DW_AT_name)
            .and_then(|attr| attr.string_value(debug_str)) {
            return Some(name);
        }

        // If we don't have the link name, check if this function refers to another
        if let Some(gimli::AttributeValue::UnitRef(origin)) =
            entry.attr_value(gimli::DW_AT_abstract_origin) {
            let mut entries = header.entries_at_offset(abbrev, origin).unwrap();
            if let Ok(Some((_, parent))) = entries.next_dfs() {
                return Some(Self::resolve_name(parent, header, debug_str, abbrev)
                    .expect(&format!("failed to resolve subprogram origin {:x}", origin.0)));
            } else {
                panic!("unit has no entry at offset {:x} as required by {:x}",
                       origin.0,
                       entry.offset().0);
            }
        }

        // This really shouldn't happen
        None
    }

    fn get_ranges(entry: &gimli::DebuggingInformationEntry<Endian>,
                  debug_ranges: &gimli::DebugRanges<Endian>,
                  address_size: u8)
                  -> Vec<gimli::Range> {
        Self::parse_noncontiguous_ranges(entry, debug_ranges, address_size)
            .or_else(|| Self::parse_contiguous_range(entry).map(|range| vec![range]))
            .unwrap_or_else(Vec::new)
    }

    // This must be checked before `parse_contiguous_range`.
    fn parse_noncontiguous_ranges(entry: &gimli::DebuggingInformationEntry<Endian>,
                                  debug_ranges: &gimli::DebugRanges<Endian>,
                                  address_size: u8)
                                  -> Option<Vec<gimli::Range>> {
        let offset = match entry.attr_value(gimli::DW_AT_ranges) {
            Some(gimli::AttributeValue::DebugRangesRef(offset)) => offset,
            _ => return None,
        };
        let base_address = match entry.attr_value(gimli::DW_AT_low_pc) {
            Some(gimli::AttributeValue::Addr(addr)) => addr,
            _ => 0,
        };
        let ranges = debug_ranges.ranges(offset, address_size, base_address)
            .expect("Range offset should be valid");
        Some(ranges.collect().expect("Should parse ranges"))
    }

    fn parse_contiguous_range(entry: &gimli::DebuggingInformationEntry<Endian>)
                              -> Option<gimli::Range> {
        debug_assert!(entry.attr_value(gimli::DW_AT_ranges).is_none());

        let low_pc = match entry.attr_value(gimli::DW_AT_low_pc) {
            Some(gimli::AttributeValue::Addr(addr)) => addr,
            _ => return None,
        };

        let high_pc = match entry.attr_value(gimli::DW_AT_high_pc) {
            Some(gimli::AttributeValue::Addr(addr)) => addr,
            Some(gimli::AttributeValue::Udata(size)) => low_pc.wrapping_add(size),
            None => low_pc.wrapping_add(1),
            _ => return None,
        };

        if low_pc == 0 {
            // https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blob;f=gdb/dwarf2read.c;h=ed10e03812f381ccdb5c51e1c689df8d61ab87f6;hb=HEAD#l16000
            // TODO: *technically* there could be a relocatable section placed at VA 0
            return None;
        }

        if low_pc == high_pc {
            // https://sourceware.org/ml/gdb-patches/2011-03/msg00739.html
            return None;
        }

        // TODO: convert to error
        assert!(low_pc < high_pc);
        Some(gimli::Range {
            begin: low_pc,
            end: high_pc,
        })
    }

    fn contains_address(&self, address: u64) -> bool {
        self.ranges.iter().any(|range| address >= range.begin && address < range.end)
    }

    fn line_rows(&self,
                 debug_line: &gimli::DebugLine<'input, Endian>)
                 -> gimli::Result<gimli::StateMachine<'input, Endian>> {
        debug_line.header(self.line_offset,
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
