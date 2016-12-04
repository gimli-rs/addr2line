//! This crate provides a cross-platform library and binary for translating addresses into file
//! names and line numbers. Given an address in an executable or an offset in a section of a
//! relocatable object, it uses the debugging information to figure out which file name and line
//! number are associated with it.
//!
//! When used as a library, executables are loaded using `Mapping::new`, and users can the use
//! `Mapping::locate` to find the corresponding file path and line number. `addr2line` avoids
//! re-executing work where it can so that looking up multiple addresses for a single file does not
//! require re-parsing the executable's debug symbols. The library makes heavy use of
//! [gimli](https://github.com/gimli-rs/gimli), which provides zero-copy access to the DWARF
//! debugging format used by most modern compilers.
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

use std::path::Path;

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
    units: Vec<Unit<'object>>,
}

impl Mapping {
    /// Construct a new `Mapping` from the debug symbols in the given executable.
    ///
    /// The target file will be memmap'd, and then `gimli` is used to parse out the necessary debug
    /// symbols, without copying data when possible.
    pub fn new(file_path: &Path) -> MappingResult<Mapping> {
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
                        Box::new(Self::symbolicate(file)
                            .expect("not all necessary debug symbols are present"))
                    }),
                })
            }),
        })
    }

    /// Locate the source file and line corresponding to the given virtual memory address.
    pub fn locate(&self, addr: u64) -> Option<(String, u64)> {
        self.inner.locate(addr)
    }

    fn symbolicate<'a>(file: &'a object::File) -> MappingResult<EndianDebugInfo<'a>> {
        if file.is_little_endian() {
            Ok(EndianDebugInfo::LEInfo(DebugInfo::new(file)?))
        } else {
            Ok(EndianDebugInfo::BEInfo(DebugInfo::new(file)?))
        }
    }
}

impl<'object> EndianDebugInfo<'object> {
    fn locate(&self, addr: u64) -> Option<(String, u64)> {
        match *self {
            EndianDebugInfo::LEInfo(ref dbg) => dbg.locate(addr),
            EndianDebugInfo::BEInfo(ref dbg) => dbg.locate(addr),
        }
    }
}

impl<'object, Endian> DebugInfo<'object, Endian>
    where Endian: gimli::Endianity
{
    fn new<'a>(file: &'a object::File) -> MappingResult<DebugInfo<'a, Endian>> {
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
            if let Some(unit) = Unit::parse(&debug_abbrev, &debug_ranges, &debug_str, &header) {
                units.push(unit);
            }
        }

        Ok(DebugInfo {
            debug_line: debug_line,
            units: units,
        })
    }

    pub fn locate(&self, addr: u64) -> Option<(String, u64)> {
        let mut current: Option<gimli::LineNumberRow> = None;
        for unit in &self.units {
            if !unit.contains_address(addr) {
                continue;
            }

            let rows = unit.line_rows(&self.debug_line);
            if rows.is_err() {
                continue;
            }

            let mut rows = rows.unwrap();
            while let Ok(Some((header, row))) = rows.next_row() {
                if row.address() > addr {
                    if let Some(ref row) = current {
                        let mut path = String::new();
                        let file = row.file(header).unwrap();
                        if let Some(directory) = file.directory(header) {
                            let directory = directory.to_string_lossy();
                            if !directory.starts_with("/") {
                                if let Some(comp_dir) = unit.comp_dir() {
                                    path.push_str(&*comp_dir.to_string_lossy());
                                    path.push_str("/");
                                }
                            }
                            path.push_str(&*directory);
                            path.push_str("/");
                        }
                        path.push_str(&*file.path_name().to_string_lossy());

                        return Some((path, row.line().unwrap()));
                    }
                    break;
                }
                if row.end_sequence() {
                    current = None;
                } else {
                    current = Some(row.clone());
                }
            }
        }
        None
    }
}

// TODO: most of this should be moved to the main library.
struct Unit<'input> {
    address_size: u8,
    ranges: Vec<gimli::Range>,
    line_offset: gimli::DebugLineOffset,
    comp_dir: Option<&'input std::ffi::CStr>,
    comp_name: Option<&'input std::ffi::CStr>,
}

impl<'input> Unit<'input> {
    fn parse<Endian>(debug_abbrev: &gimli::DebugAbbrev<Endian>,
                     debug_ranges: &gimli::DebugRanges<Endian>,
                     debug_str: &gimli::DebugStr<'input, Endian>,
                     header: &gimli::CompilationUnitHeader<'input, Endian>)
                     -> Option<Unit<'input>>
        where Endian: gimli::Endianity
    {
        let abbrev = header.abbreviations(*debug_abbrev).expect("Fail");
        let mut entries = header.entries(&abbrev);
        let (_, entry) = entries.next_dfs()
            .expect("Should parse first entry OK")
            .expect("And first entry should exist!");
        assert_eq!(entry.tag(), gimli::DW_TAG_compile_unit);

        let ranges = if let Some(ranges) =
            Self::parse_noncontiguous_ranges(entry, debug_ranges, header.address_size()) {
            ranges
        } else if let Some(range) = Self::parse_contiguous_range(entry) {
            vec![range]
        } else {
            return None;
        };

        let line_offset = match entry.attr_value(gimli::DW_AT_stmt_list) {
            Some(gimli::AttributeValue::DebugLineRef(offset)) => offset,
            _ => return None,
        };
        let comp_dir = entry.attr(gimli::DW_AT_comp_dir)
            .and_then(|attr| attr.string_value(debug_str));
        let comp_name = entry.attr(gimli::DW_AT_name)
            .and_then(|attr| attr.string_value(debug_str));

        Some(Unit {
            address_size: header.address_size(),
            ranges: ranges,
            line_offset: line_offset,
            comp_dir: comp_dir,
            comp_name: comp_name,
        })
    }

    // This must be checked before `parse_contiguous_range`.
    fn parse_noncontiguous_ranges<Endian>(entry: &gimli::DebuggingInformationEntry<Endian>,
                                          debug_ranges: &gimli::DebugRanges<Endian>,
                                          address_size: u8)
                                          -> Option<Vec<gimli::Range>>
        where Endian: gimli::Endianity
    {
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

    fn parse_contiguous_range<Endian>(entry: &gimli::DebuggingInformationEntry<Endian>)
                                      -> Option<gimli::Range>
        where Endian: gimli::Endianity
    {
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

    fn line_rows<Endian>(&self,
                         debug_line: &gimli::DebugLine<'input, Endian>)
                         -> gimli::Result<gimli::StateMachine<'input, Endian>>
        where Endian: gimli::Endianity
    {
        let header = try!(debug_line.header(self.line_offset,
                                            self.address_size,
                                            self.comp_dir,
                                            self.comp_name));
        Ok(header.rows())
    }

    fn comp_dir(&self) -> Option<&std::ffi::CStr> {
        self.comp_dir
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
