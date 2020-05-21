//! This crate provides a cross-platform library and binary for translating addresses into
//! function names, file names and line numbers. Given an address in an executable or an
//! offset in a section of a relocatable object, it uses the debugging information to
//! figure out which file name and line number are associated with it.
//!
//! When used as a library, files must first be loaded using the
//! [`object`](https://github.com/gimli-rs/object) crate.
//! A context can then be created with [`Context::new`](./struct.Context.html#method.new).
//! The context caches some of the parsed information so that multiple lookups are
//! efficient.
//! Location information is obtained with
//! [`Context::find_location`](./struct.Context.html#method.find_location).
//! Function information is obtained with
//! [`Context::find_frames`](./struct.Context.html#method.find_frames), which returns
//! a frame for each inline function. Each frame contains both name and location.
//!
//! The crate has an example CLI wrapper around the library which provides some of
//! the functionality of the `addr2line` command line tool distributed with [GNU
//! binutils](https://www.gnu.org/software/binutils/).
//!
//! Currently this library only provides information from the DWARF debugging information,
//! which is parsed using [`gimli`](https://github.com/gimli-rs/gimli).  The example CLI
//! wrapper also uses symbol table information provided by the `object` crate.
#![deny(missing_docs)]
#![no_std]

#[allow(unused_imports)]
#[macro_use]
extern crate alloc;

#[cfg(feature = "cpp_demangle")]
extern crate cpp_demangle;
#[cfg(feature = "fallible-iterator")]
pub extern crate fallible_iterator;
pub extern crate gimli;
#[cfg(feature = "object")]
pub extern crate object;
#[cfg(feature = "rustc-demangle")]
extern crate rustc_demangle;

use alloc::borrow::Cow;
use alloc::boxed::Box;
#[cfg(feature = "object")]
use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use core::cmp::Ordering;
use core::iter;
use core::mem;
use core::u64;

use crate::lazy::LazyCell;

#[cfg(feature = "smallvec")]
mod maybe_small {
    pub type Vec<T> = smallvec::SmallVec<[T; 16]>;
    pub type IntoIter<T> = smallvec::IntoIter<[T; 16]>;
}
#[cfg(not(feature = "smallvec"))]
mod maybe_small {
    pub type Vec<T> = alloc::vec::Vec<T>;
    pub type IntoIter<T> = alloc::vec::IntoIter<T>;
}

mod lazy;

type Error = gimli::Error;

/// The state necessary to perform address to line translation.
///
/// Constructing a `Context` is somewhat costly, so users should aim to reuse `Context`s
/// when performing lookups for many addresses in the same executable.
pub struct Context<R>
where
    R: gimli::Reader,
{
    unit_ranges: Vec<UnitRange>,
    units: Vec<ResUnit<R>>,
    sections: gimli::Dwarf<R>,
}

struct UnitRange {
    unit_id: usize,
    max_end: u64,
    range: gimli::Range,
}

/// The type of `Context` that supports the `new` method.
#[cfg(feature = "std-object")]
pub type ObjectContext = Context<gimli::EndianRcSlice<gimli::RunTimeEndian>>;

#[cfg(feature = "std-object")]
impl Context<gimli::EndianRcSlice<gimli::RunTimeEndian>> {
    /// Construct a new `Context`.
    ///
    /// The resulting `Context` uses `gimli::EndianRcSlice<gimli::RunTimeEndian>`.
    /// This means it is not thread safe, has no lifetime constraints (since it copies
    /// the input data), and works for any endianity.
    ///
    /// Performance sensitive applications may want to use `Context::from_sections`
    /// with a more specialised `gimli::Reader` implementation.
    pub fn new<'data, 'file, O: object::Object<'data, 'file>>(
        file: &'file O,
    ) -> Result<Self, Error> {
        let endian = if file.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        fn load_section<'data, 'file, O, S, Endian>(file: &'file O, endian: Endian) -> S
        where
            O: object::Object<'data, 'file>,
            S: gimli::Section<gimli::EndianRcSlice<Endian>>,
            Endian: gimli::Endianity,
        {
            use object::ObjectSection;

            let data = file
                .section_by_name(S::section_name())
                .and_then(|section| section.uncompressed_data().ok())
                .unwrap_or(Cow::Borrowed(&[]));
            S::from(gimli::EndianRcSlice::new(Rc::from(&*data), endian))
        }

        let debug_abbrev: gimli::DebugAbbrev<_> = load_section(file, endian);
        let debug_addr: gimli::DebugAddr<_> = load_section(file, endian);
        let debug_info: gimli::DebugInfo<_> = load_section(file, endian);
        let debug_line: gimli::DebugLine<_> = load_section(file, endian);
        let debug_line_str: gimli::DebugLineStr<_> = load_section(file, endian);
        let debug_ranges: gimli::DebugRanges<_> = load_section(file, endian);
        let debug_rnglists: gimli::DebugRngLists<_> = load_section(file, endian);
        let debug_str: gimli::DebugStr<_> = load_section(file, endian);
        let debug_str_offsets: gimli::DebugStrOffsets<_> = load_section(file, endian);
        let default_section = gimli::EndianRcSlice::new(Rc::from(&[][..]), endian);

        Context::from_sections(
            debug_abbrev,
            debug_addr,
            debug_info,
            debug_line,
            debug_line_str,
            debug_ranges,
            debug_rnglists,
            debug_str,
            debug_str_offsets,
            default_section,
        )
    }
}

impl<R: gimli::Reader> Context<R> {
    /// Construct a new `Context` from DWARF sections.
    pub fn from_sections(
        debug_abbrev: gimli::DebugAbbrev<R>,
        debug_addr: gimli::DebugAddr<R>,
        debug_info: gimli::DebugInfo<R>,
        debug_line: gimli::DebugLine<R>,
        debug_line_str: gimli::DebugLineStr<R>,
        debug_ranges: gimli::DebugRanges<R>,
        debug_rnglists: gimli::DebugRngLists<R>,
        debug_str: gimli::DebugStr<R>,
        debug_str_offsets: gimli::DebugStrOffsets<R>,
        default_section: R,
    ) -> Result<Self, Error> {
        Self::from_dwarf(gimli::Dwarf {
            debug_abbrev,
            debug_addr,
            debug_info,
            debug_line,
            debug_line_str,
            debug_str,
            debug_str_offsets,
            debug_str_sup: default_section.clone().into(),
            debug_types: default_section.clone().into(),
            locations: gimli::LocationLists::new(
                default_section.clone().into(),
                default_section.clone().into(),
            ),
            ranges: gimli::RangeLists::new(debug_ranges, debug_rnglists),
        })
    }

    /// Construct a new `Context` from an existing [`gimli::Dwarf`] object.
    pub fn from_dwarf(sections: gimli::Dwarf<R>) -> Result<Self, Error> {
        let mut unit_ranges = Vec::new();
        let mut res_units = Vec::new();
        let mut units = sections.units();
        while let Some(header) = units.next()? {
            let unit_id = res_units.len();
            let offset = header.offset();
            let dw_unit = match sections.unit(header) {
                Ok(dw_unit) => dw_unit,
                Err(_) => continue,
            };

            let mut lang = None;
            {
                let mut entries = dw_unit.entries_raw(None)?;

                let abbrev = match entries.read_abbreviation()? {
                    Some(abbrev) if abbrev.tag() == gimli::DW_TAG_compile_unit => abbrev,
                    _ => continue, // wtf?
                };

                let mut low_pc = None;
                let mut high_pc = None;
                let mut size = None;
                let mut ranges = None;
                for spec in abbrev.attributes() {
                    let attr = entries.read_attribute(*spec)?;
                    match attr.name() {
                        gimli::DW_AT_low_pc => {
                            if let gimli::AttributeValue::Addr(val) = attr.value() {
                                low_pc = Some(val);
                            }
                        }
                        gimli::DW_AT_high_pc => match attr.value() {
                            gimli::AttributeValue::Addr(val) => high_pc = Some(val),
                            gimli::AttributeValue::Udata(val) => size = Some(val),
                            _ => {}
                        },
                        gimli::DW_AT_ranges => {
                            ranges = sections.attr_ranges_offset(&dw_unit, attr.value())?;
                        }
                        gimli::DW_AT_language => {
                            if let gimli::AttributeValue::Language(val) = attr.value() {
                                lang = Some(val);
                            }
                        }
                        _ => {}
                    }
                }

                let mut add_range = |range: gimli::Range| {
                    if range.begin < range.end {
                        unit_ranges.push(UnitRange {
                            range,
                            unit_id,
                            max_end: 0,
                        });
                    }
                };
                if let Some(offset) = ranges {
                    let mut ranges = sections.ranges(&dw_unit, offset)?;
                    while let Some(range) = ranges.next()? {
                        add_range(range);
                    }
                } else if let (Some(begin), Some(end)) = (low_pc, high_pc) {
                    add_range(gimli::Range { begin, end });
                } else if let (Some(begin), Some(size)) = (low_pc, size) {
                    add_range(gimli::Range {
                        begin,
                        end: begin + size,
                    });
                }
            }

            res_units.push(ResUnit {
                offset,
                dw_unit,
                lang,
                lines: LazyCell::new(),
                funcs: LazyCell::new(),
            });
        }

        // Sort this for faster lookup in `find_unit_id_and_address` below.
        unit_ranges.sort_by_key(|i| i.range.begin);

        // Calculate the `max_end` field now that we've determined the order of
        // CUs.
        let mut max = 0;
        for i in unit_ranges.iter_mut() {
            max = max.max(i.range.end);
            i.max_end = max;
        }

        Ok(Context {
            units: res_units,
            unit_ranges,
            sections,
        })
    }

    /// The dwarf sections associated with this `Context`.
    pub fn dwarf(&self) -> &gimli::Dwarf<R> {
        &self.sections
    }

    // Finds the CU id (index in `self.units`) for the function address given.
    // The index of the address in the CU's address table is also returned
    // because this is calculated here to ensure the function address actually
    // resides in the functions of the CU.
    fn find_unit_id_and_address(&self, probe: u64) -> Option<(usize, usize)> {
        // First up find the position in the array which could have our function
        // address.
        let pos = match self
            .unit_ranges
            .binary_search_by_key(&probe, |i| i.range.begin)
        {
            // Although unlikely, we could find an exact match.
            Ok(i) => i + 1,
            // No exact match was found, but this probe would fit at slot `i`.
            // This means that slot `i` is bigger than `probe`, along with all
            // indices greater than `i`, so we need to search all previous
            // entries.
            Err(i) => i,
        };

        // Once we have our index we iterate backwards from that position
        // looking for a matching CU.
        for i in self.unit_ranges[..pos].iter().rev() {
            // We know that this CU's start is beneath the probe already because
            // of our sorted array.
            debug_assert!(i.range.begin <= probe);

            // Each entry keeps track of the maximum end address seen so far,
            // starting from the beginning of the array of unit ranges. We're
            // iterating in reverse so if our probe is beyond the maximum range
            // of this entry, then it's guaranteed to not fit in any prior
            // entries, so we break out.
            if probe > i.max_end {
                break;
            }

            // If this CU doesn't actually contain this address, move to the
            // next CU.
            if probe > i.range.end {
                continue;
            }

            // There might be multiple CUs whose range contains this address.
            // Weak symbols have shown up in the wild which cause this to happen
            // but otherwise this happened in rust-lang/backtrace-rs#327 too. In
            // any case we assume that might happen, and as a result we need to
            // find a CU which actually contains this function.
            //
            // Consequently we consult the function address table here, and only
            // if there's actually a function in this CU which contains this
            // address do we return this unit.
            let funcs = match self.units[i.unit_id].parse_functions(&self.sections, &self.units) {
                Ok(func) => func,
                Err(_) => continue,
            };
            if let Some(addr) = funcs.find_address(probe) {
                return Some((i.unit_id, addr));
            }
        }

        None
    }

    /// Find the DWARF unit corresponding to the given virtual memory address.
    pub fn find_dwarf_unit(&self, probe: u64) -> Option<&gimli::Unit<R>> {
        self.find_unit_id_and_address(probe)
            .map(|(unit_id, _)| &self.units[unit_id].dw_unit)
    }

    /// Find the source file and line corresponding to the given virtual memory address.
    pub fn find_location(&self, probe: u64) -> Result<Option<Location<'_>>, Error> {
        match self.find_unit_id_and_address(probe) {
            Some((unit_id, _)) => self.units[unit_id].find_location(probe, &self.sections),
            None => Ok(None),
        }
    }

    /// Return an iterator for the function frames corresponding to the given virtual
    /// memory address.
    ///
    /// If the probe address is not for an inline function then only one frame is
    /// returned.
    ///
    /// If the probe address is for an inline function then the first frame corresponds
    /// to the innermost inline function.  Subsequent frames contain the caller and call
    /// location, until an non-inline caller is reached.
    pub fn find_frames(&self, probe: u64) -> Result<FrameIter<R>, Error> {
        let (unit_id, loc, funcs) =
            match self.find_unit_id_and_address(probe) {
                Some((unit_id, address)) => {
                    let unit = &self.units[unit_id];
                    let loc = unit.find_location(probe, &self.sections)?;
                    let functions = unit.parse_functions(&self.sections, &self.units)?;
                    let mut res = maybe_small::Vec::new();
                    let mut function_index = functions.addresses[address].function;
                    loop {
                        let function = &functions.functions[function_index];
                        res.push(function);
                        if let Some(inlined) = function.inlined.iter().find(|inlined| {
                            probe >= inlined.range.begin && probe < inlined.range.end
                        }) {
                            function_index = inlined.function;
                            continue;
                        } else {
                            break;
                        }
                    }
                    (unit_id, loc, res)
                }
                None => (0, None, maybe_small::Vec::new()),
            };

        Ok(FrameIter {
            unit_id,
            units: &self.units,
            sections: &self.sections,
            funcs: funcs.into_iter().rev(),
            next: loc,
        })
    }

    /// Initialize all line data structures. This is used for benchmarks.
    #[doc(hidden)]
    pub fn parse_lines(&self) -> Result<(), Error> {
        for unit in &self.units {
            unit.parse_lines(&self.sections)?;
        }
        Ok(())
    }

    /// Initialize all function data structures. This is used for benchmarks.
    #[doc(hidden)]
    pub fn parse_functions(&self) -> Result<(), Error> {
        for unit in &self.units {
            unit.parse_functions(&self.sections, &self.units)?;
        }
        Ok(())
    }
}

struct Lines {
    files: Box<[String]>,
    sequences: Box<[LineSequence]>,
}

struct LineSequence {
    start: u64,
    end: u64,
    rows: Box<[LineRow]>,
}

struct LineRow {
    address: u64,
    file_index: u64,
    line: u32,
    column: u32,
}

struct ResUnit<R>
where
    R: gimli::Reader,
{
    offset: gimli::DebugInfoOffset<R::Offset>,
    dw_unit: gimli::Unit<R>,
    lang: Option<gimli::DwLang>,
    lines: LazyCell<Result<Lines, Error>>,
    funcs: LazyCell<Result<Functions<R>, Error>>,
}

impl<R> ResUnit<R>
where
    R: gimli::Reader,
{
    fn parse_lines(&self, sections: &gimli::Dwarf<R>) -> Result<Option<&Lines>, Error> {
        let ilnp = match self.dw_unit.line_program {
            Some(ref ilnp) => ilnp,
            None => return Ok(None),
        };
        self.lines
            .borrow_with(|| {
                let mut sequences = Vec::new();
                let mut sequence_rows = Vec::<LineRow>::new();
                let mut rows = ilnp.clone().rows();
                while let Some((_, row)) = rows.next_row()? {
                    if row.end_sequence() {
                        if let Some(start) = sequence_rows.first().map(|x| x.address) {
                            let end = row.address();
                            let mut rows = Vec::new();
                            mem::swap(&mut rows, &mut sequence_rows);
                            if start != 0 {
                                sequences.push(LineSequence {
                                    start,
                                    end,
                                    rows: rows.into_boxed_slice(),
                                });
                            }
                        }
                        continue;
                    }

                    let address = row.address();
                    let file_index = row.file_index();
                    let line = row.line().unwrap_or(0) as u32;
                    let column = match row.column() {
                        gimli::ColumnType::LeftEdge => 0,
                        gimli::ColumnType::Column(x) => x as u32,
                    };

                    if let Some(last_row) = sequence_rows.last_mut() {
                        if last_row.address == address {
                            last_row.file_index = file_index;
                            last_row.line = line;
                            last_row.column = column;
                            continue;
                        }
                    }

                    sequence_rows.push(LineRow {
                        address,
                        file_index,
                        line,
                        column,
                    });
                }
                sequences.sort_by_key(|x| x.start);

                let mut files = Vec::new();
                let mut index = 0;
                let header = ilnp.header();
                while let Some(file) = header.file(index) {
                    files.push(self.render_file(file, header, sections)?);
                    index += 1;
                }

                Ok(Lines {
                    files: files.into_boxed_slice(),
                    sequences: sequences.into_boxed_slice(),
                })
            })
            .as_ref()
            .map(Some)
            .map_err(Error::clone)
    }

    fn parse_functions(
        &self,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
    ) -> Result<&Functions<R>, Error> {
        self.funcs
            .borrow_with(|| Functions::parse(&self.dw_unit, sections, units))
            .as_ref()
            .map_err(Error::clone)
    }

    fn find_location(
        &self,
        probe: u64,
        sections: &gimli::Dwarf<R>,
    ) -> Result<Option<Location<'_>>, Error> {
        let lines = match self.parse_lines(sections)? {
            Some(lines) => lines,
            None => return Ok(None),
        };

        let idx = lines.sequences.binary_search_by(|sequence| {
            if probe < sequence.start {
                Ordering::Greater
            } else if probe >= sequence.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        let idx = match idx {
            Ok(x) => x,
            Err(_) => return Ok(None),
        };
        let sequence = &lines.sequences[idx];

        let idx = sequence
            .rows
            .binary_search_by(|row| row.address.cmp(&probe));
        let idx = match idx {
            Ok(x) => x,
            Err(0) => return Ok(None),
            Err(x) => x - 1,
        };
        let row = &sequence.rows[idx];

        let file = lines.files.get(row.file_index as usize).map(String::as_str);
        Ok(Some(Location {
            file,
            line: if row.line != 0 { Some(row.line) } else { None },
            column: if row.column != 0 {
                Some(row.column)
            } else {
                None
            },
        }))
    }

    fn render_file(
        &self,
        file: &gimli::FileEntry<R, R::Offset>,
        header: &gimli::LineProgramHeader<R, R::Offset>,
        sections: &gimli::Dwarf<R>,
    ) -> Result<String, gimli::Error> {
        let mut path = if let Some(ref comp_dir) = self.dw_unit.comp_dir {
            comp_dir.to_string_lossy()?.into_owned()
        } else {
            String::new()
        };

        if let Some(directory) = file.directory(header) {
            path_push(
                &mut path,
                sections
                    .attr_string(&self.dw_unit, directory)?
                    .to_string_lossy()?
                    .as_ref(),
            );
        }

        path_push(
            &mut path,
            sections
                .attr_string(&self.dw_unit, file.path_name())?
                .to_string_lossy()?
                .as_ref(),
        );

        Ok(path)
    }
}

fn path_push(path: &mut String, p: &str) {
    if p.starts_with('/') {
        *path = p.to_string();
    } else {
        if !path.ends_with('/') {
            path.push('/');
        }
        *path += p;
    }
}

fn name_attr<'abbrev, 'unit, R>(
    attr: gimli::AttributeValue<R>,
    unit: &gimli::Unit<R>,
    sections: &gimli::Dwarf<R>,
    units: &[ResUnit<R>],
    recursion_limit: usize,
) -> Result<Option<R>, Error>
where
    R: gimli::Reader,
{
    if recursion_limit == 0 {
        return Ok(None);
    }

    let mut entries = match attr {
        gimli::AttributeValue::UnitRef(offset) => unit.entries_raw(Some(offset))?,
        gimli::AttributeValue::DebugInfoRef(dr) => {
            let unit = match units.binary_search_by_key(&dr.0, |unit| unit.offset.0) {
                // There is never a DIE at the unit offset or before the first unit.
                Ok(_) | Err(0) => return Err(gimli::Error::NoEntryAtGivenOffset),
                Err(i) => &units[i - 1],
            };
            unit.dw_unit
                .entries_raw(Some(gimli::UnitOffset(dr.0 - unit.offset.0)))?
        }
        _ => return Ok(None),
    };

    let abbrev = if let Some(abbrev) = entries.read_abbreviation()? {
        abbrev
    } else {
        return Err(gimli::Error::NoEntryAtGivenOffset);
    };

    let mut name = None;
    let mut next = None;
    for spec in abbrev.attributes() {
        match entries.read_attribute(*spec) {
            Ok(ref attr) => match attr.name() {
                gimli::DW_AT_linkage_name | gimli::DW_AT_MIPS_linkage_name => {
                    if let Ok(val) = sections.attr_string(unit, attr.value()) {
                        return Ok(Some(val));
                    }
                }
                gimli::DW_AT_name => {
                    if let Ok(val) = sections.attr_string(unit, attr.value()) {
                        name = Some(val);
                    }
                }
                gimli::DW_AT_abstract_origin | gimli::DW_AT_specification => {
                    next = Some(attr.value());
                }
                _ => {}
            },
            Err(e) => return Err(e),
        }
    }

    if name.is_some() {
        return Ok(name);
    }

    if let Some(next) = next {
        return name_attr(next, unit, sections, units, recursion_limit - 1);
    }

    Ok(None)
}

/// A single address range for a function.
///
/// It is possible for a function to have multiple address ranges; this
/// is handled by having multiple `FunctionAddress` entries with the same
/// `function` field.
struct FunctionAddress {
    range: gimli::Range,
    /// An index into `Functions::functions`.
    function: usize,
}

struct Functions<R: gimli::Reader> {
    /// List of all `DW_TAG_subprogram` and `DW_TAG_inlined_subroutine` details.
    functions: Box<[Function<R>]>,
    /// List of `DW_TAG_subprogram` address ranges in the unit.
    addresses: Box<[FunctionAddress]>,
}

struct Function<R: gimli::Reader> {
    dw_die_offset: gimli::UnitOffset<R::Offset>,
    name: Option<R>,
    call_file: u64,
    call_line: u32,
    call_column: u32,
    /// List of `DW_TAG_inlined_subroutine` address ranges in this function.
    // TODO: this is often empty, so we could save more memory by storing the
    // length in the allocated memory.
    inlined: Box<[FunctionAddress]>,
}

impl<R: gimli::Reader> Functions<R> {
    fn parse(
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
    ) -> Result<Functions<R>, Error> {
        let mut functions = Vec::new();
        let mut addresses = Vec::new();
        // These are ignored.
        let mut inlined = Vec::new();
        let mut entries = unit.entries_raw(None)?;
        while !entries.is_empty() {
            let dw_die_offset = entries.next_offset();
            let depth = entries.next_depth();
            if let Some(abbrev) = entries.read_abbreviation()? {
                if abbrev.tag() == gimli::DW_TAG_subprogram {
                    Function::parse(
                        dw_die_offset,
                        &mut entries,
                        abbrev,
                        depth,
                        unit,
                        sections,
                        units,
                        &mut functions,
                        &mut addresses,
                        &mut inlined,
                    )?;
                } else {
                    for spec in abbrev.attributes() {
                        match entries.read_attribute(*spec) {
                            Ok(_) => {}
                            Err(e) => return Err(e),
                        }
                    }
                }
            }
        }

        // The binary search requires the addresses to be sorted.
        //
        // It also requires them to be non-overlapping.  In practice, overlapping
        // function ranges are unlikely, so we don't try to handle that yet.
        //
        // It's possible for multiple functions to have the same address range if the
        // compiler can detect and remove functions with identical code.  In that case
        // we'll nondeterministically return one of them.
        addresses.sort_by_key(|x| x.range.begin);

        Ok(Functions {
            functions: functions.into_boxed_slice(),
            addresses: addresses.into_boxed_slice(),
        })
    }

    fn find_address(&self, probe: u64) -> Option<usize> {
        self.addresses
            .binary_search_by(|address| {
                if probe < address.range.begin {
                    Ordering::Greater
                } else if probe >= address.range.end {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .ok()
    }
}

impl<R: gimli::Reader> Function<R> {
    fn parse(
        dw_die_offset: gimli::UnitOffset<R::Offset>,
        entries: &mut gimli::EntriesRaw<R>,
        abbrev: &gimli::Abbreviation,
        depth: isize,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
        functions: &mut Vec<Function<R>>,
        addresses: &mut Vec<FunctionAddress>,
        inlined: &mut Vec<FunctionAddress>,
    ) -> Result<(), Error> {
        let mut low_pc = None;
        let mut high_pc = None;
        let mut size = None;
        let mut ranges = None;
        let mut name = None;
        let mut call_file = 0;
        let mut call_line = 0;
        let mut call_column = 0;
        for spec in abbrev.attributes() {
            match entries.read_attribute(*spec) {
                Ok(ref attr) => {
                    match attr.name() {
                        gimli::DW_AT_low_pc => {
                            if let gimli::AttributeValue::Addr(val) = attr.value() {
                                low_pc = Some(val);
                            }
                        }
                        gimli::DW_AT_high_pc => match attr.value() {
                            gimli::AttributeValue::Addr(val) => high_pc = Some(val),
                            gimli::AttributeValue::Udata(val) => size = Some(val),
                            _ => {}
                        },
                        gimli::DW_AT_ranges => {
                            ranges = sections.attr_ranges_offset(unit, attr.value())?;
                        }
                        gimli::DW_AT_linkage_name | gimli::DW_AT_MIPS_linkage_name => {
                            if let Ok(val) = sections.attr_string(unit, attr.value()) {
                                name = Some(val);
                            }
                        }
                        gimli::DW_AT_name => {
                            if name.is_none() {
                                name = sections.attr_string(unit, attr.value()).ok();
                            }
                        }
                        gimli::DW_AT_abstract_origin | gimli::DW_AT_specification => {
                            if name.is_none() {
                                name = name_attr(attr.value(), unit, sections, units, 16)?;
                            }
                        }
                        _ => {}
                    };
                    if abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
                        match attr.name() {
                            gimli::DW_AT_call_file => {
                                if let gimli::AttributeValue::FileIndex(fi) = attr.value() {
                                    call_file = fi;
                                }
                            }
                            gimli::DW_AT_call_line => {
                                call_line = attr.udata_value().unwrap_or(0) as u32;
                            }
                            gimli::DW_AT_call_column => {
                                call_column = attr.udata_value().unwrap_or(0) as u32;
                            }
                            _ => {}
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }

        let mut local_inlined = Vec::new();
        loop {
            let dw_die_offset = entries.next_offset();
            let next_depth = entries.next_depth();
            if next_depth <= depth {
                break;
            }
            if let Some(abbrev) = entries.read_abbreviation()? {
                if abbrev.tag() == gimli::DW_TAG_subprogram
                    || abbrev.tag() == gimli::DW_TAG_inlined_subroutine
                {
                    Function::parse(
                        dw_die_offset,
                        entries,
                        abbrev,
                        next_depth,
                        unit,
                        sections,
                        units,
                        functions,
                        addresses,
                        &mut local_inlined,
                    )?;
                } else {
                    for spec in abbrev.attributes() {
                        match entries.read_attribute(*spec) {
                            Ok(_) => {}
                            Err(e) => return Err(e),
                        }
                    }
                }
            }
        }

        let function_index = functions.len();
        functions.push(Function {
            dw_die_offset,
            name,
            call_file,
            call_line,
            call_column,
            inlined: local_inlined.into_boxed_slice(),
        });

        let addresses = if abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
            inlined
        } else {
            addresses
        };
        let mut add_range = |range: gimli::Range| {
            // Ignore invalid DWARF so that a query of 0 does not give
            // a long list of matches.
            // TODO: don't ignore if there is a section at this address
            if range.begin != 0 && range.begin < range.end {
                addresses.push(FunctionAddress {
                    range,
                    function: function_index,
                });
            }
        };
        if let Some(offset) = ranges {
            let mut ranges = sections.ranges(unit, offset)?;
            while let Some(range) = ranges.next()? {
                add_range(range);
            }
        } else if let (Some(begin), Some(end)) = (low_pc, high_pc) {
            add_range(gimli::Range { begin, end });
        } else if let (Some(begin), Some(size)) = (low_pc, size) {
            add_range(gimli::Range {
                begin,
                end: begin + size,
            });
        }

        Ok(())
    }
}

/// An iterator over function frames.
pub struct FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    unit_id: usize,
    units: &'ctx Vec<ResUnit<R>>,
    sections: &'ctx gimli::Dwarf<R>,
    funcs: iter::Rev<maybe_small::IntoIter<&'ctx Function<R>>>,
    next: Option<Location<'ctx>>,
}

impl<'ctx, R> FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    /// Advances the iterator and returns the next frame.
    pub fn next(&mut self) -> Result<Option<Frame<'ctx, R>>, Error> {
        let (loc, func) = match (self.next.take(), self.funcs.next()) {
            (None, None) => return Ok(None),
            (loc, Some(func)) => (loc, func),
            (Some(loc), None) => {
                return Ok(Some(Frame {
                    dw_die_offset: None,
                    function: None,
                    location: Some(loc),
                }))
            }
        };

        let unit = &self.units[self.unit_id];

        if self.funcs.len() != 0 {
            let mut next = Location {
                file: None,
                line: if func.call_line != 0 {
                    Some(func.call_line)
                } else {
                    None
                },
                column: if func.call_column != 0 {
                    Some(func.call_column)
                } else {
                    None
                },
            };
            if func.call_file != 0 {
                if let Some(lines) = unit.parse_lines(self.sections)? {
                    next.file = lines.files.get(func.call_file as usize).map(String::as_str);
                }
            }

            self.next = Some(next);
        }

        Ok(Some(Frame {
            dw_die_offset: Some(func.dw_die_offset),
            function: func.name.clone().map(|name| FunctionName {
                name,
                language: unit.lang,
            }),
            location: loc,
        }))
    }
}

#[cfg(feature = "fallible-iterator")]
impl<'ctx, R> fallible_iterator::FallibleIterator for FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    type Item = Frame<'ctx, R>;
    type Error = Error;

    #[inline]
    fn next(&mut self) -> Result<Option<Frame<'ctx, R>>, Error> {
        self.next()
    }
}

/// A function frame.
pub struct Frame<'ctx, R: gimli::Reader> {
    /// The DWARF unit offset corresponding to the DIE of the function.
    pub dw_die_offset: Option<gimli::UnitOffset<R::Offset>>,
    /// The name of the function.
    pub function: Option<FunctionName<R>>,
    /// The source location corresponding to this frame.
    pub location: Option<Location<'ctx>>,
}

/// A function name.
pub struct FunctionName<R: gimli::Reader> {
    /// The name of the function.
    pub name: R,
    /// The language of the compilation unit containing this function.
    pub language: Option<gimli::DwLang>,
}

impl<R: gimli::Reader> FunctionName<R> {
    /// The raw name of this function before demangling.
    pub fn raw_name(&self) -> Result<Cow<str>, Error> {
        self.name.to_string_lossy()
    }

    /// The name of this function after demangling (if applicable).
    pub fn demangle(&self) -> Result<Cow<str>, Error> {
        self.raw_name().map(|x| demangle_auto(x, self.language))
    }
}

/// Demangle a symbol name using the demangling scheme for the given language.
///
/// Returns `None` if demangling failed or is not required.
#[allow(unused_variables)]
pub fn demangle(name: &str, language: gimli::DwLang) -> Option<String> {
    match language {
        #[cfg(feature = "rustc-demangle")]
        gimli::DW_LANG_Rust => rustc_demangle::try_demangle(name)
            .ok()
            .as_ref()
            .map(|x| format!("{:#}", x)),
        #[cfg(feature = "cpp_demangle")]
        gimli::DW_LANG_C_plus_plus
        | gimli::DW_LANG_C_plus_plus_03
        | gimli::DW_LANG_C_plus_plus_11
        | gimli::DW_LANG_C_plus_plus_14 => cpp_demangle::Symbol::new(name)
            .ok()
            .and_then(|x| x.demangle(&Default::default()).ok()),
        _ => None,
    }
}

/// Apply 'best effort' demangling of a symbol name.
///
/// If `language` is given, then only the demangling scheme for that language
/// is used.
///
/// If `language` is `None`, then heuristics are used to determine how to
/// demangle the name. Currently, these heuristics are very basic.
///
/// If demangling fails or is not required, then `name` is returned unchanged.
pub fn demangle_auto(name: Cow<str>, language: Option<gimli::DwLang>) -> Cow<str> {
    match language {
        Some(language) => demangle(name.as_ref(), language),
        None => demangle(name.as_ref(), gimli::DW_LANG_Rust)
            .or_else(|| demangle(name.as_ref(), gimli::DW_LANG_C_plus_plus)),
    }
    .map(Cow::from)
    .unwrap_or(name)
}

/// A source location.
pub struct Location<'a> {
    /// The file name.
    pub file: Option<&'a str>,
    /// The line number.
    pub line: Option<u32>,
    /// The column number.
    pub column: Option<u32>,
}
