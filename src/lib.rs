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
#![cfg_attr(not(feature = "std"), feature(alloc))]

#[cfg(feature = "std")]
#[macro_use]
extern crate std;

#[cfg(not(feature = "std"))]
extern crate alloc;
#[cfg(not(feature = "std"))]
extern crate core as std;

#[cfg(feature = "cpp_demangle")]
extern crate cpp_demangle;
pub extern crate fallible_iterator;
pub extern crate gimli;
extern crate lazycell;
#[cfg(feature = "object")]
pub extern crate object;
#[cfg(feature = "rustc-demangle")]
extern crate rustc_demangle;
extern crate smallvec;

#[cfg(feature = "std")]
mod alloc {
    pub use std::{borrow, boxed, rc, string, vec};
}

use alloc::borrow::Cow;
use alloc::boxed::Box;
#[cfg(feature = "object")]
use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use std::cmp::Ordering;
use std::iter;
use std::mem;
use std::u64;

use fallible_iterator::FallibleIterator;
use lazycell::LazyCell;
use smallvec::SmallVec;

type Error = gimli::Error;

/// The state necessary to perform address to line translation.
///
/// Constructing a `Context` is somewhat costly, so users should aim to reuse `Context`s
/// when performing lookups for many addresses in the same executable.
pub struct Context<R = gimli::EndianRcSlice<gimli::RunTimeEndian>>
where
    R: gimli::Reader,
{
    unit_ranges: Vec<(gimli::Range, usize)>,
    units: Vec<ResUnit<R>>,
    sections: gimli::Dwarf<R>,
}

impl Context<gimli::EndianRcSlice<gimli::RunTimeEndian>> {
    /// Construct a new `Context`.
    ///
    /// The resulting `Context` uses `gimli::EndianRcSlice<gimli::RunTimeEndian>`.
    /// This means it is not thread safe, has no lifetime constraints (since it copies
    /// the input data), and works for any endianity.
    ///
    /// Performance sensitive applications may want to use `Context::from_sections`
    /// with a more specialised `gimli::Reader` implementation.
    #[cfg(feature = "object")]
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
            let data = file
                .section_data_by_name(S::section_name())
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
                        unit_ranges.push((range, unit_id));
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
                dw_unit,
                lang,
                lines: LazyCell::new(),
                funcs: LazyCell::new(),
            });
        }

        unit_ranges.sort_by_key(|x| x.0.begin);

        // Ranges need to be disjoint so that we can binary search, but weak symbols can
        // cause overlap. In this case, we don't care which unit is used, so ignore the
        // beginning of the subseqent range to avoid overlap.
        let mut prev_end = 0;
        for range in &mut unit_ranges {
            if range.0.begin < prev_end {
                range.0.begin = prev_end;
            }
            if range.0.end < prev_end {
                range.0.end = prev_end;
            } else {
                prev_end = range.0.end;
            }
        }
        debug_assert!(unit_ranges.windows(2).all(|w| w[0].0.end <= w[1].0.begin));

        Ok(Context {
            units: res_units,
            unit_ranges,
            sections,
        })
    }

    fn find_unit(&self, probe: u64) -> Option<usize> {
        let idx = self.unit_ranges.binary_search_by(|r| {
            if probe < r.0.begin {
                Ordering::Greater
            } else if probe >= r.0.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        let idx = match idx {
            Ok(x) => x,
            Err(_) => return None,
        };

        let (_, unit_id) = self.unit_ranges[idx];
        Some(unit_id)
    }

    /// Find the source file and line corresponding to the given virtual memory address.
    pub fn find_location(&self, probe: u64) -> Result<Option<Location<'_>>, Error> {
        match self.find_unit(probe) {
            Some(unit_id) => self.units[unit_id].find_location(probe, &self.sections),
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
        let (unit_id, loc, funcs) = match self.find_unit(probe) {
            Some(unit_id) => {
                let unit = &self.units[unit_id];
                let loc = unit.find_location(probe, &self.sections)?;
                let functions = unit.parse_functions(&self.sections)?;
                let mut res: SmallVec<[_; 16]> = SmallVec::new();
                if let Ok(address) = functions.addresses.binary_search_by(|address| {
                    if probe < address.range.begin {
                        Ordering::Greater
                    } else if probe >= address.range.end {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                }) {
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
                }
                (unit_id, loc, res)
            }
            None => (0, None, SmallVec::new()),
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
            unit.parse_functions(&self.sections)?;
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
    line: Option<u64>,
    column: Option<u64>,
}

struct ResUnit<R>
where
    R: gimli::Reader,
{
    dw_unit: gimli::Unit<R>,
    lang: Option<gimli::DwLang>,
    lines: LazyCell<Result<Lines, Error>>,
    funcs: LazyCell<Result<Functions<R::Offset>, Error>>,
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
                    let line = row.line();
                    let column = match row.column() {
                        gimli::ColumnType::LeftEdge => None,
                        gimli::ColumnType::Column(x) => Some(x),
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

    fn parse_functions(&self, sections: &gimli::Dwarf<R>) -> Result<&Functions<R::Offset>, Error> {
        self.funcs
            .borrow_with(|| Functions::parse(&self.dw_unit, sections))
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
            line: row.line,
            column: row.column,
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
    unit: &ResUnit<R>,
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
        gimli::AttributeValue::UnitRef(offset) => unit.dw_unit.entries_raw(Some(offset))?,
        gimli::AttributeValue::DebugInfoRef(dr) => {
            if let Some((unit, offset)) = units
                .iter()
                .filter_map(|unit| {
                    gimli::UnitSectionOffset::DebugInfoOffset(dr)
                        .to_unit_offset(&unit.dw_unit)
                        .map(|uo| (&unit.dw_unit, uo))
                })
                .next()
            {
                unit.entries_raw(Some(offset))?
            } else {
                return Err(gimli::Error::NoEntryAtGivenOffset);
            }
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
                    if let Ok(val) = sections.attr_string(&unit.dw_unit, attr.value()) {
                        return Ok(Some(val));
                    }
                }
                gimli::DW_AT_name => {
                    if let Ok(val) = sections.attr_string(&unit.dw_unit, attr.value()) {
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

struct Functions<T> {
    /// List of all `DW_TAG_subprogram` and `DW_TAG_inlined_subroutine` details.
    functions: Box<[Function<T>]>,
    /// List of `DW_TAG_subprogram` address ranges in the unit.
    addresses: Box<[FunctionAddress]>,
}

struct Function<T> {
    offset: gimli::UnitOffset<T>,
    /// List of `DW_TAG_inlined_subroutine` address ranges in this function.
    // TODO: this is often empty, so we could save more memory by storing the
    // length in the allocated memory.
    inlined: Box<[FunctionAddress]>,
}

impl<T: gimli::ReaderOffset> Functions<T> {
    fn parse<R: gimli::Reader<Offset = T>>(
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
    ) -> Result<Functions<T>, Error> {
        let mut functions = Vec::new();
        let mut addresses = Vec::new();
        // These are ignored.
        let mut inlined = Vec::new();
        let mut entries = unit.entries_raw(None)?;
        while !entries.is_empty() {
            let offset = entries.next_offset();
            let depth = entries.next_depth();
            if let Some(abbrev) = entries.read_abbreviation()? {
                if abbrev.tag() == gimli::DW_TAG_subprogram {
                    Function::parse(
                        &mut entries,
                        abbrev,
                        offset,
                        depth,
                        unit,
                        sections,
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
        addresses.sort_by_key(|x| x.range.begin);
        debug_assert!(addresses
            .windows(2)
            .all(|w| w[0].range.end <= w[1].range.begin));

        Ok(Functions {
            functions: functions.into_boxed_slice(),
            addresses: addresses.into_boxed_slice(),
        })
    }
}

impl<T: gimli::ReaderOffset> Function<T> {
    fn parse<R: gimli::Reader<Offset = T>>(
        entries: &mut gimli::EntriesRaw<R>,
        abbrev: &gimli::Abbreviation,
        offset: gimli::UnitOffset<R::Offset>,
        depth: isize,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        functions: &mut Vec<Function<T>>,
        addresses: &mut Vec<FunctionAddress>,
        inlined: &mut Vec<FunctionAddress>,
    ) -> Result<(), Error> {
        let mut low_pc = None;
        let mut high_pc = None;
        let mut size = None;
        let mut ranges = None;
        for spec in abbrev.attributes() {
            match entries.read_attribute(*spec) {
                Ok(ref attr) => match attr.name() {
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
                    _ => {}
                },
                Err(e) => return Err(e),
            }
        }

        let mut local_inlined = Vec::new();
        loop {
            let next_depth = entries.next_depth();
            if next_depth <= depth {
                break;
            }
            let next_offset = entries.next_offset();
            if let Some(abbrev) = entries.read_abbreviation()? {
                if abbrev.tag() == gimli::DW_TAG_subprogram
                    || abbrev.tag() == gimli::DW_TAG_inlined_subroutine
                {
                    Function::parse(
                        entries,
                        abbrev,
                        next_offset,
                        next_depth,
                        unit,
                        sections,
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
            offset,
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
    funcs: iter::Rev<smallvec::IntoIter<[&'ctx Function<R::Offset>; 16]>>,
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
                    function: None,
                    location: Some(loc),
                }))
            }
        };

        let unit = &self.units[self.unit_id];

        let mut entries = unit.dw_unit.entries_raw(Some(func.offset))?;
        let abbrev = entries
            .read_abbreviation()?
            .expect("DIE we read a while ago is no longer readable??");

        let mut name = None;
        let mut call_file = None;
        let mut call_line = None;
        let mut call_column = None;
        for spec in abbrev.attributes() {
            match entries.read_attribute(*spec) {
                Ok(ref attr) => {
                    match attr.name() {
                        gimli::DW_AT_linkage_name | gimli::DW_AT_MIPS_linkage_name => {
                            if let Ok(val) = self.sections.attr_string(&unit.dw_unit, attr.value())
                            {
                                name = Some(val);
                            }
                        }
                        gimli::DW_AT_name => {
                            if name.is_none() {
                                name = self.sections.attr_string(&unit.dw_unit, attr.value()).ok();
                            }
                        }
                        gimli::DW_AT_abstract_origin | gimli::DW_AT_specification => {
                            if name.is_none() {
                                name =
                                    name_attr(attr.value(), unit, self.sections, self.units, 16)?;
                            }
                        }
                        _ => {}
                    }
                    if abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
                        match attr.name() {
                            gimli::DW_AT_call_file => {
                                if let gimli::AttributeValue::FileIndex(fi) = attr.value() {
                                    call_file = Some(fi);
                                }
                            }
                            gimli::DW_AT_call_line => {
                                call_line = attr.udata_value().and_then(|x| {
                                    if x == 0 {
                                        None
                                    } else {
                                        Some(x)
                                    }
                                });
                            }
                            gimli::DW_AT_call_column => {
                                call_column = attr.udata_value();
                            }
                            _ => {}
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }

        if abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
            let file = match call_file {
                Some(fi) => match unit.parse_lines(self.sections)? {
                    Some(lines) => lines.files.get(fi as usize).map(String::as_str),
                    None => None,
                },
                _ => None,
            };

            self.next = Some(Location {
                file,
                line: call_line,
                column: call_column,
            });
        }

        Ok(Some(Frame {
            function: name.map(|name| FunctionName {
                name,
                language: unit.lang,
            }),
            location: loc,
        }))
    }
}

impl<'ctx, R> FallibleIterator for FrameIter<'ctx, R>
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
    pub line: Option<u64>,
    /// The column number.
    pub column: Option<u64>,
}
