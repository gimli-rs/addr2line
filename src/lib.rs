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

use core::cell::RefCell;
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

                let mut range_components: RangeComponents<R> = Default::default();
                for spec in abbrev.attributes() {
                    let attr = entries.read_attribute(*spec)?;
                    range_components.parse_range_attr(&attr, &sections, &dw_unit)?;
                    match attr.name() {
                        gimli::DW_AT_language => {
                            if let gimli::AttributeValue::Language(val) = attr.value() {
                                lang = Some(val);
                            }
                        }
                        _ => {}
                    }
                }

                range_components.for_each_range(&sections, &dw_unit, true, |range| {
                    unit_ranges.push(UnitRange {
                        range,
                        unit_id,
                        max_end: 0,
                    });
                })?;
            }

            res_units.push(ResUnit {
                offset,
                dw_unit,
                lang,
                lines: LazyCell::new(),
                funcs: RefCell::new(None),
            });
        }

        // Sort this for faster lookup in `find_unit_id_and_function` below.
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
    // Returns Option<(unit ID, index in self.functions.functions)
    fn find_unit_id_and_function(&self, probe: u64) -> Option<(usize, usize)> {
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
            if let Ok(Some(function_index)) =
                self.units[i.unit_id].find_function(&self.sections, probe)
            {
                return Some((i.unit_id, function_index));
            }
        }

        None
    }

    /// Find the DWARF unit corresponding to the given virtual memory address.
    pub fn find_dwarf_unit(&self, probe: u64) -> Option<&gimli::Unit<R>> {
        self.find_unit_id_and_function(probe)
            .map(|(unit_id, _)| &self.units[unit_id].dw_unit)
    }

    /// Find the source file and line corresponding to the given virtual memory address.
    pub fn find_location(&self, probe: u64) -> Result<Option<Location<'_>>, Error> {
        match self.find_unit_id_and_function(probe) {
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
        let (unit_id, function_index) = match self.find_unit_id_and_function(probe) {
            Some(unit_id_and_offset) => unit_id_and_offset,
            None => return Ok(FrameIter(FrameIterState::Empty())),
        };

        let unit = &self.units[unit_id];
        let loc = unit.find_location(probe, &self.sections)?;
        let function = unit.get_parsed_function(&self.sections, &self.units, function_index)?;

        // Build the list of inline functions that contain probe. res is ordered from outside to inside.
        // The indexes are into the function.inline_functions vec.
        let mut res = maybe_small::Vec::new();
        // Walk down the path of inline functions that contain probe.
        let mut inline_ranges = &function.inline_ranges[..];
        loop {
            let current_depth = res.len();
            // Look up (probe, current_depth) in inline_ranges.
            // inline_ranges is sorted in "breadth-first traversal order", i.e.
            // by call_depth first, and then by range.begin. See the comment at
            // the sort call for more information about why.
            let search = inline_ranges.binary_search_by(|range| {
                if range.call_depth > current_depth {
                    Ordering::Greater
                } else if range.call_depth < current_depth {
                    Ordering::Less
                } else if range.range.begin > probe {
                    Ordering::Greater
                } else if range.range.end <= probe {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            if let Ok(index) = search {
                res.push(inline_ranges[index].function);
                inline_ranges = &inline_ranges[index + 1..];
            } else {
                break;
            }
        }
        Ok(FrameIter(FrameIterState::Frames(FrameIterFrames {
            unit,
            sections: &self.sections,
            outer_function: function,
            inline_function_indexes: res.into_iter().rev(),
            next_location: loc,
        })))
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
    funcs: RefCell<Option<Functions<R>>>,
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
                            sequences.push(LineSequence {
                                start,
                                end,
                                rows: rows.into_boxed_slice(),
                            });
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

    fn with_parsed_function_list<S, F: FnMut(&mut Functions<R>) -> S>(
        &self,
        sections: &gimli::Dwarf<R>,
        mut f: F,
    ) -> Result<S, Error> {
        loop {
            {
                let mut funcs = self.funcs.borrow_mut();
                if let Some(funcs) = &mut *funcs {
                    return Ok(f(funcs));
                }
            }
            self.funcs
                .replace(Some(Functions::parse(&self.dw_unit, sections)?));
        }
    }

    fn parse_functions(&self, sections: &gimli::Dwarf<R>) -> Result<(), Error> {
        self.with_parsed_function_list(sections, |_| {})?;
        Ok(())
    }

    fn find_function(
        &self,
        sections: &gimli::Dwarf<R>,
        probe: u64,
    ) -> Result<Option<usize>, Error> {
        self.with_parsed_function_list(sections, |funcs| funcs.find_function(probe))
    }

    fn get_parsed_function(
        &self,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
        function_index: usize,
    ) -> Result<Rc<OuterFunction<R>>, Error> {
        self.with_parsed_function_list(sections, |funcs| {
            funcs.get_parsed_function(&self.dw_unit, sections, units, function_index)
        })?
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
/// is handled by having multiple `FunctionAddressRange` entries with the same
/// `function` field.
struct OuterFunctionAddressRange {
    range: gimli::Range,
    /// The index in `Functions::functions`.
    function_index: usize,
}

struct Functions<R: gimli::Reader> {
    /// List of `DW_TAG_subprogram` address ranges in the unit.
    function_address_ranges: Box<[OuterFunctionAddressRange]>,
    /// Map of all `DW_TAG_subprogram` details, keyed by dw_die_offset.
    functions: Vec<(gimli::UnitOffset<R::Offset>, Option<Rc<OuterFunction<R>>>)>,
}

struct OuterFunction<R: gimli::Reader> {
    dw_die_offset: gimli::UnitOffset<R::Offset>,
    name: Option<R>,
    /// List of `DW_TAG_inlined_subroutine` functions and address ranges in this function.
    // TODO: this is often empty, so we could save more memory by storing the
    // length in the allocated memory.
    inline_functions: Vec<InlinedFunction<R>>,
    inline_ranges: Box<[InlinedFunctionAddressRange]>,
}

struct InlinedFunctionAddressRange {
    range: gimli::Range,
    call_depth: usize,
    /// An index into `OuterFunction::inline_functions`.
    function: usize,
}

struct InlinedFunction<R: gimli::Reader> {
    dw_die_offset: gimli::UnitOffset<R::Offset>,
    name: Option<R>,
    call_file_index: Option<u64>,
    call_line: Option<u32>,
    call_column: Option<u32>,
}

impl<R: gimli::Reader> Functions<R> {
    fn parse(unit: &gimli::Unit<R>, sections: &gimli::Dwarf<R>) -> Result<Functions<R>, Error> {
        let mut functions = Vec::new();
        let mut function_address_ranges = Vec::new();
        let mut entries = unit.entries_raw(None)?;
        while !entries.is_empty() {
            let dw_die_offset = entries.next_offset();
            if let Some(abbrev) = entries.read_abbreviation()? {
                if abbrev.tag() == gimli::DW_TAG_subprogram {
                    let mut range_components: RangeComponents<R> = Default::default();
                    for spec in abbrev.attributes() {
                        match entries.read_attribute(*spec) {
                            Ok(ref attr) => {
                                range_components.parse_range_attr(attr, sections, unit)?;
                            }
                            Err(e) => return Err(e),
                        }
                    }

                    let function_index = functions.len();
                    let added_any =
                        range_components.for_each_range(sections, unit, false, |range| {
                            function_address_ranges.push(OuterFunctionAddressRange {
                                range,
                                function_index,
                            });
                        })?;
                    if added_any {
                        functions.push((dw_die_offset, None));
                    }
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

        // The binary search requires the function_address_ranges to be sorted.
        //
        // It also requires them to be non-overlapping.  In practice, overlapping
        // function ranges are unlikely, so we don't try to handle that yet.
        //
        // It's possible for multiple functions to have the same address range if the
        // compiler can detect and remove functions with identical code.  In that case
        // we'll nondeterministically return one of them.
        function_address_ranges.sort_by_key(|x| x.range.begin);

        Ok(Functions {
            functions,
            function_address_ranges: function_address_ranges.into_boxed_slice(),
        })
    }

    // Returns the index in self.functions based on an address range that covers probe.
    fn find_function(&mut self, probe: u64) -> Option<usize> {
        if let Ok(address_range_index) = self.function_address_ranges.binary_search_by(|address| {
            if probe < address.range.begin {
                Ordering::Greater
            } else if probe >= address.range.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        }) {
            Some(self.function_address_ranges[address_range_index].function_index)
        } else {
            None
        }
    }

    fn get_parsed_function(
        &mut self,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
        function_index: usize,
    ) -> Result<Rc<OuterFunction<R>>, Error> {
        let (dw_die_offset, ref fun) = self.functions[function_index];
        if let Some(fun) = fun {
            return Ok(fun.clone());
        }
        let function = OuterFunction::parse(dw_die_offset, unit, sections, units)?;
        self.functions[function_index].1 = Some(function.clone());
        Ok(function)
    }
}

struct RangeComponents<R: gimli::Reader> {
    low_pc: Option<u64>,
    high_pc: Option<u64>,
    size: Option<u64>,
    range_list_offset: Option<gimli::RangeListsOffset<<R as gimli::Reader>::Offset>>,
}

impl<R: gimli::Reader> Default for RangeComponents<R> {
    fn default() -> Self {
        RangeComponents {
            low_pc: None,
            high_pc: None,
            size: None,
            range_list_offset: None,
        }
    }
}

impl<R: gimli::Reader> RangeComponents<R> {
    fn parse_range_attr(
        &mut self,
        attr: &gimli::read::Attribute<R>,
        sections: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
    ) -> Result<(), Error> {
        match attr.name() {
            gimli::DW_AT_low_pc => {
                if let gimli::AttributeValue::Addr(val) = attr.value() {
                    self.low_pc = Some(val);
                }
            }
            gimli::DW_AT_high_pc => match attr.value() {
                gimli::AttributeValue::Addr(val) => self.high_pc = Some(val),
                gimli::AttributeValue::Udata(val) => self.size = Some(val),
                _ => {}
            },
            gimli::DW_AT_ranges => {
                self.range_list_offset = sections.attr_ranges_offset(unit, attr.value())?;
            }
            _ => {}
        };
        Ok(())
    }

    fn for_each_range<F: FnMut(gimli::Range)>(
        &self,
        sections: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        allow_at_zero: bool,
        mut f: F,
    ) -> Result<bool, Error> {
        let mut added_any = false;
        let mut add_range = |range: gimli::Range| {
            // Ignore invalid DWARF so that a query of 0 does not give
            // a long list of matches.
            // TODO: don't ignore if there is a section at this address
            if (allow_at_zero || range.begin != 0) && range.begin < range.end {
                f(range);
                added_any = true
            }
        };
        if let Some(range_list_offset) = self.range_list_offset {
            let mut range_list = sections.ranges(unit, range_list_offset)?;
            while let Some(range) = range_list.next()? {
                add_range(range);
            }
        } else if let (Some(begin), Some(end)) = (self.low_pc, self.high_pc) {
            add_range(gimli::Range { begin, end });
        } else if let (Some(begin), Some(size)) = (self.low_pc, self.size) {
            add_range(gimli::Range {
                begin,
                end: begin + size,
            });
        }
        Ok(added_any)
    }
}

impl<R: gimli::Reader> OuterFunction<R> {
    fn parse(
        dw_die_offset: gimli::UnitOffset<R::Offset>,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
    ) -> Result<Rc<Self>, Error> {
        let mut entries = unit.entries_raw(Some(dw_die_offset))?;
        debug_assert_eq!(entries.next_offset(), dw_die_offset);
        let depth = entries.next_depth();
        let abbrev = entries.read_abbreviation()?.unwrap();
        debug_assert_eq!(abbrev.tag(), gimli::DW_TAG_subprogram);

        let mut name = None;
        for spec in abbrev.attributes() {
            match entries.read_attribute(*spec) {
                Ok(ref attr) => {
                    Self::parse_name_attr(attr, &mut name, sections, unit, units)?;
                }
                Err(e) => return Err(e),
            }
        }

        let mut inline_functions = Vec::new();
        let mut inline_address_ranges = Vec::new();
        loop {
            let child_dw_die_offset = entries.next_offset();
            let next_depth = entries.next_depth();
            if next_depth <= depth {
                break;
            }
            if let Some(child_abbrev) = entries.read_abbreviation()? {
                if child_abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
                    Self::parse_inlined_subroutine(
                        child_dw_die_offset,
                        &mut entries,
                        child_abbrev,
                        next_depth,
                        0,
                        unit,
                        sections,
                        units,
                        &mut inline_functions,
                        &mut inline_address_ranges,
                    )?;
                    continue;
                }

                // Consume the abbrev attributes if we end up ignoring this abbrev,
                // as required by the entries iterator API.
                for spec in child_abbrev.attributes() {
                    match entries.read_attribute(*spec) {
                        Ok(_) => {}
                        Err(e) => return Err(e),
                    }
                }
            }
        }

        // Sort ranges in "breadth-first traversal order", i.e. first by call_depth
        // and then by range.begin. This allows finding the range containing an
        // address at a certain depth using binary search.
        // Note: Using DFS order, i.e. ordering by range.begin first and then by
        // call_depth, would not work! Consider the two examples
        // "[0..10 at depth 0], [0..2 at depth 1], [6..8 at depth 1]"  and
        // "[0..5 at depth 0], [0..2 at depth 1], [5..10 at depth 0], [6..8 at depth 1]".
        // In this example, if you want to look up address 7 at depth 0, and you
        // encounter [0..2 at depth 1], are you before or after the target range?
        // You don't know.
        inline_address_ranges.sort_by(|r1, r2| {
            if r1.call_depth < r2.call_depth {
                Ordering::Less
            } else if r1.call_depth > r2.call_depth {
                Ordering::Greater
            } else if r1.range.begin < r2.range.begin {
                Ordering::Less
            } else if r1.range.begin > r2.range.begin {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        });

        Ok(Rc::new(Self {
            dw_die_offset,
            name,
            inline_functions,
            inline_ranges: inline_address_ranges.into_boxed_slice(),
        }))
    }

    fn parse_inlined_subroutine(
        dw_die_offset: gimli::UnitOffset<R::Offset>,
        entries: &mut gimli::EntriesRaw<R>,
        abbrev: &gimli::Abbreviation,
        depth: isize,
        call_depth: usize,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
        units: &[ResUnit<R>],
        inline_functions: &mut Vec<InlinedFunction<R>>,
        inline_address_ranges: &mut Vec<InlinedFunctionAddressRange>,
    ) -> Result<(), Error> {
        let mut range_components: RangeComponents<R> = Default::default();
        let mut name = None;
        let mut call_file_index = 0;
        let mut call_line = 0;
        let mut call_column = 0;
        for spec in abbrev.attributes() {
            match entries.read_attribute(*spec) {
                Ok(ref attr) => {
                    range_components.parse_range_attr(attr, sections, unit)?;
                    Self::parse_name_attr(attr, &mut name, sections, unit, units)?;

                    match attr.name() {
                        gimli::DW_AT_call_file => {
                            if let gimli::AttributeValue::FileIndex(fi) = attr.value() {
                                call_file_index = fi;
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
                Err(e) => return Err(e),
            }
        }

        loop {
            let child_dw_die_offset = entries.next_offset();
            let next_depth = entries.next_depth();
            if next_depth <= depth {
                break;
            }
            if let Some(child_abbrev) = entries.read_abbreviation()? {
                if child_abbrev.tag() == gimli::DW_TAG_inlined_subroutine {
                    Self::parse_inlined_subroutine(
                        child_dw_die_offset,
                        entries,
                        child_abbrev,
                        next_depth,
                        call_depth + 1,
                        unit,
                        sections,
                        units,
                        inline_functions,
                        inline_address_ranges,
                    )?;
                    continue;
                }

                // Consume the abbrev attributes if we end up ignoring this abbrev,
                // as required by the entries iterator API.
                for spec in child_abbrev.attributes() {
                    match entries.read_attribute(*spec) {
                        Ok(_) => {}
                        Err(e) => return Err(e),
                    }
                }
            }
        }

        let inline_function_index = inline_functions.len();
        inline_functions.push(InlinedFunction {
            dw_die_offset,
            name,
            call_file_index: if call_file_index != 0 {
                Some(call_file_index)
            } else {
                None
            },
            call_line: if call_line != 0 {
                Some(call_line)
            } else {
                None
            },
            call_column: if call_column != 0 {
                Some(call_column)
            } else {
                None
            },
        });

        range_components.for_each_range(sections, unit, false, |range| {
            inline_address_ranges.push(InlinedFunctionAddressRange {
                range,
                function: inline_function_index,
                call_depth,
            });
        })?;

        Ok(())
    }

    fn parse_name_attr(
        attr: &gimli::read::Attribute<R>,
        name: &mut Option<R>,
        sections: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        units: &[ResUnit<R>],
    ) -> Result<(), Error> {
        match attr.name() {
            gimli::DW_AT_linkage_name | gimli::DW_AT_MIPS_linkage_name => {
                if let Ok(val) = sections.attr_string(unit, attr.value()) {
                    *name = Some(val);
                }
            }
            gimli::DW_AT_name => {
                if name.is_none() {
                    *name = sections.attr_string(unit, attr.value()).ok();
                }
            }
            gimli::DW_AT_abstract_origin | gimli::DW_AT_specification => {
                if name.is_none() {
                    *name = name_attr(attr.value(), unit, sections, units, 16)?;
                }
            }
            _ => {}
        };
        Ok(())
    }
}

/// An iterator over function frames.
pub struct FrameIter<'ctx, R>(FrameIterState<'ctx, R>)
where
    R: gimli::Reader + 'ctx;

enum FrameIterState<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    Empty(),
    Frames(FrameIterFrames<'ctx, R>),
}

struct FrameIterFrames<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    unit: &'ctx ResUnit<R>,
    sections: &'ctx gimli::Dwarf<R>,
    outer_function: Rc<OuterFunction<R>>,
    inline_function_indexes: iter::Rev<maybe_small::IntoIter<usize>>,
    next_location: Option<Location<'ctx>>,
}

impl<'ctx, R> FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    /// Advances the iterator and returns the next frame.
    pub fn next(&mut self) -> Result<Option<Frame<'ctx, R>>, Error> {
        let frames = match &mut self.0 {
            FrameIterState::Empty() => return Ok(None),
            FrameIterState::Frames(frames) => frames,
        };
        let FrameIterFrames {
            outer_function,
            inline_function_indexes,
            next_location,
            ..
        } = frames;
        let (loc, inline_func_index) = match (next_location.take(), inline_function_indexes.next())
        {
            (None, None) => return Ok(None),
            (loc, Some(inline_func_index)) => (loc, inline_func_index),
            (Some(loc), None) => {
                return Ok(Some(Frame {
                    dw_die_offset: Some(outer_function.dw_die_offset),
                    function: outer_function.name.clone().map(|name| FunctionName {
                        name,
                        language: frames.unit.lang,
                    }),
                    location: Some(loc),
                }))
            }
        };

        let &InlinedFunction {
            dw_die_offset,
            ref name,
            call_file_index,
            call_line,
            call_column,
        } = &outer_function.inline_functions[inline_func_index];

        let mut file = None;
        if let Some(file_index) = call_file_index {
            if let Some(lines) = frames.unit.parse_lines(frames.sections)? {
                file = lines.files.get(file_index as usize).map(String::as_str);
            }
        }
        *next_location = Some(Location {
            file,
            line: call_line,
            column: call_column,
        });

        Ok(Some(Frame {
            dw_die_offset: Some(dw_die_offset),
            function: name.clone().map(|name| FunctionName {
                name,
                language: frames.unit.lang,
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
