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
extern crate intervaltree;
extern crate lazycell;
#[cfg(feature = "object")]
pub extern crate object;
#[cfg(feature = "rustc-demangle")]
extern crate rustc_demangle;
extern crate smallvec;

#[cfg(feature = "std")]
mod alloc {
    pub use std::{borrow, rc, string, vec};
}

use alloc::borrow::Cow;
#[cfg(feature = "object")]
use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use std::cmp::Ordering;
use std::mem;
use std::u64;

use fallible_iterator::FallibleIterator;
use intervaltree::{Element, IntervalTree};
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
        let sections = gimli::Dwarf {
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
        };

        let mut unit_ranges = Vec::new();
        let mut res_units = Vec::new();
        let mut units = sections.units();
        while let Some(header) = units.next()? {
            let unit_id = res_units.len();
            let dw_unit = match sections.unit(header) {
                Ok(dw_unit) => dw_unit,
                Err(_) => continue,
            };

            let lang;
            {
                let mut cursor = dw_unit.entries();

                let unit = match cursor.next_dfs()? {
                    Some((_, unit)) if unit.tag() == gimli::DW_TAG_compile_unit => unit,
                    _ => continue, // wtf?
                };

                lang = match unit.attr_value(gimli::DW_AT_language)? {
                    Some(gimli::AttributeValue::Language(lang)) => Some(lang),
                    _ => None,
                };
                let mut ranges = sections.unit_ranges(&dw_unit)?;
                while let Some(range) = ranges.next()? {
                    if range.begin == range.end {
                        continue;
                    }

                    unit_ranges.push((range, unit_id));
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
                let funcs = unit.parse_functions(&self.sections)?;
                let mut res: SmallVec<[_; 16]> =
                    funcs.query_point(probe).map(|x| &x.value).collect();
                res.sort_by_key(|x| -x.depth);
                (unit_id, loc, res)
            }
            None => (0, None, SmallVec::new()),
        };

        Ok(FrameIter {
            unit_id,
            units: &self.units,
            sections: &self.sections,
            funcs: funcs.into_iter(),
            next: loc,
        })
    }
}

struct Lines {
    files: Vec<String>,
    sequences: Vec<LineSequence>,
}

struct LineSequence {
    start: u64,
    end: u64,
    rows: Vec<LineRow>,
}

struct LineRow {
    address: u64,
    file_index: u64,
    line: Option<u64>,
    column: Option<u64>,
}

struct Func<T> {
    entry_off: gimli::UnitOffset<T>,
    depth: isize,
}

struct ResUnit<R>
where
    R: gimli::Reader,
{
    dw_unit: gimli::Unit<R>,
    lang: Option<gimli::DwLang>,
    lines: LazyCell<Result<Lines, Error>>,
    funcs: LazyCell<Result<IntervalTree<u64, Func<R::Offset>>, Error>>,
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
                                sequences.push(LineSequence { start, end, rows });
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

                Ok(Lines { files, sequences })
            })
            .as_ref()
            .map(Some)
            .map_err(Error::clone)
    }

    fn parse_functions(
        &self,
        sections: &gimli::Dwarf<R>,
    ) -> Result<&IntervalTree<u64, Func<R::Offset>>, Error> {
        self.funcs
            .borrow_with(|| {
                let mut results = Vec::new();
                let mut depth = 0;
                let mut cursor = self.dw_unit.entries();
                while let Some((d, entry)) = cursor.next_dfs()? {
                    depth += d;
                    match entry.tag() {
                        gimli::DW_TAG_subprogram | gimli::DW_TAG_inlined_subroutine => {
                            let mut ranges = sections.die_ranges(&self.dw_unit, entry)?;
                            while let Some(range) = ranges.next()? {
                                // Ignore invalid DWARF so that a query of 0 does not give
                                // a long list of matches.
                                // TODO: don't ignore if there is a section at this address
                                if range.begin == 0 {
                                    continue;
                                }
                                results.push(Element {
                                    range: range.begin..range.end,
                                    value: Func {
                                        entry_off: entry.offset(),
                                        depth,
                                    },
                                });
                            }
                        }
                        _ => (),
                    }
                }

                let tree: IntervalTree<_, _> = results.into_iter().collect();
                Ok(tree)
            })
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
    entry: &gimli::DebuggingInformationEntry<'abbrev, 'unit, R, R::Offset>,
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

    if let Some(attr) = entry.attr_value(gimli::DW_AT_linkage_name)? {
        if let Ok(val) = sections.attr_string(&unit.dw_unit, attr) {
            return Ok(Some(val));
        }
    }
    if let Some(attr) = entry.attr_value(gimli::DW_AT_MIPS_linkage_name)? {
        if let Ok(val) = sections.attr_string(&unit.dw_unit, attr) {
            return Ok(Some(val));
        }
    }
    if let Some(attr) = entry.attr_value(gimli::DW_AT_name)? {
        if let Ok(val) = sections.attr_string(&unit.dw_unit, attr) {
            return Ok(Some(val));
        }
    }

    let next = entry
        .attr_value(gimli::DW_AT_abstract_origin)?
        .or(entry.attr_value(gimli::DW_AT_specification)?);
    match next {
        Some(gimli::AttributeValue::UnitRef(offset)) => {
            let mut entries = unit.dw_unit.entries_at_offset(offset)?;
            if let Some((_, entry)) = entries.next_dfs()? {
                return name_attr(entry, unit, sections, units, recursion_limit - 1);
            } else {
                return Err(gimli::Error::NoEntryAtGivenOffset);
            }
        }
        Some(gimli::AttributeValue::DebugInfoRef(dr)) => {
            if let Some((unit, offset)) = units
                .iter()
                .filter_map(|unit| {
                    gimli::UnitSectionOffset::DebugInfoOffset(dr)
                        .to_unit_offset(&unit.dw_unit)
                        .map(|uo| (unit, uo))
                })
                .next()
            {
                let mut entries = unit.dw_unit.entries_at_offset(offset)?;
                if let Some((_, entry)) = entries.next_dfs()? {
                    return name_attr(entry, unit, sections, units, recursion_limit - 1);
                }
            } else {
                return Err(gimli::Error::NoEntryAtGivenOffset);
            }
        }
        _ => {}
    }

    Ok(None)
}

/// An iterator over function frames.
pub struct FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    unit_id: usize,
    units: &'ctx Vec<ResUnit<R>>,
    sections: &'ctx gimli::Dwarf<R>,
    funcs: smallvec::IntoIter<[&'ctx Func<R::Offset>; 16]>,
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

        let mut cursor = unit.dw_unit.entries_at_offset(func.entry_off)?;
        let (_, entry) = cursor
            .next_dfs()?
            .expect("DIE we read a while ago is no longer readable??");

        // Set an arbitrary recursion limit of 16
        let name = name_attr(entry, unit, self.sections, self.units, 16)?;

        if entry.tag() == gimli::DW_TAG_inlined_subroutine {
            let file = match entry.attr_value(gimli::DW_AT_call_file)? {
                Some(gimli::AttributeValue::FileIndex(fi)) => {
                    match unit.parse_lines(self.sections)? {
                        Some(lines) => lines.files.get(fi as usize).map(String::as_str),
                        None => None,
                    }
                }
                _ => None,
            };

            let line = entry
                .attr(gimli::DW_AT_call_line)?
                .and_then(|x| x.udata_value())
                .and_then(|x| if x == 0 { None } else { Some(x) });
            let column = entry
                .attr(gimli::DW_AT_call_column)?
                .and_then(|x| x.udata_value());

            self.next = Some(Location { file, line, column });
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
