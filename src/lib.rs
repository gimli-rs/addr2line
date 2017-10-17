extern crate object;
extern crate intervaltree;
extern crate fallible_iterator;
extern crate gimli;
extern crate smallvec;
#[cfg(feature = "rustc-demangle")]
extern crate rustc_demangle;
#[cfg(feature = "cpp_demangle")]
extern crate cpp_demangle;

use std::path::PathBuf;
use std::cmp::Ordering;
use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::u64;

use intervaltree::IntervalTree;
use smallvec::SmallVec;

struct Func<T> {
    unit_id: usize,
    entry_off: gimli::UnitOffset<T>,
    depth: isize,
}

struct ResUnit<R: gimli::Reader> {
    offset: gimli::DebugInfoOffset<R::Offset>,
    abbrevs: gimli::Abbreviations,
    inner: UnitInner<R>,
}

struct UnitInner<R: gimli::Reader> {
    lnp: gimli::CompleteLineNumberProgram<R>,
    sequences: Vec<gimli::LineNumberSequence<R>>,
    comp_dir: Option<R>,
    lang: gimli::DwLang,
    base_addr: u64,
}

fn render_file<R: gimli::Reader>(header: &gimli::LineNumberProgramHeader<R>,
                                 ffile: &gimli::FileEntry<R>,
                                 dcd: &Option<R>) -> Result<PathBuf, gimli::Error> {
    let name = ffile.path_name();
    let directory = ffile.directory(header).unwrap();

    let rpath = dcd.as_ref().unwrap().to_string_lossy()?;
    let mut path = PathBuf::from(rpath.as_ref());
    path.push(directory.to_string_lossy()?.as_ref());
    path.push(name.to_string_lossy()?.as_ref());
    Ok(path)
}

pub struct Context<R: gimli::Reader> {
    unit_ranges: Vec<(gimli::Range, usize)>,
    units: Vec<ResUnit<R>>,
    sections: DebugSections<R>,
}

pub struct FullContext<R: gimli::Reader> {
    funcs: IntervalTree<u64, Func<R::Offset>>,
    light: Context<R>,
}

fn read_ranges<R: gimli::Reader>(entry: &gimli::DebuggingInformationEntry<R, R::Offset>,
                                 debug_ranges: &gimli::DebugRanges<R>,
                                 addr_size: u8, base_addr: u64) -> Option<WrapRangeIter<R>> {
    Some(match entry.attr_value(gimli::DW_AT_ranges).unwrap() {
        None => {
            let low_pc = match entry.attr_value(gimli::DW_AT_low_pc).unwrap() {
                Some(gimli::AttributeValue::Addr(low_pc)) => low_pc,
                None => return None, // neither ranges nor low_pc => None
                _ => unreachable!(),
            };
            let high_pc = match entry.attr_value(gimli::DW_AT_high_pc).unwrap().unwrap() {
                gimli::AttributeValue::Addr(high_pc) => high_pc,
                gimli::AttributeValue::Udata(x) => low_pc + x,
                _ => unreachable!(),
            };
            WrapRangeIter::Synthetic(Some(gimli::Range { begin: low_pc, end: high_pc }))
        },
        Some(gimli::AttributeValue::DebugRangesRef(rr)) => {
            let ranges = debug_ranges.ranges(rr, addr_size, base_addr).unwrap();
            WrapRangeIter::Real(ranges)
        }
        _ => unreachable!(),
    })
}

impl<'a> Context<gimli::EndianBuf<'a, gimli::RunTimeEndian>> {
    pub fn new(file: &'a object::File) -> Self {
        let endian = if file.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        fn load_section<'input, 'file, S, Endian>(file: &'file object::File<'input>, endian: Endian) -> S
            where S: gimli::Section<gimli::EndianBuf<'input, Endian>>, Endian: gimli::Endianity, 'file: 'input,
        {
            let data = file.get_section(S::section_name()).unwrap_or(&[]);
            S::from(gimli::EndianBuf::new(data, endian))
        }

        let debug_abbrev: gimli::DebugAbbrev<_> = load_section(file, endian);
        let debug_info: gimli::DebugInfo<_> = load_section(file, endian);
        let debug_line: gimli::DebugLine<_> = load_section(file, endian);
        let debug_ranges: gimli::DebugRanges<_> = load_section(file, endian);
        let debug_str: gimli::DebugStr<_> = load_section(file, endian);

        let mut unit_ranges = Vec::new();
        let mut res_units = Vec::new();
        let mut units = debug_info.units();
        while let Some(dw_unit) = units.next().unwrap() {
            let unit_id = res_units.len();

            let abbrevs = dw_unit.abbreviations(&debug_abbrev).unwrap();

            let inner = {

                let mut cursor = dw_unit.entries(&abbrevs);

                let (_, unit) = cursor.next_dfs().unwrap().unwrap();
                assert_eq!(unit.tag(), gimli::DW_TAG_compile_unit);
                let dlr = match unit.attr_value(gimli::DW_AT_stmt_list).unwrap() {
                    Some(gimli::AttributeValue::DebugLineRef(dlr)) => dlr,
                    _ => unreachable!(),
                };
                let dcd = unit.attr(gimli::DW_AT_comp_dir).unwrap().map(|x| x.string_value(&debug_str).unwrap());
                let dcn = unit.attr(gimli::DW_AT_name).unwrap().map(|x| x.string_value(&debug_str).unwrap());
                let base_addr = match unit.attr_value(gimli::DW_AT_low_pc).unwrap() {
                    Some(gimli::AttributeValue::Addr(addr)) => addr,
                    _ => unreachable!(),
                };
                let lang = match unit.attr_value(gimli::DW_AT_language).unwrap() {
                    Some(gimli::AttributeValue::Language(lang)) => lang,
                    _ => unreachable!(),
                };
                if let Some(mut ranges) = read_ranges(unit, &debug_ranges, dw_unit.address_size(), base_addr) {
                    while let Some(range) = ranges.next().unwrap() {
                        if range.begin == range.end { continue; }

                        unit_ranges.push((range, unit_id));
                    }
                }

                let ilnp = debug_line.program(dlr, dw_unit.address_size(), dcd, dcn).unwrap();
                let (lnp, mut sequences) = ilnp.sequences().unwrap();
                sequences.sort_by_key(|x| x.start);
                UnitInner { lnp, sequences, comp_dir: dcd, lang, base_addr }
            };

            res_units.push(ResUnit {
                offset: dw_unit.offset(),
                abbrevs,
                inner,
            });
        }

        unit_ranges.sort_by_key(|x| x.0.begin);

        // ranges need to be disjoint or we lost
        debug_assert!(unit_ranges.windows(2).all(|w| w[0].0.end <= w[1].0.begin));

        Context {
            units: res_units,
            unit_ranges,
            sections: DebugSections {
                debug_info,
                debug_str,
                debug_ranges,
            }
        }
    }
}

impl<R: gimli::Reader> Context<R> {
    pub fn parse_functions(self) -> FullContext<R> {
        let mut results = Vec::new();

        for (unit_id, unit) in self.units.iter().enumerate() {
            let mut depth = 0;

            let dw_unit = self.sections.debug_info.header_from_offset(unit.offset).unwrap();
            let abbrevs = &unit.abbrevs;

            let mut cursor = dw_unit.entries(&abbrevs);
            while let Some((d, entry)) = cursor.next_dfs().unwrap() {
                depth += d;
                match entry.tag() {
                    gimli::DW_TAG_subprogram | gimli::DW_TAG_inlined_subroutine => {
                        // may be an inline-only function and thus not have any ranges
                        if let Some(mut ranges) = read_ranges(entry,
                                                              &self.sections.debug_ranges,
                                                              dw_unit.address_size(),
                                                              unit.inner.base_addr) {
                            while let Some(range) = ranges.next().unwrap() {
                                results.push((range.begin .. range.end, Func {
                                    unit_id,
                                    entry_off: entry.offset(),
                                    depth,
                                }));
                            }
                        }
                    }
                    _ => (),
                }
            }
        }

        let tree: IntervalTree<_, _> = results.into_iter().collect();
        FullContext {
            light: self,
            funcs: tree,
        }
    }
}

struct DebugSections<R: gimli::Reader> {
    debug_info: gimli::DebugInfo<R>,
    debug_str: gimli::DebugStr<R>,
    debug_ranges: gimli::DebugRanges<R>,
}

pub struct IterFrames<'ctx, R: gimli::Reader + 'ctx> {
    units: &'ctx Vec<ResUnit<R>>,
    sections: &'ctx DebugSections<R>,
    funcs: smallvec::IntoIter<[&'ctx Func<R::Offset>; 16]>,
    next: Option<Location>,
}

pub struct Frame<R: gimli::Reader> {
    pub function: Option<FunctionName<R>>,
    pub location: Option<Location>,
}

pub struct FunctionName<R: gimli::Reader> {
    name: R,
    pub language: gimli::DwLang,
}

impl<R: gimli::Reader> FunctionName<R> {
    pub fn raw_name(&self) -> Cow<str> {
        self.name.to_string_lossy().unwrap()
    }

    pub fn demangle(&self) -> Option<String> {
        let name = self.name.to_string_lossy().unwrap();
        match self.language {
            #[cfg(feature = "rustc-demangle")]
            gimli::DW_LANG_Rust => rustc_demangle::try_demangle(name.as_ref())
                .ok().as_ref().map(ToString::to_string),
            #[cfg(feature = "cpp_demangle")]
            gimli::DW_LANG_C_plus_plus
                | gimli::DW_LANG_C_plus_plus_03
                | gimli::DW_LANG_C_plus_plus_11
                | gimli::DW_LANG_C_plus_plus_14 =>
                cpp_demangle::Symbol::new(name.as_ref()).ok()
                    .and_then(|x| x.demangle(&Default::default()).ok()),
            _ => None,
        }
    }
}

impl<R: gimli::Reader> Display for FunctionName<R> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let name = self.demangle().map(Cow::from).unwrap_or(self.raw_name());
        write!(fmt, "{}", name)
    }
}

enum WrapRangeIter<R: gimli::Reader> {
    Real(gimli::RangesIter<R>),
    Synthetic(Option<gimli::Range>),
}

use fallible_iterator::FallibleIterator;

impl<R: gimli::Reader> FallibleIterator for WrapRangeIter<R> {
    type Item = gimli::Range;
    type Error = gimli::Error;

    fn next(&mut self) -> Result<Option<gimli::Range>, gimli::Error> {
        match *self {
            WrapRangeIter::Real(ref mut ri) => ri.next(),
            WrapRangeIter::Synthetic(ref mut range) => Ok(range.take()),
        }
    }
}

pub struct Location {
    pub file: PathBuf,
    pub line: Option<u64>,
    pub column: Option<u64>,
}

impl<R: gimli::Reader> Context<R> {
    pub fn find_location(&self, probe: u64) -> Option<Location> {
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

        self.find_location_inner(probe, &self.units[unit_id].inner)
    }

    fn find_location_inner(&self, probe: u64, uunit: &UnitInner<R>) -> Option<Location> {
        let cp = &uunit.lnp;
        let idx = uunit.sequences.binary_search_by(|ln| {
            if probe < ln.start {
                Ordering::Greater
            } else if probe >= ln.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        let idx = match idx {
            Ok(x) => x,
            Err(_) => return None,
        };
        let ln = &uunit.sequences[idx];
        let mut sm = cp.resume_from(&ln);
        let mut file = None;
        let mut line = None;
        let mut col = None;
        while let Some((_, row)) = sm.next_row().unwrap() {
            if row.address() > probe {
                break;
            }

            file = row.file(cp.header());
            line = row.line();
            col = match row.column() {
                gimli::ColumnType::LeftEdge => None,
                gimli::ColumnType::Column(x) => Some(x),
            };
        }

        Some(Location {
            file: render_file(uunit.lnp.header(), file.unwrap(), &uunit.comp_dir).unwrap(),
            line: line,
            column: col,
        })
    }
}

impl<R: gimli::Reader> FullContext<R> {
    pub fn query<'a>(&self, probe: u64) -> IterFrames<R> {
        let ctx = &self.light;
        let mut res: SmallVec<[_; 16]> = self.funcs.query_point(probe).map(|x| &x.value).collect();
        res.sort_by_key(|x| -x.depth);

        let loc = match res.get(0) {
            Some(r) => {
                let uunit = &ctx.units[r.unit_id].inner;
                self.light.find_location_inner(probe, uunit)
            }
            None => self.light.find_location(probe),
        };

        IterFrames {
            units: &ctx.units,
            sections: &ctx.sections,
            funcs: res.into_iter(),
            next: loc,
        }
    }
}

fn str_attr<'abbrev, 'unit, R: gimli::Reader>(entry: &gimli::DebuggingInformationEntry<'abbrev, 'unit, R, R::Offset>,
                                              dw_unit: &gimli::CompilationUnitHeader<R, R::Offset>,
                                              abbrevs: &gimli::Abbreviations,
                                              sections: &DebugSections<R>,
                                              name: gimli::DwAt) -> Option<R> {
    match entry.attr(name).unwrap() {
        Some(x) => Some(x.string_value(&sections.debug_str).unwrap()),
        None => {
            match entry.attr_value(gimli::DW_AT_abstract_origin).unwrap() {
                Some(gimli::AttributeValue::UnitRef(offset)) => {
                    let mut tcursor = dw_unit.entries_at_offset(&abbrevs, offset).unwrap();
                    let (_, entry) = tcursor.next_dfs().unwrap().unwrap();

                    str_attr(entry, dw_unit, abbrevs, sections, name)
                }
                None => None,
                x => panic!("wat {:?}", x),
            }
        }
    }
}

impl<'ctx, R: gimli::Reader + 'ctx> Iterator for IterFrames<'ctx, R> {
    type Item = Frame<R>;
    fn next(&mut self) -> Option<Frame<R>> {
        let (loc, func) = match (self.next.take(), self.funcs.next()) {
            (None, None) => return None,
            (loc, Some(func)) => (loc, func),
            (Some(loc), None) => return Some(Frame {
                function: None,
                location: Some(loc),
            }),
        };

        let unit = &self.units[func.unit_id];
        let dw_unit = self.sections.debug_info.header_from_offset(unit.offset).unwrap();
        let abbrevs = &unit.abbrevs;

        let mut cursor = dw_unit.entries_at_offset(abbrevs, func.entry_off).unwrap();
        let (_, entry) = cursor.next_dfs().unwrap().unwrap();

        let name = str_attr(entry, &dw_unit, abbrevs, self.sections, gimli::DW_AT_linkage_name);

        if entry.tag() == gimli::DW_TAG_inlined_subroutine {
            let file = match entry.attr_value(gimli::DW_AT_call_file).unwrap() {
                Some(gimli::AttributeValue::FileIndex(fi)) => {
                    unit.inner.lnp.header().file(fi).unwrap()
                }
                _ => unreachable!(),
            };

            let line = entry.attr(gimli::DW_AT_call_line).unwrap().and_then(|x| x.udata_value());
            let column = entry.attr(gimli::DW_AT_call_column).unwrap().and_then(|x| x.udata_value());

            self.next = Some(Location {
                file: render_file(unit.inner.lnp.header(), file, &unit.inner.comp_dir).unwrap(),
                line,
                column,
            });
        }


        Some(Frame {
            function: name.map(|name| FunctionName {
                name,
                language: unit.inner.lang,
            }),
            location: loc,
        })
    }
}
