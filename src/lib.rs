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
//! [`Context::find_location`](./struct.Context.html#method.find_location) or
//! [`Context::find_location_range`](./struct.Context.html#method.find_location_range).
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

#[cfg(feature = "std")]
extern crate std;

#[allow(unused_imports)]
#[macro_use]
extern crate alloc;

#[cfg(feature = "fallible-iterator")]
pub extern crate fallible_iterator;
pub extern crate gimli;
#[cfg(feature = "object")]
pub extern crate object;

use alloc::borrow::Cow;
#[cfg(feature = "object")]
use alloc::rc::Rc;
use alloc::string::String;
use alloc::sync::Arc;

use core::iter;
use core::marker::PhantomData;
use core::ops::ControlFlow;
use core::u64;

use crate::function::{Function, Functions, InlinedFunction};
use crate::line::{LineLocationRangeIter, Lines};
use crate::unit::{ResUnit, ResUnits, SupUnits};

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

#[cfg(all(feature = "std", feature = "object", feature = "memmap2"))]
/// A simple builtin split DWARF loader.
pub mod builtin_split_dwarf_loader;
mod function;
mod lazy;
mod line;

mod unit;
pub use unit::LocationRangeIter;

type Error = gimli::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DebugFile {
    Primary,
    Supplementary,
    Dwo,
}

/// Operations that consult debug information may require additional files
/// to be loaded if split DWARF is being used. This enum returns the result
/// of the operation in the `Break` variant, or information about the split
/// DWARF that is required and a continuation to invoke once it is available
/// in the `Continue` variant.
///
/// This enum is intended to be used in a loop like so:
/// ```no_run
///   # use addr2line::*;
///   # use std::sync::Arc;
///   # let ctx: Context<gimli::EndianRcSlice<gimli::RunTimeEndian>> = todo!();
///   # let do_split_dwarf_load = |load: SplitDwarfLoad<gimli::EndianRcSlice<gimli::RunTimeEndian>>| -> Option<Arc<gimli::Dwarf<gimli::EndianRcSlice<gimli::RunTimeEndian>>>> { None };
///   const ADDRESS: u64 = 0xdeadbeef;
///   let mut r = ctx.find_frames(ADDRESS);
///   let result = loop {
///     match r {
///       LookupResult::Output(result) => break result,
///       LookupResult::Load { load, continuation } => {
///         let dwo = do_split_dwarf_load(load);
///         r = continuation.resume(dwo);
///       }
///     }
///   };
/// ```
pub enum LookupResult<L: LookupContinuation> {
    /// The lookup requires split DWARF data to be loaded.
    Load {
        /// The information needed to find the split DWARF data.
        load: SplitDwarfLoad<<L as LookupContinuation>::Buf>,
        /// The continuation to resume with the loaded split DWARF data.
        continuation: L,
    },
    /// The lookup has completed and produced an output.
    Output(<L as LookupContinuation>::Output),
}

/// This trait represents a partially complete operation that can be resumed
/// once a load of needed split DWARF data is completed or abandoned by the
/// API consumer.
pub trait LookupContinuation: Sized {
    /// The final output of this operation.
    type Output;
    /// The type of reader used.
    type Buf: gimli::Reader;

    /// Resumes the operation with the provided data.
    ///
    /// After the caller loads the split DWARF data required, call this
    /// method to resume the operation. The return value of this method
    /// indicates if the computation has completed or if further data is
    /// required.
    ///
    /// If the additional data cannot be located, or the caller does not
    /// support split DWARF, `resume(None)` can be used to continue the
    /// operation with the data that is available.
    fn resume(self, input: Option<Arc<gimli::Dwarf<Self::Buf>>>) -> LookupResult<Self>;
}

impl<L: LookupContinuation> LookupResult<L> {
    /// Callers that do not handle split DWARF can call `skip_all_loads`
    /// to fast-forward to the end result. This result is produced with
    /// the data that is available and may be less accurate than the
    /// the results that would be produced if the caller did properly
    /// support split DWARF.
    pub fn skip_all_loads(mut self) -> L::Output {
        loop {
            self = match self {
                LookupResult::Output(t) => return t,
                LookupResult::Load { continuation, .. } => continuation.resume(None),
            };
        }
    }

    fn map<T, F: FnOnce(L::Output) -> T>(self, f: F) -> LookupResult<MappedLookup<T, L, F>> {
        match self {
            LookupResult::Output(t) => LookupResult::Output(f(t)),
            LookupResult::Load { load, continuation } => LookupResult::Load {
                load,
                continuation: MappedLookup {
                    original: continuation,
                    mutator: f,
                },
            },
        }
    }

    fn unwrap(self) -> L::Output {
        match self {
            LookupResult::Output(t) => t,
            LookupResult::Load { .. } => unreachable!("Internal API misuse"),
        }
    }
}

/// The state necessary to perform address to line translation.
///
/// Constructing a `Context` is somewhat costly, so users should aim to reuse `Context`s
/// when performing lookups for many addresses in the same executable.
pub struct Context<R: gimli::Reader> {
    sections: Arc<gimli::Dwarf<R>>,
    units: ResUnits<R>,
    sup_units: SupUnits<R>,
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
    /// Performance sensitive applications may want to use `Context::from_dwarf`
    /// with a more specialised `gimli::Reader` implementation.
    #[inline]
    pub fn new<'data, O: object::Object<'data>>(file: &O) -> Result<Self, Error> {
        Self::new_with_sup(file, None)
    }

    /// Construct a new `Context`.
    ///
    /// Optionally also use a supplementary object file.
    ///
    /// The resulting `Context` uses `gimli::EndianRcSlice<gimli::RunTimeEndian>`.
    /// This means it is not thread safe, has no lifetime constraints (since it copies
    /// the input data), and works for any endianity.
    ///
    /// Performance sensitive applications may want to use `Context::from_dwarf`
    /// with a more specialised `gimli::Reader` implementation.
    pub fn new_with_sup<'data, O: object::Object<'data>>(
        file: &O,
        sup_file: Option<&O>,
    ) -> Result<Self, Error> {
        let endian = if file.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        fn load_section<'data, O, Endian>(
            id: gimli::SectionId,
            file: &O,
            endian: Endian,
        ) -> Result<gimli::EndianRcSlice<Endian>, Error>
        where
            O: object::Object<'data>,
            Endian: gimli::Endianity,
        {
            use object::ObjectSection;

            let data = file
                .section_by_name(id.name())
                .and_then(|section| section.uncompressed_data().ok())
                .unwrap_or(Cow::Borrowed(&[]));
            Ok(gimli::EndianRcSlice::new(Rc::from(&*data), endian))
        }

        let mut dwarf = gimli::Dwarf::load(|id| load_section(id, file, endian))?;
        if let Some(sup_file) = sup_file {
            dwarf.load_sup(|id| load_section(id, sup_file, endian))?;
        }
        Context::from_dwarf(dwarf)
    }
}

impl<R: gimli::Reader> Context<R> {
    /// Construct a new `Context` from DWARF sections.
    ///
    /// This method does not support using a supplementary object file.
    pub fn from_sections(
        debug_abbrev: gimli::DebugAbbrev<R>,
        debug_addr: gimli::DebugAddr<R>,
        debug_aranges: gimli::DebugAranges<R>,
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
            debug_aranges,
            debug_info,
            debug_line,
            debug_line_str,
            debug_str,
            debug_str_offsets,
            debug_types: default_section.clone().into(),
            locations: gimli::LocationLists::new(
                default_section.clone().into(),
                default_section.into(),
            ),
            ranges: gimli::RangeLists::new(debug_ranges, debug_rnglists),
            file_type: gimli::DwarfFileType::Main,
            sup: None,
            abbreviations_cache: gimli::AbbreviationsCache::new(),
        })
    }

    /// Construct a new `Context` from an existing [`gimli::Dwarf`] object.
    #[inline]
    pub fn from_dwarf(sections: gimli::Dwarf<R>) -> Result<Context<R>, Error> {
        let sections = Arc::new(sections);
        let units = ResUnits::parse(&sections)?;
        let sup_units = if let Some(sup) = sections.sup.as_ref() {
            SupUnits::parse(sup)?
        } else {
            SupUnits::default()
        };
        Ok(Context {
            sections,
            units,
            sup_units,
        })
    }
}

impl<R: gimli::Reader> Context<R> {
    /// Find the DWARF unit corresponding to the given virtual memory address.
    pub fn find_dwarf_and_unit(
        &self,
        probe: u64,
    ) -> LookupResult<
        impl LookupContinuation<Output = Option<(&gimli::Dwarf<R>, &gimli::Unit<R>)>, Buf = R>,
    > {
        let mut units_iter = self.units.find(probe);
        if let Some(unit) = units_iter.next() {
            return LoopingLookup::new_lookup(
                unit.find_function_or_location(probe, self),
                move |r| {
                    ControlFlow::Break(match r {
                        Ok((Some(_), _)) | Ok((_, Some(_))) => {
                            let (_file, sections, unit) = unit
                                .dwarf_and_unit_dwo(self)
                                // We've already been through both error cases here to get to this point.
                                .unwrap()
                                .unwrap();
                            Some((sections, unit))
                        }
                        _ => match units_iter.next() {
                            Some(next_unit) => {
                                return ControlFlow::Continue(
                                    next_unit.find_function_or_location(probe, self),
                                );
                            }
                            None => None,
                        },
                    })
                },
            );
        }

        LoopingLookup::new_complete(None)
    }

    /// Find the source file and line corresponding to the given virtual memory address.
    pub fn find_location(&self, probe: u64) -> Result<Option<Location<'_>>, Error> {
        for unit in self.units.find(probe) {
            if let Some(location) = unit.find_location(probe, &self.sections)? {
                return Ok(Some(location));
            }
        }
        Ok(None)
    }

    /// Return source file and lines for a range of addresses. For each location it also
    /// returns the address and size of the range of the underlying instructions.
    pub fn find_location_range(
        &self,
        probe_low: u64,
        probe_high: u64,
    ) -> Result<LocationRangeIter<'_, R>, Error> {
        self.units
            .find_location_range(probe_low, probe_high, &self.sections)
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
    pub fn find_frames(
        &self,
        probe: u64,
    ) -> LookupResult<impl LookupContinuation<Output = Result<FrameIter<'_, R>, Error>, Buf = R>>
    {
        let mut units_iter = self.units.find(probe);
        if let Some(unit) = units_iter.next() {
            LoopingLookup::new_lookup(unit.find_function_or_location(probe, self), move |r| {
                ControlFlow::Break(match r {
                    Err(e) => Err(e),
                    Ok((Some(function), location)) => {
                        let inlined_functions = function.find_inlined_functions(probe);
                        Ok(FrameIter::new_frames(
                            unit,
                            &self.sections,
                            function,
                            inlined_functions,
                            location,
                        ))
                    }
                    Ok((None, Some(location))) => Ok(FrameIter::new_location(location)),
                    Ok((None, None)) => match units_iter.next() {
                        Some(next_unit) => {
                            return ControlFlow::Continue(
                                next_unit.find_function_or_location(probe, self),
                            );
                        }
                        None => Ok(FrameIter::new_empty()),
                    },
                })
            })
        } else {
            LoopingLookup::new_complete(Ok(FrameIter::new_empty()))
        }
    }

    /// Preload units for `probe`.
    ///
    /// The iterator returns pairs of `SplitDwarfLoad`s containing the
    /// information needed to locate and load split DWARF for `probe` and
    /// a matching callback to invoke once that data is available.
    ///
    /// If this method is called, and all of the returned closures are invoked,
    /// addr2line guarantees that any future API call for the address `probe`
    /// will not require the loading of any split DWARF.
    ///
    /// ```no_run
    ///   # use addr2line::*;
    ///   # use std::sync::Arc;
    ///   # let ctx: Context<gimli::EndianRcSlice<gimli::RunTimeEndian>> = todo!();
    ///   # let do_split_dwarf_load = |load: SplitDwarfLoad<gimli::EndianRcSlice<gimli::RunTimeEndian>>| -> Option<Arc<gimli::Dwarf<gimli::EndianRcSlice<gimli::RunTimeEndian>>>> { None };
    ///   const ADDRESS: u64 = 0xdeadbeef;
    ///   ctx.preload_units(ADDRESS).for_each(|(load, callback)| {
    ///     let dwo = do_split_dwarf_load(load);
    ///     callback(dwo);
    ///   });
    ///
    ///   let frames_iter = match ctx.find_frames(ADDRESS) {
    ///     LookupResult::Output(result) => result,
    ///     LookupResult::Load { .. } => unreachable!("addr2line promised we wouldn't get here"),
    ///   };
    ///
    ///   // ...
    /// ```
    pub fn preload_units(
        &'_ self,
        probe: u64,
    ) -> impl Iterator<
        Item = (
            SplitDwarfLoad<R>,
            impl FnOnce(Option<Arc<gimli::Dwarf<R>>>) -> Result<(), gimli::Error> + '_,
        ),
    > {
        self.units
            .find(probe)
            .filter_map(move |unit| match unit.dwarf_and_unit_dwo(self) {
                LookupResult::Output(_) => None,
                LookupResult::Load { load, continuation } => Some((load, |result| {
                    continuation.resume(result).unwrap().map(|_| ())
                })),
            })
    }

    /// Initialize all line data structures. This is used for benchmarks.
    #[doc(hidden)]
    pub fn parse_lines(&self) -> Result<(), Error> {
        for unit in self.units.iter() {
            unit.parse_lines(&self.sections)?;
        }
        Ok(())
    }

    /// Initialize all function data structures. This is used for benchmarks.
    #[doc(hidden)]
    pub fn parse_functions(&self) -> Result<(), Error> {
        for unit in self.units.iter() {
            unit.parse_functions(self).skip_all_loads()?;
        }
        Ok(())
    }

    /// Initialize all inlined function data structures. This is used for benchmarks.
    #[doc(hidden)]
    pub fn parse_inlined_functions(&self) -> Result<(), Error> {
        for unit in self.units.iter() {
            unit.parse_inlined_functions(self).skip_all_loads()?;
        }
        Ok(())
    }
}

impl<R: gimli::Reader> Context<R> {
    // Find the unit containing the given offset, and convert the offset into a unit offset.
    fn find_unit(
        &self,
        offset: gimli::DebugInfoOffset<R::Offset>,
        file: DebugFile,
    ) -> Result<(&gimli::Unit<R>, gimli::UnitOffset<R::Offset>), Error> {
        let unit = match file {
            DebugFile::Primary => self.units.find_offset(offset)?,
            DebugFile::Supplementary => self.sup_units.find_offset(offset)?,
            DebugFile::Dwo => return Err(gimli::Error::NoEntryAtGivenOffset),
        };

        let unit_offset = offset
            .to_unit_offset(&unit.header)
            .ok_or(gimli::Error::NoEntryAtGivenOffset)?;
        Ok((unit, unit_offset))
    }
}

/// This struct contains the information needed to find split DWARF data
/// and to produce a `gimli::Dwarf<R>` for it.
pub struct SplitDwarfLoad<R> {
    /// The dwo id, for looking up in a DWARF package, or for
    /// verifying an unpacked dwo found on the file system
    pub dwo_id: gimli::DwoId,
    /// The compilation directory `path` is relative to.
    pub comp_dir: Option<R>,
    /// A path on the filesystem, relative to `comp_dir` to find this dwo.
    pub path: Option<R>,
    /// Once the split DWARF data is loaded, the loader is expected
    /// to call [make_dwo(parent)](gimli::read::Dwarf::make_dwo) before
    /// returning the data.
    pub parent: Arc<gimli::Dwarf<R>>,
}

struct SimpleLookup<T, R, F>
where
    F: FnOnce(Option<Arc<gimli::Dwarf<R>>>) -> T,
    R: gimli::Reader,
{
    f: F,
    phantom: PhantomData<(T, R)>,
}

impl<T, R, F> SimpleLookup<T, R, F>
where
    F: FnOnce(Option<Arc<gimli::Dwarf<R>>>) -> T,
    R: gimli::Reader,
{
    fn new_complete(t: F::Output) -> LookupResult<SimpleLookup<T, R, F>> {
        LookupResult::Output(t)
    }

    fn new_needs_load(load: SplitDwarfLoad<R>, f: F) -> LookupResult<SimpleLookup<T, R, F>> {
        LookupResult::Load {
            load,
            continuation: SimpleLookup {
                f,
                phantom: PhantomData,
            },
        }
    }
}

impl<T, R, F> LookupContinuation for SimpleLookup<T, R, F>
where
    F: FnOnce(Option<Arc<gimli::Dwarf<R>>>) -> T,
    R: gimli::Reader,
{
    type Output = T;
    type Buf = R;

    fn resume(self, v: Option<Arc<gimli::Dwarf<Self::Buf>>>) -> LookupResult<Self> {
        LookupResult::Output((self.f)(v))
    }
}

struct MappedLookup<T, L, F>
where
    L: LookupContinuation,
    F: FnOnce(L::Output) -> T,
{
    original: L,
    mutator: F,
}

impl<T, L, F> LookupContinuation for MappedLookup<T, L, F>
where
    L: LookupContinuation,
    F: FnOnce(L::Output) -> T,
{
    type Output = T;
    type Buf = L::Buf;

    fn resume(self, v: Option<Arc<gimli::Dwarf<Self::Buf>>>) -> LookupResult<Self> {
        match self.original.resume(v) {
            LookupResult::Output(t) => LookupResult::Output((self.mutator)(t)),
            LookupResult::Load { load, continuation } => LookupResult::Load {
                load,
                continuation: MappedLookup {
                    original: continuation,
                    mutator: self.mutator,
                },
            },
        }
    }
}

/// Some functions (e.g. `find_frames`) require considering multiple
/// compilation units, each of which might require their own split DWARF
/// lookup (and thus produce a continuation).
///
/// We store the underlying continuation here as well as a mutator function
/// that will either a) decide that the result of this continuation is
/// what is needed and mutate it to the final result or b) produce another
/// `LookupResult`. `new_lookup` will in turn eagerly drive any non-continuation
/// `LookupResult` with successive invocations of the mutator, until a new
/// continuation or a final result is produced. And finally, the impl of
/// `LookupContinuation::resume` will call `new_lookup` each time the
/// computation is resumed.
struct LoopingLookup<T, L, F>
where
    L: LookupContinuation,
    F: FnMut(L::Output) -> ControlFlow<T, LookupResult<L>>,
{
    continuation: L,
    mutator: F,
}

impl<T, L, F> LoopingLookup<T, L, F>
where
    L: LookupContinuation,
    F: FnMut(L::Output) -> ControlFlow<T, LookupResult<L>>,
{
    fn new_complete(t: T) -> LookupResult<Self> {
        LookupResult::Output(t)
    }

    fn new_lookup(mut r: LookupResult<L>, mut mutator: F) -> LookupResult<Self> {
        // Drive the loop eagerly so that we only ever have to represent one state
        // (the r == ControlFlow::Continue state) in LoopingLookup.
        loop {
            match r {
                LookupResult::Output(l) => match mutator(l) {
                    ControlFlow::Break(t) => return LookupResult::Output(t),
                    ControlFlow::Continue(r2) => {
                        r = r2;
                    }
                },
                LookupResult::Load { load, continuation } => {
                    return LookupResult::Load {
                        load,
                        continuation: LoopingLookup {
                            continuation,
                            mutator,
                        },
                    };
                }
            }
        }
    }
}

impl<T, L, F> LookupContinuation for LoopingLookup<T, L, F>
where
    L: LookupContinuation,
    F: FnMut(L::Output) -> ControlFlow<T, LookupResult<L>>,
{
    type Output = T;
    type Buf = L::Buf;

    fn resume(self, v: Option<Arc<gimli::Dwarf<Self::Buf>>>) -> LookupResult<Self> {
        let r = self.continuation.resume(v);
        LoopingLookup::new_lookup(r, self.mutator)
    }
}

struct RangeAttributes<R: gimli::Reader> {
    low_pc: Option<u64>,
    high_pc: Option<u64>,
    size: Option<u64>,
    ranges_offset: Option<gimli::RangeListsOffset<<R as gimli::Reader>::Offset>>,
}

impl<R: gimli::Reader> Default for RangeAttributes<R> {
    fn default() -> Self {
        RangeAttributes {
            low_pc: None,
            high_pc: None,
            size: None,
            ranges_offset: None,
        }
    }
}

impl<R: gimli::Reader> RangeAttributes<R> {
    fn for_each_range<F: FnMut(gimli::Range)>(
        &self,
        sections: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        mut f: F,
    ) -> Result<bool, Error> {
        let mut added_any = false;
        let mut add_range = |range: gimli::Range| {
            if range.begin < range.end {
                f(range);
                added_any = true
            }
        };
        if let Some(ranges_offset) = self.ranges_offset {
            let mut range_list = sections.ranges(unit, ranges_offset)?;
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

/// An iterator over function frames.
pub struct FrameIter<'ctx, R>(FrameIterState<'ctx, R>)
where
    R: gimli::Reader;

enum FrameIterState<'ctx, R>
where
    R: gimli::Reader,
{
    Empty,
    Location(Option<Location<'ctx>>),
    Frames(FrameIterFrames<'ctx, R>),
}

struct FrameIterFrames<'ctx, R>
where
    R: gimli::Reader,
{
    unit: &'ctx ResUnit<R>,
    sections: &'ctx gimli::Dwarf<R>,
    function: &'ctx Function<R>,
    inlined_functions: iter::Rev<maybe_small::IntoIter<&'ctx InlinedFunction<R>>>,
    next: Option<Location<'ctx>>,
}

impl<'ctx, R> FrameIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    pub(crate) fn new_empty() -> Self {
        FrameIter(FrameIterState::Empty)
    }

    pub(crate) fn new_location(location: Location<'ctx>) -> Self {
        FrameIter(FrameIterState::Location(Some(location)))
    }

    pub(crate) fn new_frames(
        unit: &'ctx ResUnit<R>,
        sections: &'ctx gimli::Dwarf<R>,
        function: &'ctx Function<R>,
        inlined_functions: maybe_small::Vec<&'ctx InlinedFunction<R>>,
        location: Option<Location<'ctx>>,
    ) -> Self {
        FrameIter(FrameIterState::Frames(FrameIterFrames {
            unit,
            sections,
            function,
            inlined_functions: inlined_functions.into_iter().rev(),
            next: location,
        }))
    }

    /// Advances the iterator and returns the next frame.
    pub fn next(&mut self) -> Result<Option<Frame<'ctx, R>>, Error> {
        let frames = match &mut self.0 {
            FrameIterState::Empty => return Ok(None),
            FrameIterState::Location(location) => {
                // We can't move out of a mutable reference, so use `take` instead.
                let location = location.take();
                self.0 = FrameIterState::Empty;
                return Ok(Some(Frame {
                    dw_die_offset: None,
                    function: None,
                    location,
                }));
            }
            FrameIterState::Frames(frames) => frames,
        };

        let loc = frames.next.take();
        let func = match frames.inlined_functions.next() {
            Some(func) => func,
            None => {
                let frame = Frame {
                    dw_die_offset: Some(frames.function.dw_die_offset),
                    function: frames.function.name.clone().map(|name| FunctionName {
                        name,
                        language: frames.unit.lang,
                    }),
                    location: loc,
                };
                self.0 = FrameIterState::Empty;
                return Ok(Some(frame));
            }
        };

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
        if let Some(call_file) = func.call_file {
            if let Some(lines) = frames.unit.parse_lines(frames.sections)? {
                next.file = lines.file(call_file);
            }
        }
        frames.next = Some(next);

        Ok(Some(Frame {
            dw_die_offset: Some(func.dw_die_offset),
            function: func.name.clone().map(|name| FunctionName {
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
    pub fn raw_name(&self) -> Result<Cow<'_, str>, Error> {
        self.name.to_string_lossy()
    }

    /// The name of this function after demangling (if applicable).
    pub fn demangle(&self) -> Result<Cow<'_, str>, Error> {
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
pub fn demangle_auto(name: Cow<'_, str>, language: Option<gimli::DwLang>) -> Cow<'_, str> {
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
    ///
    /// A value of `Some(0)` indicates the left edge.
    pub column: Option<u32>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn context_is_send() {
        fn assert_is_send<T: Send>() {}
        assert_is_send::<crate::Context<gimli::read::EndianSlice<'_, gimli::LittleEndian>>>();
    }
}
