use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::cmp;

use crate::lazy::LazyResult;
use crate::{
    Context, DebugFile, Error, Function, Functions, LineLocationRangeIter, Lines, Location,
    LookupContinuation, LookupResult, RangeAttributes, SimpleLookup, SplitDwarfLoad,
};

pub(crate) struct UnitRange {
    unit_id: usize,
    max_end: u64,
    range: gimli::Range,
}

pub(crate) struct ResUnit<R: gimli::Reader> {
    offset: gimli::DebugInfoOffset<R::Offset>,
    dw_unit: gimli::Unit<R>,
    pub(crate) lang: Option<gimli::DwLang>,
    lines: LazyResult<Lines>,
    funcs: LazyResult<Functions<R>>,
    dwo: LazyResult<Option<Box<DwoUnit<R>>>>,
}

type UnitRef<'unit, R> = (DebugFile, &'unit gimli::Dwarf<R>, &'unit gimli::Unit<R>);

impl<R: gimli::Reader> ResUnit<R> {
    pub(crate) fn dwarf_and_unit_dwo<'unit, 'ctx: 'unit>(
        &'unit self,
        ctx: &'ctx Context<R>,
    ) -> LookupResult<
        SimpleLookup<
            Result<UnitRef<'unit, R>, Error>,
            R,
            impl FnOnce(Option<Arc<gimli::Dwarf<R>>>) -> Result<UnitRef<'unit, R>, Error>,
        >,
    > {
        loop {
            break SimpleLookup::new_complete(match self.dwo.borrow() {
                Some(Ok(Some(dwo))) => Ok((DebugFile::Dwo, &*dwo.sections, &dwo.dw_unit)),
                Some(Ok(None)) => Ok((DebugFile::Primary, &*ctx.sections, &self.dw_unit)),
                Some(Err(e)) => Err(*e),
                None => {
                    let dwo_id = match self.dw_unit.dwo_id {
                        None => {
                            self.dwo.borrow_with(|| Ok(None));
                            continue;
                        }
                        Some(dwo_id) => dwo_id,
                    };

                    let comp_dir = self.dw_unit.comp_dir.clone();

                    let dwo_name = self.dw_unit.dwo_name().and_then(|s| {
                        if let Some(s) = s {
                            Ok(Some(ctx.sections.attr_string(&self.dw_unit, s)?))
                        } else {
                            Ok(None)
                        }
                    });

                    let path = match dwo_name {
                        Ok(v) => v,
                        Err(e) => {
                            self.dwo.borrow_with(|| Err(e));
                            continue;
                        }
                    };

                    let process_dwo = move |dwo_dwarf: Option<Arc<gimli::Dwarf<R>>>| {
                        let dwo_dwarf = match dwo_dwarf {
                            None => return Ok(None),
                            Some(dwo_dwarf) => dwo_dwarf,
                        };
                        let mut dwo_units = dwo_dwarf.units();
                        let dwo_header = match dwo_units.next()? {
                            Some(dwo_header) => dwo_header,
                            None => return Ok(None),
                        };

                        let mut dwo_unit = dwo_dwarf.unit(dwo_header)?;
                        dwo_unit.copy_relocated_attributes(&self.dw_unit);
                        Ok(Some(Box::new(DwoUnit {
                            sections: dwo_dwarf,
                            dw_unit: dwo_unit,
                        })))
                    };

                    return SimpleLookup::new_needs_load(
                        SplitDwarfLoad {
                            dwo_id,
                            comp_dir,
                            path,
                            parent: ctx.sections.clone(),
                        },
                        move |dwo_dwarf| match self.dwo.borrow_with(|| process_dwo(dwo_dwarf)) {
                            Ok(Some(dwo)) => Ok((DebugFile::Dwo, &*dwo.sections, &dwo.dw_unit)),
                            Ok(None) => Ok((DebugFile::Primary, &*ctx.sections, &self.dw_unit)),
                            Err(e) => Err(*e),
                        },
                    );
                }
            });
        }
    }

    pub(crate) fn parse_lines(&self, sections: &gimli::Dwarf<R>) -> Result<Option<&Lines>, Error> {
        // NB: line information is always stored in the main debug file so this does not need
        // to handle DWOs.
        let ilnp = match self.dw_unit.line_program {
            Some(ref ilnp) => ilnp,
            None => return Ok(None),
        };
        self.lines
            .borrow_with(|| Lines::parse(&self.dw_unit, ilnp.clone(), sections))
            .as_ref()
            .map(Some)
            .map_err(Error::clone)
    }

    fn parse_functions_dwarf_and_unit(
        &self,
        unit: &gimli::Unit<R>,
        sections: &gimli::Dwarf<R>,
    ) -> Result<&Functions<R>, Error> {
        self.funcs
            .borrow_with(|| Functions::parse(unit, sections))
            .as_ref()
            .map_err(Error::clone)
    }

    pub(crate) fn parse_functions<'unit, 'ctx: 'unit>(
        &'unit self,
        ctx: &'ctx Context<R>,
    ) -> LookupResult<impl LookupContinuation<Output = Result<&'unit Functions<R>, Error>, Buf = R>>
    {
        self.dwarf_and_unit_dwo(ctx).map(move |r| {
            let (_file, sections, unit) = r?;
            self.parse_functions_dwarf_and_unit(unit, sections)
        })
    }

    pub(crate) fn parse_inlined_functions<'unit, 'ctx: 'unit>(
        &'unit self,
        ctx: &'ctx Context<R>,
    ) -> LookupResult<impl LookupContinuation<Output = Result<(), Error>, Buf = R> + 'unit> {
        self.dwarf_and_unit_dwo(ctx).map(move |r| {
            let (file, sections, unit) = r?;
            self.funcs
                .borrow_with(|| Functions::parse(unit, sections))
                .as_ref()
                .map_err(Error::clone)?
                .parse_inlined_functions(file, unit, ctx, sections)
        })
    }

    pub(crate) fn find_location(
        &self,
        probe: u64,
        sections: &gimli::Dwarf<R>,
    ) -> Result<Option<Location<'_>>, Error> {
        let Some(lines) = self.parse_lines(sections)? else {
            return Ok(None);
        };
        let mut iter = lines.location_ranges(probe, probe + 1)?;
        match iter.next() {
            None => Ok(None),
            Some((_addr, _len, loc)) => Ok(Some(loc)),
        }
    }

    #[inline]
    pub(crate) fn find_location_range(
        &self,
        probe_low: u64,
        probe_high: u64,
        sections: &gimli::Dwarf<R>,
    ) -> Result<Option<LineLocationRangeIter<'_>>, Error> {
        let Some(lines) = self.parse_lines(sections)? else {
            return Ok(None);
        };
        lines.location_ranges(probe_low, probe_high).map(Some)
    }

    pub(crate) fn find_function_or_location<'unit, 'ctx: 'unit>(
        &'unit self,
        probe: u64,
        ctx: &'ctx Context<R>,
    ) -> LookupResult<
        impl LookupContinuation<
            Output = Result<(Option<&'unit Function<R>>, Option<Location<'unit>>), Error>,
            Buf = R,
        >,
    > {
        self.dwarf_and_unit_dwo(ctx).map(move |r| {
            let (file, sections, unit) = r?;
            let functions = self.parse_functions_dwarf_and_unit(unit, sections)?;
            let function = match functions.find_address(probe) {
                Some(address) => {
                    let function_index = functions.addresses[address].function;
                    let (offset, ref function) = functions.functions[function_index];
                    Some(
                        function
                            .borrow_with(|| Function::parse(offset, file, unit, ctx, sections))
                            .as_ref()
                            .map_err(Error::clone)?,
                    )
                }
                None => None,
            };
            let location = self.find_location(probe, sections)?;
            Ok((function, location))
        })
    }
}

pub(crate) struct ResUnits<R: gimli::Reader> {
    ranges: Box<[UnitRange]>,
    units: Box<[ResUnit<R>]>,
}

impl<R: gimli::Reader> ResUnits<R> {
    pub(crate) fn parse(sections: &gimli::Dwarf<R>) -> Result<Self, Error> {
        // Find all the references to compilation units in .debug_aranges.
        // Note that we always also iterate through all of .debug_info to
        // find compilation units, because .debug_aranges may be missing some.
        let mut aranges = Vec::new();
        let mut headers = sections.debug_aranges.headers();
        while let Some(header) = headers.next()? {
            aranges.push((header.debug_info_offset(), header.offset()));
        }
        aranges.sort_by_key(|i| i.0);

        let mut unit_ranges = Vec::new();
        let mut res_units = Vec::new();
        let mut units = sections.units();
        while let Some(header) = units.next()? {
            let unit_id = res_units.len();
            let offset = match header.offset().as_debug_info_offset() {
                Some(offset) => offset,
                None => continue,
            };
            // We mainly want compile units, but we may need to follow references to entries
            // within other units for function names.  We don't need anything from type units.
            match header.type_() {
                gimli::UnitType::Type { .. } | gimli::UnitType::SplitType { .. } => continue,
                _ => {}
            }
            let dw_unit = match sections.unit(header) {
                Ok(dw_unit) => dw_unit,
                Err(_) => continue,
            };

            let mut lang = None;
            let mut have_unit_range = false;
            {
                let mut entries = dw_unit.entries_raw(None)?;

                let abbrev = match entries.read_abbreviation()? {
                    Some(abbrev) => abbrev,
                    None => continue,
                };

                let mut ranges = RangeAttributes::default();
                for spec in abbrev.attributes() {
                    let attr = entries.read_attribute(*spec)?;
                    match attr.name() {
                        gimli::DW_AT_low_pc => match attr.value() {
                            gimli::AttributeValue::Addr(val) => ranges.low_pc = Some(val),
                            gimli::AttributeValue::DebugAddrIndex(index) => {
                                ranges.low_pc = Some(sections.address(&dw_unit, index)?);
                            }
                            _ => {}
                        },
                        gimli::DW_AT_high_pc => match attr.value() {
                            gimli::AttributeValue::Addr(val) => ranges.high_pc = Some(val),
                            gimli::AttributeValue::DebugAddrIndex(index) => {
                                ranges.high_pc = Some(sections.address(&dw_unit, index)?);
                            }
                            gimli::AttributeValue::Udata(val) => ranges.size = Some(val),
                            _ => {}
                        },
                        gimli::DW_AT_ranges => {
                            ranges.ranges_offset =
                                sections.attr_ranges_offset(&dw_unit, attr.value())?;
                        }
                        gimli::DW_AT_language => {
                            if let gimli::AttributeValue::Language(val) = attr.value() {
                                lang = Some(val);
                            }
                        }
                        _ => {}
                    }
                }

                // Find the address ranges for the CU, using in order of preference:
                // - DW_AT_ranges
                // - .debug_aranges
                // - DW_AT_low_pc/DW_AT_high_pc
                //
                // Using DW_AT_ranges before .debug_aranges is possibly an arbitrary choice,
                // but the feeling is that DW_AT_ranges is more likely to be reliable or complete
                // if it is present.
                //
                // .debug_aranges must be used before DW_AT_low_pc/DW_AT_high_pc because
                // it has been observed on macOS that DW_AT_ranges was not emitted even for
                // discontiguous CUs.
                let i = match ranges.ranges_offset {
                    Some(_) => None,
                    None => aranges.binary_search_by_key(&offset, |x| x.0).ok(),
                };
                if let Some(mut i) = i {
                    // There should be only one set per CU, but in practice multiple
                    // sets have been observed. This is probably a compiler bug, but
                    // either way we need to handle it.
                    while i > 0 && aranges[i - 1].0 == offset {
                        i -= 1;
                    }
                    for (_, aranges_offset) in aranges[i..].iter().take_while(|x| x.0 == offset) {
                        let aranges_header = sections.debug_aranges.header(*aranges_offset)?;
                        let mut aranges = aranges_header.entries();
                        while let Some(arange) = aranges.next()? {
                            if arange.length() != 0 {
                                unit_ranges.push(UnitRange {
                                    range: arange.range(),
                                    unit_id,
                                    max_end: 0,
                                });
                                have_unit_range = true;
                            }
                        }
                    }
                } else {
                    have_unit_range |= ranges.for_each_range(sections, &dw_unit, |range| {
                        unit_ranges.push(UnitRange {
                            range,
                            unit_id,
                            max_end: 0,
                        });
                    })?;
                }
            }

            let lines = LazyResult::new();
            if !have_unit_range {
                // The unit did not declare any ranges.
                // Try to get some ranges from the line program sequences.
                if let Some(ref ilnp) = dw_unit.line_program {
                    if let Ok(lines) = lines
                        .borrow_with(|| Lines::parse(&dw_unit, ilnp.clone(), sections))
                        .as_ref()
                    {
                        for range in lines.ranges() {
                            unit_ranges.push(UnitRange {
                                range,
                                unit_id,
                                max_end: 0,
                            })
                        }
                    }
                }
            }

            res_units.push(ResUnit {
                offset,
                dw_unit,
                lang,
                lines,
                funcs: LazyResult::new(),
                dwo: LazyResult::new(),
            });
        }

        // Sort this for faster lookup in `find_unit_and_address` below.
        unit_ranges.sort_by_key(|i| i.range.begin);

        // Calculate the `max_end` field now that we've determined the order of
        // CUs.
        let mut max = 0;
        for i in unit_ranges.iter_mut() {
            max = max.max(i.range.end);
            i.max_end = max;
        }

        Ok(ResUnits {
            ranges: unit_ranges.into_boxed_slice(),
            units: res_units.into_boxed_slice(),
        })
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &ResUnit<R>> {
        self.units.iter()
    }

    pub(crate) fn find_offset(
        &self,
        offset: gimli::DebugInfoOffset<R::Offset>,
    ) -> Result<&gimli::Unit<R>, Error> {
        match self
            .units
            .binary_search_by_key(&offset.0, |unit| unit.offset.0)
        {
            // There is never a DIE at the unit offset or before the first unit.
            Ok(_) | Err(0) => Err(gimli::Error::NoEntryAtGivenOffset),
            Err(i) => Ok(&self.units[i - 1].dw_unit),
        }
    }

    /// Finds the CUs for the function address given.
    ///
    /// There might be multiple CUs whose range contains this address.
    /// Weak symbols have shown up in the wild which cause this to happen
    /// but otherwise this can happen if the CU has non-contiguous functions
    /// but only reports a single range.
    ///
    /// Consequently we return an iterator for all CUs which may contain the
    /// address, and the caller must check if there is actually a function or
    /// location in the CU for that address.
    pub(crate) fn find(&self, probe: u64) -> impl Iterator<Item = &ResUnit<R>> {
        self.find_range(probe, probe + 1).map(|(unit, _range)| unit)
    }

    /// Finds the CUs covering the range of addresses given.
    ///
    /// The range is [low, high) (ie, the upper bound is exclusive). This can return multiple
    /// ranges for the same unit.
    #[inline]
    pub(crate) fn find_range(
        &self,
        probe_low: u64,
        probe_high: u64,
    ) -> impl Iterator<Item = (&ResUnit<R>, &gimli::Range)> {
        // First up find the position in the array which could have our function
        // address.
        let pos = match self
            .ranges
            .binary_search_by_key(&probe_high, |i| i.range.begin)
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
        self.ranges[..pos]
            .iter()
            .rev()
            .take_while(move |i| {
                // We know that this CU's start is beneath the probe already because
                // of our sorted array.
                debug_assert!(i.range.begin <= probe_high);

                // Each entry keeps track of the maximum end address seen so far,
                // starting from the beginning of the array of unit ranges. We're
                // iterating in reverse so if our probe is beyond the maximum range
                // of this entry, then it's guaranteed to not fit in any prior
                // entries, so we break out.
                probe_low < i.max_end
            })
            .filter_map(move |i| {
                // If this CU doesn't actually contain this address, move to the
                // next CU.
                if probe_low >= i.range.end || probe_high <= i.range.begin {
                    return None;
                }
                Some((&self.units[i.unit_id], &i.range))
            })
    }

    pub(crate) fn find_location_range<'a>(
        &'a self,
        probe_low: u64,
        probe_high: u64,
        sections: &'a gimli::Dwarf<R>,
    ) -> Result<LocationRangeIter<'a, R>, Error> {
        let unit_iter = Box::new(self.find_range(probe_low, probe_high));
        Ok(LocationRangeIter {
            unit_iter,
            iter: None,
            probe_low,
            probe_high,
            sections,
        })
    }
}

/// A DWO unit has its own DWARF sections.
struct DwoUnit<R: gimli::Reader> {
    sections: Arc<gimli::Dwarf<R>>,
    dw_unit: gimli::Unit<R>,
}

pub(crate) struct SupUnit<R: gimli::Reader> {
    offset: gimli::DebugInfoOffset<R::Offset>,
    dw_unit: gimli::Unit<R>,
}

pub(crate) struct SupUnits<R: gimli::Reader> {
    units: Box<[SupUnit<R>]>,
}

impl<R: gimli::Reader> Default for SupUnits<R> {
    fn default() -> Self {
        SupUnits {
            units: Box::default(),
        }
    }
}

impl<R: gimli::Reader> SupUnits<R> {
    pub(crate) fn parse(sections: &gimli::Dwarf<R>) -> Result<Self, Error> {
        let mut sup_units = Vec::new();
        let mut units = sections.units();
        while let Some(header) = units.next()? {
            let offset = match header.offset().as_debug_info_offset() {
                Some(offset) => offset,
                None => continue,
            };
            let dw_unit = match sections.unit(header) {
                Ok(dw_unit) => dw_unit,
                Err(_) => continue,
            };
            sup_units.push(SupUnit { dw_unit, offset });
        }
        Ok(SupUnits {
            units: sup_units.into_boxed_slice(),
        })
    }

    pub(crate) fn find_offset(
        &self,
        offset: gimli::DebugInfoOffset<R::Offset>,
    ) -> Result<&gimli::Unit<R>, Error> {
        match self
            .units
            .binary_search_by_key(&offset.0, |unit| unit.offset.0)
        {
            // There is never a DIE at the unit offset or before the first unit.
            Ok(_) | Err(0) => Err(gimli::Error::NoEntryAtGivenOffset),
            Err(i) => Ok(&self.units[i - 1].dw_unit),
        }
    }
}

/// Iterator over `Location`s in a range of addresses, returned by `Context::find_location_range`.
pub struct LocationRangeIter<'ctx, R: gimli::Reader> {
    unit_iter: Box<dyn Iterator<Item = (&'ctx ResUnit<R>, &'ctx gimli::Range)> + 'ctx>,
    iter: Option<LineLocationRangeIter<'ctx>>,

    probe_low: u64,
    probe_high: u64,
    sections: &'ctx gimli::Dwarf<R>,
}

impl<'ctx, R: gimli::Reader> LocationRangeIter<'ctx, R> {
    fn next_loc(&mut self) -> Result<Option<(u64, u64, Location<'ctx>)>, Error> {
        loop {
            let iter = self.iter.take();
            match iter {
                None => match self.unit_iter.next() {
                    Some((unit, range)) => {
                        self.iter = unit.find_location_range(
                            cmp::max(self.probe_low, range.begin),
                            cmp::min(self.probe_high, range.end),
                            self.sections,
                        )?;
                    }
                    None => return Ok(None),
                },
                Some(mut iter) => {
                    if let item @ Some(_) = iter.next() {
                        self.iter = Some(iter);
                        return Ok(item);
                    }
                }
            }
        }
    }
}

impl<'ctx, R> Iterator for LocationRangeIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    type Item = (u64, u64, Location<'ctx>);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_loc() {
            Err(_) => None,
            Ok(loc) => loc,
        }
    }
}

#[cfg(feature = "fallible-iterator")]
impl<'ctx, R> fallible_iterator::FallibleIterator for LocationRangeIter<'ctx, R>
where
    R: gimli::Reader + 'ctx,
{
    type Item = (u64, u64, Location<'ctx>);
    type Error = Error;

    #[inline]
    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.next_loc()
    }
}
