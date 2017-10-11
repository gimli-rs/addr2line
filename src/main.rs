extern crate memmap;
extern crate object;
extern crate intervaltree;
extern crate fallible_iterator;
extern crate gimli;

use intervaltree::IntervalTree;

struct Func {
    unit_off: gimli::DebugInfoOffset,
    entry_off: gimli::UnitOffset,
    depth: isize,
}

fn main() {
    //let path = std::env::args().skip(1).next().unwrap();

    let map = memmap::Mmap::open_path(path, memmap::Protection::Read).unwrap();
    let file = &object::File::parse(unsafe { map.as_slice() }).unwrap();
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

    let debug_abbrev: &gimli::DebugAbbrev<_> = &load_section(file, endian);
    let debug_aranges: &gimli::DebugAranges<_> = &load_section(file, endian);
    let debug_info: &gimli::DebugInfo<_> = &load_section(file, endian);
    let debug_line: &gimli::DebugLine<_> = &load_section(file, endian);
    let debug_loc: &gimli::DebugLoc<_> = &load_section(file, endian);
    let debug_pubnames: &gimli::DebugPubNames<_> = &load_section(file, endian);
    let debug_pubtypes: &gimli::DebugPubTypes<_> = &load_section(file, endian);
    let debug_ranges: &gimli::DebugRanges<_> = &load_section(file, endian);
    let debug_str: &gimli::DebugStr<_> = &load_section(file, endian);
    let debug_types: &gimli::DebugTypes<_> = &load_section(file, endian);

    let mut results = Vec::new();
    let mut units = debug_info.units();
    while let Some(dw_unit) = units.next().unwrap() {
        let abbrevs = dw_unit.abbreviations(debug_abbrev).unwrap();

        let mut depth = 0;
        let mut res_ranges = Vec::new();
        let mut cursor = dw_unit.entries(&abbrevs);
        while let Some((d, entry)) = cursor.next_dfs().unwrap() {
            depth += d;
            match entry.tag() {
                gimli::DW_TAG_compile_unit => {
                    // DW_AT_stmt_list
                    // DW_AT_low_pc ?
                    // DW_AT_comp_dir
                }
                gimli::DW_TAG_namespace => (), // TODO
                gimli::DW_TAG_subprogram | gimli::DW_TAG_inlined_subroutine => {
                    //let name = str_attr(entry, gimli::DW_AT_linkage_name).unwrap().to_string_lossy();
                    //println!("sub {}", name);

                    res_ranges.clear();
                    match entry.attr_value(gimli::DW_AT_ranges).unwrap() {
                        None => {
                            let low_pc = match entry.attr_value(gimli::DW_AT_low_pc).unwrap() {
                                Some(gimli::AttributeValue::Addr(low_pc)) => low_pc,
                                None => {
                                    // neither ranges nor low_pc => inline-only function
                                    continue;
                                }
                                _ => unreachable!(),
                            };
                            let high_pc = match entry.attr_value(gimli::DW_AT_high_pc).unwrap().unwrap() {
                                gimli::AttributeValue::Addr(high_pc) => high_pc,
                                gimli::AttributeValue::Udata(x) => low_pc + x,
                                _ => unreachable!(),
                            };
                            res_ranges.push(gimli::Range { begin: low_pc, end: high_pc });
                        },
                        Some(gimli::AttributeValue::DebugRangesRef(rr)) => {
                            let mut ranges = debug_ranges.ranges(rr, dw_unit.address_size(), 0 /* fixme unit base addr */).unwrap();
                            while let Some(range) = ranges.next().unwrap() {
                                res_ranges.push(range);
                            }
                        }
                        _ => unreachable!(),
                    }

                    for range in &res_ranges {
                        //results.push((range.begin .. range.end, name.clone()));
                        results.push((range.begin .. range.end, Func {
                            unit_off: dw_unit.offset(),
                            entry_off: entry.offset(),
                            depth,
                        }));
                    }
                }
                _ => (),
            }
        }
    }

    let tree: IntervalTree<_, _> = results.into_iter().collect();

    let probes = &[0x84ee, 0x8643, 0x7095, 0x1e05c, 0x6ca7, 0x6f57, 0x5722b];
    for &probe in probes {
        let mut res: Vec<_> = tree.query_point(probe).collect();
        res.sort_by_key(|x| -x.depth);
        for &Func { unit_off, entry_off, .. } in res {
            let dw_unit = debug_info.header_from_offset(unit_off).unwrap();
            let abbrevs = dw_unit.abbreviations(debug_abbrev).unwrap();
            let mut cursor = dw_unit.entries_at_offset(&abbrevs, entry_off).unwrap();

            let str_attr = |entry: &gimli::DebuggingInformationEntry<_>, name| {
                match entry.attr(name).unwrap() {
                    Some(x) => Some(x),
                    None => {
                        match entry.attr_value(gimli::DW_AT_abstract_origin).unwrap() {
                            Some(gimli::AttributeValue::UnitRef(offset)) => {
                                let mut tcursor = dw_unit.entries_at_offset(&abbrevs, offset).unwrap();
                                let (_, entry) = tcursor.next_dfs().unwrap().unwrap();

                                entry.attr(name).unwrap()
                            }
                            None => None,
                            x => panic!("wat {:?}", x),
                        }
                    }
                }.map(|x| x.string_value(debug_str).unwrap())
            };

            let (_, entry) = cursor.next_dfs().unwrap().unwrap();
            let name = str_attr(entry, gimli::DW_AT_linkage_name).unwrap().to_string_lossy();
            println!("aka {}", name);
        }
        println!();
        //println!("{:?}", tree.query_point(probe));
    }
}
