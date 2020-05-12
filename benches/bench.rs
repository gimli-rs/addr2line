#![feature(test)]

extern crate addr2line;
extern crate memmap;
extern crate object;
extern crate test;

use std::borrow::Cow;
use std::env;
use std::fs::File;
use std::path::{self, PathBuf};

use object::{Object, ObjectSection};

fn release_fixture_path() -> PathBuf {
    let mut path = PathBuf::new();
    if let Ok(dir) = env::var("CARGO_MANIFEST_DIR") {
        path.push(dir);
    }
    path.push("fixtures");
    path.push("addr2line-release");
    path
}

fn with_file<F: FnOnce(&object::File)>(target: &path::Path, f: F) {
    let file = File::open(target).unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = object::File::parse(&*map).unwrap();
    f(&file)
}

fn dwarf_load<'a>(object: &object::File<'a>) -> gimli::Dwarf<Cow<'a, [u8]>> {
    let load_section = |id: gimli::SectionId| -> Result<Cow<'a, [u8]>, gimli::Error> {
        Ok(object
            .section_by_name(id.name())
            .map(|section| section.uncompressed_data().unwrap())
            .unwrap_or(Cow::Borrowed(&[][..])))
    };
    let load_section_sup = |_| Ok(Cow::Borrowed(&[][..]));
    gimli::Dwarf::load(&load_section, &load_section_sup).unwrap()
}

fn dwarf_borrow<'a>(
    dwarf: &'a gimli::Dwarf<Cow<[u8]>>,
) -> gimli::Dwarf<gimli::EndianSlice<'a, gimli::LittleEndian>> {
    let borrow_section: &dyn for<'b> Fn(
        &'b Cow<[u8]>,
    ) -> gimli::EndianSlice<'b, gimli::LittleEndian> =
        &|section| gimli::EndianSlice::new(&*section, gimli::LittleEndian);
    dwarf.borrow(&borrow_section)
}

/// Obtain a list of addresses contained within the text section of the `target` executable.
fn get_test_addresses(target: &object::File) -> Vec<u64> {
    let addresses: Vec<_> = target
        .symbols()
        .map(|(_, s)| s)
        .filter(|s| s.kind() == object::SymbolKind::Text && s.address() != 0 && s.size() != 0)
        .take(200)
        .flat_map(|s| {
            let addr = s.address();
            let size = s.size();
            let end = addr + size;
            (addr..end).step_by(5)
        })
        .collect();

    assert!(!addresses.is_empty());

    addresses
}

#[bench]
fn context_new_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            addr2line::Context::new(file).unwrap();
        });
    });
}

#[bench]
fn context_new_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            addr2line::Context::from_dwarf(dwarf).unwrap();
        });
    });
}

#[bench]
fn context_new_parse_lines_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let context = addr2line::Context::new(file).unwrap();
            context.parse_lines().unwrap();
        });
    });
}

#[bench]
fn context_new_parse_lines_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let context = addr2line::Context::from_dwarf(dwarf).unwrap();
            context.parse_lines().unwrap();
        });
    });
}

#[bench]
fn context_new_parse_functions_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let context = addr2line::Context::new(file).unwrap();
            context.parse_functions().unwrap();
        });
    });
}

#[bench]
fn context_new_parse_functions_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let context = addr2line::Context::from_dwarf(dwarf).unwrap();
            context.parse_functions().unwrap();
        });
    });
}

#[bench]
fn context_query_location_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let ctx = addr2line::Context::new(file).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            test::black_box(ctx.find_location(*addr)).ok();
        }

        b.iter(|| {
            for addr in &addresses {
                test::black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

#[bench]
fn context_query_location_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let dwarf = dwarf_load(file);
        let dwarf = dwarf_borrow(&dwarf);
        let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            test::black_box(ctx.find_location(*addr)).ok();
        }

        b.iter(|| {
            for addr in &addresses {
                test::black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

#[bench]
fn context_query_with_functions_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let ctx = addr2line::Context::new(file).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            let mut frames = ctx.find_frames(*addr).unwrap();
            while let Ok(Some(ref frame)) = frames.next() {
                test::black_box(frame);
            }
        }

        b.iter(|| {
            for addr in &addresses {
                let mut frames = ctx.find_frames(*addr).unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    test::black_box(frame);
                }
            }
        });
    });
}

#[bench]
fn context_query_with_functions_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let dwarf = dwarf_load(file);
        let dwarf = dwarf_borrow(&dwarf);
        let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            let mut frames = ctx.find_frames(*addr).unwrap();
            while let Ok(Some(ref frame)) = frames.next() {
                test::black_box(frame);
            }
        }

        b.iter(|| {
            for addr in &addresses {
                let mut frames = ctx.find_frames(*addr).unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    test::black_box(frame);
                }
            }
        });
    });
}

#[bench]
fn context_new_and_query_location_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let ctx = addr2line::Context::new(file).unwrap();
            for addr in addresses.iter().take(100) {
                test::black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

#[bench]
fn context_new_and_query_location_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
            for addr in addresses.iter().take(100) {
                test::black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

#[bench]
fn context_new_and_query_with_functions_rc(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let ctx = addr2line::Context::new(file).unwrap();
            for addr in addresses.iter().take(100) {
                let mut frames = ctx.find_frames(*addr).unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    test::black_box(frame);
                }
            }
        });
    });
}
#[bench]
fn context_new_and_query_with_functions_slice(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
            for addr in addresses.iter().take(100) {
                let mut frames = ctx.find_frames(*addr).unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    test::black_box(frame);
                }
            }
        });
    });
}
