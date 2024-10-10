use criterion::{black_box, criterion_group, criterion_main, Bencher, Criterion};

use std::borrow::Cow;
use std::env;
use std::fs::File;
use std::path::{self, PathBuf};

use object::{Object, ObjectSection, ObjectSymbol};

fn release_fixture_path() -> PathBuf {
    let mut path = PathBuf::new();
    if let Ok(dir) = env::var("CARGO_MANIFEST_DIR") {
        path.push(dir);
    }
    path.push("fixtures");
    path.push("addr2line-release");
    path
}

fn with_file<F: FnOnce(&object::File<'_>)>(target: &path::Path, f: F) {
    let file = File::open(target).unwrap();
    let map = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let file = object::File::parse(&*map).unwrap();
    f(&file)
}

fn dwarf_load<'a>(object: &object::File<'a>) -> gimli::DwarfSections<Cow<'a, [u8]>> {
    let load_section = |id: gimli::SectionId| -> Result<Cow<'a, [u8]>, gimli::Error> {
        Ok(object
            .section_by_name(id.name())
            .map(|section| section.uncompressed_data().unwrap())
            .unwrap_or(Cow::Borrowed(&[][..])))
    };
    gimli::DwarfSections::load(&load_section).unwrap()
}

fn dwarf_borrow<'a>(
    dwarf: &'a gimli::DwarfSections<Cow<'_, [u8]>>,
) -> gimli::Dwarf<gimli::EndianSlice<'a, gimli::LittleEndian>> {
    let borrow_section: &dyn for<'b> Fn(
        &'b Cow<'_, [u8]>,
    ) -> gimli::EndianSlice<'b, gimli::LittleEndian> =
        &|section| gimli::EndianSlice::new(section, gimli::LittleEndian);
    dwarf.borrow(&borrow_section)
}

/// Obtain a list of addresses contained within the text section of the `target` executable.
fn get_test_addresses(target: &object::File<'_>) -> Vec<u64> {
    let addresses: Vec<_> = target
        .symbols()
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

fn context_new(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            addr2line::Context::from_dwarf(dwarf).unwrap();
        });
    });
}

fn bench_context_new(c: &mut Criterion) {
    c.bench_function("context_new", context_new);
}

fn context_new_parse_lines(b: &mut Bencher) {
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

fn bench_context_new_parse_lines(c: &mut Criterion) {
    c.bench_function("context_new_parse_lines", context_new_parse_lines);
}

fn context_new_parse_functions(b: &mut Bencher) {
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

fn bench_context_new_parse_functions(c: &mut Criterion) {
    c.bench_function("context_new_parse_functions", context_new_parse_functions);
}

fn context_new_parse_inlined_functions(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let context = addr2line::Context::from_dwarf(dwarf).unwrap();
            context.parse_inlined_functions().unwrap();
        });
    });
}

fn bench_context_new_parse_inlined_functions(c: &mut Criterion) {
    c.bench_function(
        "context_new_parse_inlined_functions",
        context_new_parse_inlined_functions,
    );
}

fn context_query_location(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let dwarf = dwarf_load(file);
        let dwarf = dwarf_borrow(&dwarf);
        let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            black_box(ctx.find_location(*addr)).ok();
        }

        b.iter(|| {
            for addr in &addresses {
                black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

fn bench_context_query_location(c: &mut Criterion) {
    c.bench_function("context_query_location", context_query_location);
}

fn context_query_with_functions(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let dwarf = dwarf_load(file);
        let dwarf = dwarf_borrow(&dwarf);
        let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            let mut frames = ctx.find_frames(*addr).skip_all_loads().unwrap();
            while let Ok(Some(ref frame)) = frames.next() {
                black_box(frame);
            }
        }

        b.iter(|| {
            for addr in &addresses {
                let mut frames = ctx.find_frames(*addr).skip_all_loads().unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    black_box(frame);
                }
            }
        });
    });
}

fn bench_context_query_with_functions(c: &mut Criterion) {
    c.bench_function("context_query_with_functions", context_query_with_functions);
}

fn context_new_and_query_location(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
            for addr in addresses.iter().take(100) {
                black_box(ctx.find_location(*addr)).ok();
            }
        });
    });
}

fn bench_context_new_and_query_location(c: &mut Criterion) {
    c.bench_function(
        "context_new_and_query_location",
        context_new_and_query_location,
    );
}

fn context_new_and_query_with_functions(b: &mut Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let dwarf = dwarf_load(file);
            let dwarf = dwarf_borrow(&dwarf);
            let ctx = addr2line::Context::from_dwarf(dwarf).unwrap();
            for addr in addresses.iter().take(100) {
                let mut frames = ctx.find_frames(*addr).skip_all_loads().unwrap();
                while let Ok(Some(ref frame)) = frames.next() {
                    black_box(frame);
                }
            }
        });
    });
}

fn bench_context_new_and_query_with_functions(c: &mut Criterion) {
    c.bench_function(
        "context_new_and_query_with_functions",
        context_new_and_query_with_functions,
    );
}

criterion_group!(
    benches,
    bench_context_new,
    bench_context_new_parse_lines,
    bench_context_new_parse_functions,
    bench_context_new_parse_inlined_functions,
    bench_context_query_location,
    bench_context_query_with_functions,
    bench_context_new_and_query_location,
    bench_context_new_and_query_with_functions,
);
criterion_main!(benches);
