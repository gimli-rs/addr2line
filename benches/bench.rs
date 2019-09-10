#![feature(test)]

extern crate addr2line;
extern crate memmap;
extern crate object;
extern crate test;

use std::env;
use std::fs::File;
use std::path::{self, PathBuf};

use object::Object;

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

/// Obtain a list of addresses contained within the text section of the `target` executable.
fn get_test_addresses(target: &object::File) -> Vec<u64> {
    let addresses: Vec<_> =
        target
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
fn context_new_location(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        b.iter(|| {
            addr2line::Context::new(file).unwrap();
        });
    });
}

#[bench]
fn context_query_location(b: &mut test::Bencher) {
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
fn context_query_with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        let ctx = addr2line::Context::new(file).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            test::black_box(ctx.find_frames(*addr)).ok();
        }

        b.iter(|| {
            for addr in &addresses {
                test::black_box(ctx.find_frames(*addr)).ok();
            }
        });
    });
}

#[bench]
fn context_new_and_query_location(b: &mut test::Bencher) {
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
fn context_new_and_query_with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let addresses = get_test_addresses(file);

        b.iter(|| {
            let ctx = addr2line::Context::new(file).unwrap();
            for addr in addresses.iter().take(100) {
                test::black_box(ctx.find_frames(*addr)).ok();
            }
        });
    });
}
