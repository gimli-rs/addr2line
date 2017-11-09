#![feature(test)]
// FIXME: `get_test_addresses` doesn't work on macos.
#![cfg(not(target_os = "macos"))]

extern crate addr2line;
extern crate test;

use std::env;
use std::path::{self, PathBuf};
use std::process;

fn release_fixture_path() -> PathBuf {
    let mut path = PathBuf::new();
    if let Ok(dir) = env::var("CARGO_MANIFEST_DIR") {
        path.push(dir);
    }
    path.push("fixtures");
    path.push("addr2line-release");
    path
}

/// Obtain a list of addresses contained within the text section of the `target` executable.
fn get_test_addresses(target: &path::Path) -> Vec<u64> {
    let names = process::Command::new("/usr/bin/nm")
        .arg("-S")
        .arg(target)
        .output()
        .expect("failed to execute nm");

    let symbols = String::from_utf8_lossy(&names.stdout);
    let mut addresses = Vec::new();
    for line in symbols.lines().take(200) {
        let fields: Vec<_> = line.split_whitespace().take(4).collect();
        if fields.len() >= 4 && (fields[2] == "T" || fields[2] == "t") {
            let mut addr = u64::from_str_radix(fields[0], 16).unwrap();
            let size = u64::from_str_radix(fields[1], 16).unwrap();
            let end = addr + size;
            while addr < end {
                addresses.push(addr);
                addr += 5;
            }
        }
    }
    addresses
}

// Bench `Mapping::new_inner`.
//
// Does not call `Mapping::locate`, so no locations will be loaded.
#[bench]
fn build_default(b: &mut test::Bencher) {
    let target = release_fixture_path();

    b.iter(|| {
        addr2line::Options::default().build(&target).unwrap();
    });
}

#[bench]
fn build_with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();

    b.iter(|| {
        addr2line::Options::default()
            .with_functions()
            .build(&target)
            .unwrap();
    });
}

// Bench `Mapping::locate`.
#[bench]
fn locate_default(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());
    let mut addr2line = addr2line::Options::default().build(&target).unwrap();
    // Ensure nothing is lazily loaded.
    for addr in &addresses {
        test::black_box(addr2line.locate(*addr)).ok();
    }

    b.iter(|| {
        for addr in &addresses {
            test::black_box(addr2line.locate(*addr)).ok();
        }
    });
}

#[bench]
fn locate_with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());
    let mut addr2line = addr2line::Options::default()
        .with_functions()
        .build(&target)
        .unwrap();
    for addr in &addresses {
        test::black_box(addr2line.locate(*addr).ok());
    }

    b.iter(|| {
        for addr in &addresses {
            test::black_box(addr2line.locate(*addr).ok());
        }
    });
}

// Bench `Mapping::new_inner`. Also calls `Mapping::locate` so that
// some (but not all) lazily loaded locations are loaded.
#[bench]
fn build_and_locate_default(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    b.iter(|| {
        let mut addr2line = addr2line::Options::default().build(&target).unwrap();
        for addr in addresses.iter().take(100) {
            test::black_box(addr2line.locate(*addr).ok());
        }
    });
}

#[bench]
fn build_and_locate_with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    b.iter(|| {
        let mut addr2line = addr2line::Options::default()
            .with_functions()
            .build(&target)
            .unwrap();
        for addr in addresses.iter().take(100) {
            test::black_box(addr2line.locate(*addr)).ok();
        }
    });
}
