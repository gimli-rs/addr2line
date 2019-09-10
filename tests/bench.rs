#![feature(test)]
// FIXME: `get_test_addresses` doesn't work on macos.
#![cfg(not(target_os = "macos"))]

extern crate addr2line;
extern crate memmap;
extern crate object;
extern crate test;

use std::env;
use std::fs::File;
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

fn with_file<F: FnOnce(&object::File)>(target: &path::Path, f: F) {
    let file = File::open(target).unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = object::File::parse(&*map).unwrap();
    f(&file)
}

/// Obtain a list of addresses contained within the text section of the `target` executable.
// TODO: use object crate instead of nm
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

#[test]
fn context_new() {
    let target = release_fixture_path();

    with_file(&target, |file| {
        addr2line::Context::new(file).unwrap();
    });
}

#[test]
fn context_new_parse_lines() {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let context = addr2line::Context::new(file).unwrap();
        context.parse_lines().unwrap();
    });
}

#[test]
fn context_new_parse_functions() {
    let target = release_fixture_path();

    with_file(&target, |file| {
        let context = addr2line::Context::new(file).unwrap();
        context.parse_functions().unwrap();
    });
}

#[test]
fn context_query_location() {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    with_file(&target, |file| {
        let ctx = addr2line::Context::new(file).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            test::black_box(ctx.find_location(*addr)).ok();
        }

        for addr in &addresses {
            test::black_box(ctx.find_location(*addr)).ok();
        }
    });
}

#[test]
fn context_query_with_functions() {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    with_file(&target, |file| {
        let ctx = addr2line::Context::new(file).unwrap();
        // Ensure nothing is lazily loaded.
        for addr in &addresses {
            test::black_box(ctx.find_frames(*addr)).ok();
        }

        for addr in &addresses {
            test::black_box(ctx.find_frames(*addr)).ok();
        }
    });
}

#[test]
fn context_new_and_query_location() {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    with_file(&target, |file| {
        let ctx = addr2line::Context::new(file).unwrap();
        for addr in addresses.iter().take(100) {
            test::black_box(ctx.find_location(*addr)).ok();
        }
    });
}

#[test]
fn context_new_and_query_with_functions() {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    with_file(&target, |file| {
        let ctx = addr2line::Context::new(file).unwrap();
        for addr in addresses.iter().take(100) {
            test::black_box(ctx.find_frames(*addr)).ok();
        }
    });
}
