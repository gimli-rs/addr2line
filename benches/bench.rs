#![feature(test)]
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
    for line in symbols.lines() {
        let fields: Vec<_> = line.split_whitespace().take(4).collect();
        if fields.len() >= 4 && (fields[3] == "T" || fields[3] == "t") {
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

#[bench]
#[cfg(not(target_os = "macos"))]
fn without_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    b.iter(|| {
        let mut addr2line = addr2line::Options::default().build(&target).unwrap();
        for addr in &addresses {
            test::black_box(addr2line.locate(*addr).ok());
        }
    });
}

#[bench]
#[cfg(not(target_os = "macos"))]
fn with_functions(b: &mut test::Bencher) {
    let target = release_fixture_path();
    let addresses = get_test_addresses(target.as_path());

    b.iter(|| {
        let mut addr2line = addr2line::Options::default()
            .with_functions()
            .build(&target)
            .unwrap();
        for addr in &addresses {
            test::black_box(addr2line.locate(*addr).ok());
        }
    });
}
