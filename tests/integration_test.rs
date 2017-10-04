extern crate addr2line;
extern crate itertools;

use std::env;
use std::path::{self, PathBuf};
use std::process;
use std::io::prelude::*;
use itertools::Itertools;

fn self_path() -> (PathBuf, PathBuf) {
    let path = env::current_exe().unwrap();
    let mut debug = path.clone();

    // On macOS, symbols are kept in a separate .dSYM file
    if cfg!(target_os = "macos") {
        let fname = debug.file_name().unwrap().to_string_lossy().into_owned();
        debug.set_file_name(format!("{}.dSYM", fname));
        debug.push("Contents");
        debug.push("Resources");
        debug.push("DWARF");
        debug.push(fname);
    }

    (path, debug)
}

fn release_fixture_path() -> (PathBuf, PathBuf) {
    let mut path = PathBuf::new();
    if let Ok(dir) = env::var("CARGO_MANIFEST_DIR") {
        path.push(dir);
    }
    path.push("fixtures");
    path.push("addr2line-release");
    let debug = path.clone();
    (path, debug)
}

#[test]
#[ignore] // FIXME: hangs sometimes
fn self_identity_map() {
    identity_map(self_path())
}

#[test]
#[ignore] // FIXME: hangs a lot
#[cfg(not(target_os = "macos"))]
fn self_with_functions() {
    with_functions(self_path())
}

#[test]
fn release_fixture_identity_map() {
    identity_map(release_fixture_path())
}

#[test]
#[cfg(not(target_os = "macos"))]
fn release_fixture_with_functions() {
    with_functions(release_fixture_path())
}

fn identity_map((target, debug): (PathBuf, PathBuf)) {
    // Parse the debug symbols using "our" addr2line
    let mut ours = addr2line::Mapping::new(&debug).unwrap();

    // Spin up the "real" addr2line
    let mut theirs = spawn_oracle(target.as_path(), &[]);

    // Find some addresses to test using nm (we'll later filter to just text section symbols)
    let mut theirs_in = theirs.stdin.take().unwrap();
    let mappings = get_test_addresses(target.as_path(), &mut theirs_in);
    drop(theirs_in);

    // Go through addresses one by one, and check that we get the same answer as addr2line does.
    use std::io::BufReader;
    let mut theirs_out = BufReader::new(theirs.stdout.take().unwrap()).lines();
    let mut all = 0;
    let mut excusable = 0;
    let mut got = 0;
    for addr in mappings {
        // Read the oracle ouput
        let oracle = theirs_out.next().unwrap().unwrap();
        let oracle = canonicalize_oracle_output(&*oracle);

        // Does our answer match?
        let loc = ours.locate(addr);
        let loc = loc.expect("debug symbols for test binary should be error-free");
        if let Some((file, lineno, _)) = loc {
            // We dared to guess -- did we give the right answer?
            let mut file = file.to_string_lossy();
            if cfg!(target_os = "macos") {
                use std::borrow::Cow;
                // atos doesn't include the full path, just file name
                let f = path::PathBuf::from(&*file);
                file = Cow::Owned(f.file_name().unwrap().to_string_lossy().into_owned());
            }

            all += 1;
            if oracle.0.is_none() && lineno.is_none() {
                // This can happen for the main() wrapper.
                println!("we found 0x{:08x}: {}:??", addr, file);
                excusable += 1;
            } else {
                assert_eq!((addr, oracle), (addr, (Some(&*file), lineno)));
                got += 1;
            }
        } else if oracle.0.is_some() {
            // addr2line found something, and we didn't :(
            println!("we missed 0x{:08x}: {:?}", addr, oracle);
            all += 1;
            if oracle.1.is_none() {
                excusable += 1;
            }
        } else {
            // addr2line did not find a source file, so it's okay that we didn't either.
        }
    }

    assert_ne!(got, 0);
    println!(
        "resolved {}/{} addresses ({} file-only misses)",
        got,
        all,
        excusable
    );
}

#[cfg(not(target_os = "macos"))]
fn with_functions((target, debug): (PathBuf, PathBuf)) {
    // Ignored on macOS since atos doesn't support -f

    // Parse the debug symbols using "our" addr2line
    let mut ours = addr2line::Options::default()
        .with_functions()
        .build(&debug)
        .unwrap();

    // Spin up the "real" addr2line
    let mut theirs = spawn_oracle(target.as_path(), &["-f"]);

    // Find some addresses to test using nm (we'll later filter to just text section symbols)
    let mut theirs_in = theirs.stdin.take().unwrap();
    let mappings = get_test_addresses(target.as_path(), &mut theirs_in);
    drop(theirs_in);

    // Go through addresses one by one, and check that we get the same answer as addr2line does.
    use std::io::BufReader;
    let mut theirs_out = BufReader::new(theirs.stdout.take().unwrap()).lines();
    let mut all = 0;
    let mut excusable = 0;
    let mut func_hits = 0;
    let mut got = 0;
    for addr in mappings {
        // Read the oracle ouput
        let function = theirs_out.next().unwrap().unwrap();
        let oracle = theirs_out.next().unwrap().unwrap();
        let oracle = canonicalize_oracle_output(&*oracle);

        // Does our answer match?
        let loc = ours.locate(addr);
        let loc = loc.expect("debug symbols for test binary should be error-free");
        if let Some((file, lineno, func)) = loc {
            // We dared to guess -- did we give the right answer?
            let f = &*file.to_string_lossy();
            let test = (Some(f), lineno);

            all += 1;
            if oracle.0.is_none() && lineno.is_none() {
                // This can happen for the main() wrapper.
                println!("we found 0x{:08x}: {}", addr, f);
                excusable += 1;
            } else {
                assert_eq!(oracle, test);

                if let Some(func) = func {
                    // We even tried to guess the function!
                    assert_eq!(function, func);
                    func_hits += 1;
                }
                got += 1;
            }
        } else if oracle.0.is_some() {
            // addr2line found something, and we didn't :(
            println!("we missed 0x{:08x}: {:?}", addr, oracle);
            all += 1;
            if oracle.1.is_none() {
                excusable += 1;
            }
        } else {
            // addr2line did not find a source file, so it's okay that we didn't either.
        }
    }

    assert_ne!(got, 0);
    assert_ne!(func_hits, 0);
    println!(
        "resolved {}/{} addresses ({} file-only misses, {} function hits)",
        got,
        all,
        excusable,
        func_hits
    );
}

/// Spawns the oracle version of addr2line on this platform for the given executable.
fn spawn_oracle(target: &path::Path, args: &[&str]) -> process::Child {
    let (theirs, exe) = if cfg!(target_os = "macos") {
        // We have to use atos on macOS
        ("atos", "-o")
    } else {
        // And addr2line on Linux
        ("addr2line", "-e")
    };

    process::Command::new(theirs)
        .arg(exe)
        .arg(target)
        .args(args)
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn()
        .expect("failed to spawn their addr2line")
}

/// Obtain a list of addresses contained within the text section of the `target` executable.
/// Addresses are written to the given `oracle` in the same order as they are yielded.
fn get_test_addresses(target: &path::Path, oracle: &mut Write) -> Vec<u64> {
    // Find some addresses to test using nm (we'll later filter to just text section symbols)
    let names = process::Command::new("/usr/bin/nm")
        .arg(target)
        .output()
        .expect("failed to execute nm");

    // Ideally, we'd just loop over mappings and do them one by one.
    // Unfortunately, atos on macOS doesn't flush its output for every
    // input line, and so this doesn't work. Instead, we pass all the
    // mappings to `theirs`, close stdin, and then start walking.
    let mappings = String::from_utf8_lossy(&names.stdout);
    mappings
        .lines()
        .filter_map(|map| {
            let mut fields = map.split_whitespace();
            let addr = fields.next().expect("nm address missing");
            let t = fields.next().unwrap();
            if t != "T" && t != "t" {
                // We only want functions, which are in the text section
                None
            } else {
                Some(addr)
            }
        })
        .step(5)
        .map(|addr| {
            oracle.write_all(b"0x").unwrap();
            oracle.write_all(addr.as_bytes()).unwrap();
            oracle.write_all(b"\n").unwrap();
            u64::from_str_radix(addr, 16).unwrap()
        })
        .collect()
}

/// The oracles used on different platforms produce output in different formats.
/// This function parses oracle output and produces the represented file and line number.
fn canonicalize_oracle_output(line: &str) -> (Option<&str>, Option<u64>) {
    // Get a line with format file:lineno
    let spec = if cfg!(target_os = "macos") {
        // atos on macOS prints lines in a funky way
        // e.g. "-[SKTGraphicView drawRect:] (in Sketch) (SKTGraphicView.m:445)"
        // We want only path and line number
        let line = line.rsplitn(2, '(').next().unwrap().trim_right_matches(')');
        if !line.contains(':') {
            "??:?"
        } else {
            line
        }
    } else {
        line
    };

    // Parse out line number.
    // Note that we need to use rsplitn in case the file path contains :
    let mut spec = spec.rsplitn(2, ':');

    // Is the line number present?
    let lineno = spec.next().unwrap();
    let lineno = if lineno == "?" {
        None
    } else {
        Some(u64::from_str_radix(lineno, 10).unwrap())
    };

    // Is the file path present?
    let file = spec.next().unwrap();
    let file = if file == "??" || file.is_empty() {
        None
    } else {
        Some(file)
    };

    let mut oracle = (file, lineno);

    // Workaround binutils addr2line bug.
    if oracle
        == (
            Some("/checkout/src/liballoc_jemalloc/../jemalloc/src/prof.c"),
            Some(2093),
        ) {
        oracle = (
            Some("/checkout/src/liballoc_jemalloc/../jemalloc/src/prof.c"),
            Some(1906),
        );
    }

    oracle
}
