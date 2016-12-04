extern crate addr2line;
extern crate itertools;
extern crate glob;

use std::env;
use std::path;
use std::process;
use std::io::prelude::*;
use itertools::Itertools;

#[test]
fn identity_map() {
    let target = env::current_exe().unwrap();
    let mut debug = target.clone();

    // On macOS, symbols are kept in a separate .dSYM file
    if cfg!(target_os = "macos") {
        let fname = debug.file_name().unwrap().to_string_lossy().into_owned();
        debug.set_file_name(format!("{}.dSYM", fname));
        debug.push("Contents");
        debug.push("Resources");
        debug.push("DWARF");
        debug.push(fname);
    }

    // Parse the debug symbols using "our" addr2line
    let ours = addr2line::Mapping::new(&debug).unwrap();

    // Spin up the "real" addr2line
    let mut theirs = spawn_oracle(target.as_path());

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
        if let Some((mut file, lineno)) = ours.locate(addr) {
            // We dared to guess -- did we give the right answer?
            if cfg!(target_os = "macos") {
                // atos doesn't include the full path, just file name
                let f = path::PathBuf::from(&file);
                let f = f.file_name().unwrap().to_string_lossy().into_owned();
                file = f;
            }

            assert_eq!(oracle.0, Some(&*file));
            assert_eq!(oracle.1, Some(lineno));
            got += 1;
            all += 1;
        } else {
            if oracle.0.is_some() {
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
    }

    assert_ne!(got, 0);
    println!("resolved {}/{} addresses ({} file-only misses)",
             got,
             all,
             excusable);
}

/// Spawns the oracle version of addr2line on this platform for the given executable.
fn spawn_oracle(target: &path::Path) -> process::Child {
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
    mappings.lines()
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
            oracle.write_all("0x".as_bytes()).unwrap();
            oracle.write_all(addr.as_bytes()).unwrap();
            oracle.write_all("\n".as_bytes()).unwrap();
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
        let line = line.rsplitn(2, "(").next().unwrap().trim_right_matches(')');
        if !line.contains(":") { "??:?" } else { line }
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

    (file, lineno)
}
