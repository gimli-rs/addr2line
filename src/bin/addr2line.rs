extern crate addr2line;
#[macro_use]
extern crate clap;

use clap::{App, Arg};

#[cfg_attr(rustfmt, rustfmt_skip)]
const POSTSCRIPT: &'static str = "\
addr2line translates addresses into file names and line numbers.
Given an address in an executable or an offset in a section of a
relocatable object, it uses the debugging information to figure
out which file name and line number are associated with it.

By default, the format of the output is `FILENAME:LINENO', and
each input address generates one line of output. This behavior
can be modified by passing the -a option, which also includes
the address on its own line.

If the file name can not be determined, addr2line will print two
question marks in their place. If the line number can not be
determined, addr2line will print 0.
";

fn get_matches() -> clap::ArgMatches<'static> {
    App::new("addr2line")
        .version(crate_version!())
        .about(
            "Convert addresses into line number/file name pairs.\n\
             If no addresses are specified on the command line, they will be read from stdin.",
        )
        .arg(
            Arg::with_name("executable")
                .short("e")
                .long("exe")
                .default_value("a.out")
                .help(
                    "Specify the name of the executable for which addresses should be translated.",
                )
                .takes_value(true),
        )
        .arg(
            Arg::with_name("addresses")
                .short("a")
                .long("addresses")
                .help(
                    "Display the address before the function name, file and line number \
                     information. The address is printed with a `0x' prefix to easily identify \
                     it.",
                ),
        )
        .arg(
            Arg::with_name("demangle").short("C").long("demangle").help(
                "Decode (demangle) low-level symbol names into user-level names.  Besides \
                 removing any initial underscore prepended by the system, this makes C++ \
                 function names readable.",
            ),
        )
        .arg(
            Arg::with_name("functions")
                .short("f")
                .long("functions")
                .help("Display function names as well as file and line number information."),
        )
        .arg(Arg::with_name("addr").multiple(true).index(1))
        .after_help(POSTSCRIPT)
        .get_matches()
}

#[cfg(any(feature = "rustc-demangle", feature = "cpp_demangle"))]
fn get_options(matches: &clap::ArgMatches) -> addr2line::Options {
    let mut opts = addr2line::Options::default().with_symbol_table();

    if matches.is_present("functions") {
        opts = opts.with_functions();
    }
    if matches.is_present("demangle") {
        opts = opts.with_demangling();
    }

    opts
}

#[cfg(not(any(feature = "rustc-demangle", feature = "cpp_demangle")))]
fn get_options(matches: &clap::ArgMatches) -> addr2line::Options {
    let mut opts = addr2line::Options::default().with_symbol_table();

    if matches.is_present("functions") {
        opts = opts.with_functions();
    }

    opts
}

fn main() {
    let matches = get_matches();
    let opts = get_options(&matches);

    let exe = matches.value_of("executable").unwrap_or("./a.out");
    let show_funcs = matches.is_present("functions");
    let debug = opts.build(exe);

    if let Err(e) = debug {
        println!("addr2line: {:?}", e);
        std::process::exit(1);
    };
    let mut debug = debug.unwrap();

    let show_addrs = matches.is_present("addresses");
    let mut one = |addr: &str| {
        let addr = parse_uint_from_hex_string(addr);
        if show_addrs {
            use std::mem;
            match mem::size_of::<usize>() {
                8 => println!("0x{:016x}", addr),
                _ => println!("0x{:08x}", addr),
            }
        }

        // TODO: we may want to print an error here. GNU binutils addr2line doesn't though...
        let loc = debug.locate(addr).unwrap_or(None);
        if let Some((file, lineno, func)) = loc {
            use std::borrow::Cow;
            if show_funcs {
                println!("{}", func.unwrap_or(Cow::Borrowed("??")));
            }
            println!(
                "{}:{}",
                file.as_ref()
                    .map(|f| f.to_string_lossy())
                    .unwrap_or(Cow::Borrowed("??")),
                lineno
                    .map(|n| Cow::Owned(format!("{}", n)))
                    .unwrap_or(Cow::Borrowed("?"))
            );
        } else {
            if show_funcs {
                println!("??")
            }
            println!("??:0")
        }
    };

    if let Some(addrs) = matches.values_of("addr") {
        for addr in addrs {
            one(addr);
        }
    } else {
        use std::io;
        use std::io::prelude::*;

        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let addr = line.unwrap();
            one(&*addr);
        }
    }
}

fn parse_uint_from_hex_string(string: &str) -> u64 {
    if string.len() > 2 && string.starts_with("0x") {
        u64::from_str_radix(&string[2..], 16).expect("Failed to parse address")
    } else {
        u64::from_str_radix(string, 16).expect("Failed to parse address")
    }
}
