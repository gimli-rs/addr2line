#[macro_use]
extern crate clap;
extern crate addr2line;

use clap::{App, Arg};

use std::path;

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

fn main() {
    let matches = App::new("addr2line")
        .version(crate_version!())
        .about("Convert addresses into line number/file name pairs.\n\
                If no addresses are specified on the command line, they will be read from stdin.")
        .arg(Arg::with_name("executable")
            .short("e")
            .long("exe")
            .default_value("a.out")
            .help("Specify the name of the executable for which addresses should be translated.")
            .takes_value(true))
        .arg(Arg::with_name("addresses")
            .short("a")
            .long("addresses")
            .help("Display the address before the function name, file and line number \
                   information. The address is printed with a `0x' prefix to easily identify \
                   it."))
        .arg(Arg::with_name("functions")
            .short("f")
            .long("functions")
            .help("Display function names as well as file and line number information."))
        .arg(Arg::with_name("addr")
            .multiple(true)
            .index(1))
        .after_help(POSTSCRIPT)
        .get_matches();

    let exe = matches.value_of("executable").unwrap_or("./a.out");
    let show_funcs = matches.is_present("functions");

    let debug = if show_funcs {
        addr2line::Mapping::with_functions(path::Path::new(&exe))
    } else {
        addr2line::Mapping::new(path::Path::new(&exe))
    };

    if let Err(e) = debug {
        println!("addr2line: {:?}", e);
        std::process::exit(1);
    };
    let debug = debug.unwrap();

    let show_addrs = matches.is_present("addresses");
    let one = |addr: &str| {
        let addr = parse_uint_from_hex_string(addr);
        if show_addrs {
            use std::mem;
            match mem::size_of::<usize>() {
                8 => println!("0x{:016x}", addr),
                _ => println!("0x{:08x}", addr),
            }
        }
        if let Some((file, lineno, func)) = debug.locate(addr) {
            if show_funcs {
                use std::borrow::Cow;
                println!("{}", func.unwrap_or(Cow::Borrowed("??")));
            }
            println!("{}:{}",
                     file.to_string_lossy(),
                     lineno.map(|n| format!("{}", n)).unwrap_or("?".to_owned()));
        } else {
            if show_funcs {
                println!("??")
            }
            println!("??:?")
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
