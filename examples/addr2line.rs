extern crate addr2line;
extern crate bpaf;
extern crate fallible_iterator;
extern crate gimli;
extern crate memmap2;
extern crate object;
extern crate typed_arena;

use std::borrow::Cow;
use std::fs::File;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};

use fallible_iterator::FallibleIterator;
use object::{Object, ObjectSection, SymbolMap, SymbolMapName};
use typed_arena::Arena;

use addr2line::{Context, Location};

fn parse_uint_from_hex_string(string: &str) -> Result<u64, std::num::ParseIntError> {
    if string.len() > 2 && string.starts_with("0x") {
        u64::from_str_radix(&string[2..], 16)
    } else {
        u64::from_str_radix(string, 16)
    }
}

fn print_loc(loc: Option<&Location>, basenames: bool, llvm: bool) {
    if let Some(loc) = loc {
        if let Some(ref file) = loc.file.as_ref() {
            let path = if basenames {
                Path::new(Path::new(file).file_name().unwrap())
            } else {
                Path::new(file)
            };
            print!("{}:", path.display());
        } else {
            print!("??:");
        }
        if llvm {
            print!("{}:{}", loc.line.unwrap_or(0), loc.column.unwrap_or(0));
        } else if let Some(line) = loc.line {
            print!("{}", line);
        } else {
            print!("?");
        }
        println!();
    } else if llvm {
        println!("??:0:0");
    } else {
        println!("??:0");
    }
}

fn print_function(name: Option<&str>, language: Option<gimli::DwLang>, demangle: bool) {
    if let Some(name) = name {
        if demangle {
            print!("{}", addr2line::demangle_auto(Cow::from(name), language));
        } else {
            print!("{}", name);
        }
    } else {
        print!("??");
    }
}

fn load_file_section<'input, 'arena, Endian: gimli::Endianity>(
    id: gimli::SectionId,
    file: &object::File<'input>,
    endian: Endian,
    arena_data: &'arena Arena<Cow<'input, [u8]>>,
) -> Result<gimli::EndianSlice<'arena, Endian>, ()> {
    // TODO: Unify with dwarfdump.rs in gimli.
    let name = id.name();
    match file.section_by_name(name) {
        Some(section) => match section.uncompressed_data().unwrap() {
            Cow::Borrowed(b) => Ok(gimli::EndianSlice::new(b, endian)),
            Cow::Owned(b) => Ok(gimli::EndianSlice::new(arena_data.alloc(b.into()), endian)),
        },
        None => Ok(gimli::EndianSlice::new(&[][..], endian)),
    }
}

fn find_name_from_symbols<'a>(
    symbols: &'a SymbolMap<SymbolMapName>,
    probe: u64,
) -> Option<&'a str> {
    symbols.get(probe).map(|x| x.name())
}

struct Options {
    exe: PathBuf,
    sup: Option<PathBuf>,
    do_functions: bool,
    do_inlines: bool,
    pretty: bool,
    print_addrs: bool,
    basenames: bool,
    demangle: bool,
    llvm: bool,
    addrs: Vec<Option<u64>>,
}

fn options() -> Options {
    use bpaf::*;

    let print_addrs = short('a')
        .long("addresses")
        .help("Display the address before the function name, file and line number information.")
        .switch();

    let exe = short('e')
        .long("exe")
        .help("Specify the name of the executable for which addresses should be translated.")
        .argument_os("filename")
        .map(PathBuf::from);

    let sup = long("sup")
        .help("Path to supplementary object file.")
        .argument_os("filename")
        .map(PathBuf::from)
        .optional();

    let do_functions = short('f')
        .long("functions")
        .help("Display function names as well as file and line number information.")
        .switch();

    let pretty = short('p')
        .long("pretty-print")
        .help("Make the output more human friendly: each location are printed on one line.")
        .switch();

    let do_inlines = short('i')
        .long("inlines")
        .help(
            "If the address belongs to a function that was inlined, the source information for
all enclosing scopes back to the first non-inlined function will also be printed.",
        )
        .switch();

    let basenames = short('s')
        .long("basenames")
        .help("Display only the base of each file name.")
        .switch();

    let demangle = short('C')
        .long("demangle")
        .help(
            "Demangle function names.
Specifying a specific demangling style (like GNU addr2line) is not supported. (TODO)",
        )
        .switch();

    let llvm = short('L')
        .long("llvm")
        .help("Display output in the same format as llvm-symbolizer.")
        .switch();

    let addrs = positional("addrs")
        .help("Addresses to use instead of reading from stdin.")
        .map(|s| parse_uint_from_hex_string(&s).ok())
        .many();

    construct!(Options {
        print_addrs,
        do_functions,
        exe,
        sup,
        pretty,
        demangle,
        basenames,
        do_inlines,
        llvm,
        addrs,
    })
    .to_options()
    .descr("A fast addr2line Rust port")
    .version(env!("CARGO_PKG_VERSION"))
    .run()
}

fn main() {
    let opts = options();

    let arena_data = Arena::new();

    let file = File::open(opts.exe).unwrap();
    let map = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let object = &object::File::parse(&*map).unwrap();

    let endian = if object.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    let mut load_section = |id: gimli::SectionId| -> Result<_, _> {
        load_file_section(id, object, endian, &arena_data)
    };

    let sup_map;
    let sup_object = if let Some(sup_path) = opts.sup {
        let sup_file = File::open(sup_path).unwrap();
        sup_map = unsafe { memmap2::Mmap::map(&sup_file).unwrap() };
        Some(object::File::parse(&*sup_map).unwrap())
    } else {
        None
    };

    let symbols = object.symbol_map();
    let mut dwarf = gimli::Dwarf::load(&mut load_section).unwrap();
    if let Some(ref sup_object) = sup_object {
        let mut load_sup_section = |id: gimli::SectionId| -> Result<_, _> {
            load_file_section(id, sup_object, endian, &arena_data)
        };
        dwarf.load_sup(&mut load_sup_section).unwrap();
    }

    let ctx = Context::from_dwarf(dwarf).unwrap();

    let addrs: Box<dyn Iterator<Item = Option<u64>>> = if opts.addrs.is_empty() {
        Box::new(
            std::io::stdin()
                .lock()
                .lines()
                .map(|s| parse_uint_from_hex_string(&s.unwrap()).ok()),
        )
    } else {
        Box::new(opts.addrs.iter().copied())
    };

    for mprobe in addrs {
        let probe = match mprobe {
            Some(probe) => probe,
            None => {
                println!("??:0");
                continue;
            }
        };
        if opts.print_addrs {
            let addr = probe;
            if opts.llvm {
                print!("0x{:x}", addr);
            } else {
                print!("0x{:016x}", addr);
            }
            if opts.pretty {
                print!(": ");
            } else {
                println!();
            }
        }

        if opts.do_functions || opts.do_inlines {
            let mut printed_anything = false;

            let mut frames = ctx.find_frames(probe).unwrap().enumerate();
            while let Some((i, frame)) = frames.next().unwrap() {
                if opts.pretty && i != 0 {
                    print!(" (inlined by) ");
                }

                if opts.do_functions {
                    if let Some(func) = frame.function {
                        print_function(
                            func.raw_name().ok().as_ref().map(AsRef::as_ref),
                            func.language,
                            opts.demangle,
                        );
                    } else {
                        let name = find_name_from_symbols(&symbols, probe);
                        print_function(name, None, opts.demangle);
                    }

                    if opts.pretty {
                        print!(" at ");
                    } else {
                        println!();
                    }
                }

                print_loc(frame.location.as_ref(), opts.basenames, opts.llvm);

                printed_anything = true;

                if !opts.do_inlines {
                    break;
                }
            }

            if !printed_anything {
                if opts.do_functions {
                    let name = find_name_from_symbols(&symbols, probe);
                    print_function(name, None, opts.demangle);

                    if opts.pretty {
                        print!(" at ");
                    } else {
                        println!();
                    }
                }

                print_loc(None, opts.basenames, opts.llvm);
            }
        } else {
            let loc = ctx.find_location(probe).unwrap();
            print_loc(loc.as_ref(), opts.basenames, opts.llvm);
        }

        if opts.llvm {
            println!();
        }
        std::io::stdout().flush().unwrap();
    }
}
