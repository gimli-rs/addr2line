use std::borrow::Cow;
use std::fs::File;
use std::io::{BufRead, Lines, StdinLock, Write};
use std::path::{Path, PathBuf};

use clap::{Arg, ArgAction, Command};
use clap_num::maybe_hex;
use fallible_iterator::FallibleIterator;
use gimli::DW_LANG_C_plus_plus;
use object::{Object, ObjectSection, ObjectSegment, SymbolMap, SymbolMapName};
use typed_arena::Arena;

use addr2line::{Context, Location};

fn parse_uint_from_hex_string(string: &str) -> Option<u64> {
    if string.len() > 2 && string.starts_with("0x") {
        u64::from_str_radix(&string[2..], 16).ok()
    } else {
        u64::from_str_radix(string, 16).ok()
    }
}

enum Addrs<'a> {
    Args(clap::parser::ValuesRef<'a, String>),
    Stdin(Lines<StdinLock<'a>>),
}

impl<'a> Iterator for Addrs<'a> {
    type Item = Option<u64>;

    fn next(&mut self) -> Option<Option<u64>> {
        let text = match *self {
            Addrs::Args(ref mut vals) => vals.next().map(Cow::from),
            Addrs::Stdin(ref mut lines) => lines.next().map(Result::unwrap).map(Cow::from),
        };
        text.as_ref()
            .map(Cow::as_ref)
            .map(parse_uint_from_hex_string)
    }
}

fn print_loc(loc: Option<&Location<'_>>, basenames: bool, llvm: bool) {
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
    // println!("print_function: language = {:?}", language);
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
    symbols: &'a SymbolMap<SymbolMapName<'_>>,
    probe: u64,
) -> Option<&'a str> {
    symbols.get(probe).map(|x| x.name())
}

struct Options<'a> {
    do_functions: bool,
    do_inlines: bool,
    only_print_last_inlined_frame: bool,
    pretty: bool,
    print_addrs: bool,
    basenames: bool,
    demangle: bool,
    llvm: bool,
    exe: &'a PathBuf,
    sup: Option<&'a PathBuf>,
    verbose: bool,
    load_addr: Option<&'a u64>,
}

fn main() {
    let matches = Command::new("addr2line")
        .version(env!("CARGO_PKG_VERSION"))
        .about("A fast addr2line Rust port")
        .max_term_width(100)
        .args(&[
            Arg::new("exe")
                .short('e')
                .long("exe")
                .value_name("filename")
                .value_parser(clap::value_parser!(PathBuf))
                .help(
                    "Specify the name of the executable for which addresses should be translated.",
                )
                .required(true),
            Arg::new("sup")
                .long("sup")
                .value_name("filename")
                .value_parser(clap::value_parser!(PathBuf))
                .help("Path to supplementary object file."),
            Arg::new("functions")
                .short('f')
                .long("functions")
                .action(ArgAction::SetTrue)
                .help("Display function names as well as file and line number information."),
            Arg::new("pretty").short('p').long("pretty-print")
                .action(ArgAction::SetTrue)
                .help(
                "Make the output more human friendly: each location are printed on one line.",
            ),
            Arg::new("inlines").short('i').long("inlines")
                .action(ArgAction::SetTrue)
                .help(
                "If the address belongs to a function that was inlined, the source information for \
                all enclosing scopes back to the first non-inlined function will also be printed.",
            ),
            Arg::new("addresses").short('a').long("addresses")
                .action(ArgAction::SetTrue)
                .help(
                "Display the address before the function name, file and line number information.",
            ),
            Arg::new("basenames")
                .short('s')
                .long("basenames")
                .action(ArgAction::SetTrue)
                .help("Display only the base of each file name."),
            Arg::new("demangle").short('C').long("demangle")
                .action(ArgAction::SetTrue)
                .help(
                "Demangle function names. \
                Specifying a specific demangling style (like GNU addr2line) is not supported. \
                (TODO)"
            ),
            Arg::new("llvm")
                .long("llvm")
                .action(ArgAction::SetTrue)
                .help("Display output in the same format as llvm-symbolizer."),
            Arg::new("addrs")
                .action(ArgAction::Append)
                .help("Addresses to use instead of reading from stdin."),
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::SetTrue)
                .help("turn on verbose printing (debugging)"),
            Arg::new("load_addr")
                .long("load_addr")
                .short('l')
                .required(false)
                .default_missing_value(None)
                .value_parser(maybe_hex::<u64>)
                .action(ArgAction::Set)
                .help("specify the load address of the binary image and use all addrs as addresses from within that binary image."),
            Arg::new("only_print_last_inlined_frame")
                .long("only_print_last_inlined_frame")
                .short('F')
                .required(false)
                .action(ArgAction::SetFalse)
                .help("when handling inlined stack frames, only print the last frame and not the intervening frames."),
        ])
        .get_matches();

    let arena_data = Arena::new();

    let opts = Options {
        do_functions: matches.get_flag("functions"),
        do_inlines: matches.get_flag("inlines"),
        only_print_last_inlined_frame: !(matches.get_flag("only_print_last_inlined_frame")),
        pretty: matches.get_flag("pretty"),
        print_addrs: matches.get_flag("addresses"),
        basenames: matches.get_flag("basenames"),
        demangle: matches.get_flag("demangle"),
        llvm: matches.get_flag("llvm"),
        exe: matches.get_one::<PathBuf>("exe").unwrap(),
        sup: matches.get_one::<PathBuf>("sup"),
        verbose: matches.get_flag("verbose"),
        load_addr: matches.get_one::<u64>("load_addr"),
    };
    let file = File::open(opts.exe).unwrap();
    let map = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let object = &object::File::parse(&*map).unwrap();

    let endian = if object.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    let mut load_addr: u64 = 0;
    let mut text_vmaddr: u64 = 0;
    if let Some(addr) = opts.load_addr {
        load_addr = addr.clone();
        let mut segments = object.segments();
        while let Some(segment) = segments.next() {
            if let Ok(Some(name)) = segment.name() {
                if opts.verbose {
                    println!("segment name = {}", name)
                }
                if name == "__TEXT" {
                    // there can only be one __TEXT segment for file.
                    text_vmaddr = segment.address();
                    // println!("in loop: text_vmraddr = {}", text_vmaddr);
                    break;
                }
            }
        }
        if opts.verbose {
            println!("load_ddr  =  0x{:016x}", load_addr);
            println!("text_vmaddr = {}", text_vmaddr);
        }
    }

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

    let mut split_dwarf_loader = addr2line::builtin_split_dwarf_loader::SplitDwarfLoader::new(
        |data, endian| {
            gimli::EndianSlice::new(arena_data.alloc(Cow::Owned(data.into_owned())), endian)
        },
        Some(opts.exe.clone()),
    );
    let ctx = Context::from_dwarf(dwarf).unwrap();

    let stdin = std::io::stdin();
    let addrs = matches
        .get_many::<String>("addrs")
        .map(Addrs::Args)
        .unwrap_or_else(|| Addrs::Stdin(stdin.lock().lines()));

    for mut probe in addrs {
        if opts.print_addrs {
            let addr = probe.unwrap_or(0);
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


        if load_addr != 0 {
            let stack_addr: u64 = probe.unwrap_or(0);
            let adjusted_addr: u64 = text_vmaddr + stack_addr - load_addr;
            probe = Some(adjusted_addr);
            if opts.verbose {
                println!("text_vmaddr = 0x{:016x} stack_addr = 0x{:016x} load_addr = 0x{:016x}",
                         text_vmaddr, stack_addr, load_addr);
                println!("adjusted_addr = 0x{:016x}", adjusted_addr);
                println!("new probe     = 0x{:016x}", probe.unwrap_or(0));
            }
        }

        if opts.do_functions || opts.do_inlines {
            let mut printed_anything = false;
            if let Some(probe) = probe {
                let frames = ctx.find_frames(probe);
                let frames = split_dwarf_loader.run(frames).unwrap();
                let mut frames = frames.enumerate();
                let mut last_frame = None;
                while let Some((i, frame)) = frames.next().unwrap() {
                    if opts.only_print_last_inlined_frame {
                        last_frame = Some(frame);
                        continue;
                    }
                    if opts.pretty && i != 0 {
                        print!(" (inlined by) ");
                    }
                    if opts.do_functions {
                        if let Some(func) = frame.function {
                            let language = // func.language;
                                Some(DW_LANG_C_plus_plus);

                            print_function(
                                func.raw_name().ok().as_ref().map(AsRef::as_ref),
                                // func.language,
                                language,
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
                        // if I'm not following inlined functions, print the first frame.
                        break;
                    }
                }
                if let Some(frame) = last_frame {
                    if opts.do_functions {
                        if let Some(func) = frame.function {
                            let language = // func.language;
                                Some(DW_LANG_C_plus_plus);

                            print_function(
                                func.raw_name().ok().as_ref().map(AsRef::as_ref),
                                // func.language,
                                language,
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
                }
            }


            if !printed_anything {
                // this means the loop above never run, meaning there's no stack to iterate over.
                // How can this be? Perhaps just an address in main? prior to entering main?
                if opts.do_functions {
                    let name = probe.and_then(|probe| find_name_from_symbols(&symbols, probe));
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
            // this is for not printing functions and not printing inlined funcs.
            let loc = probe.and_then(|probe| ctx.find_location(probe).unwrap());
            print_loc(loc.as_ref(), opts.basenames, opts.llvm);
        };

        if opts.llvm {
            println!();
        }
        std::io::stdout().flush().unwrap();
    }
}
