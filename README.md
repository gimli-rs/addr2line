# addr2line

[![](http://meritbadge.herokuapp.com/addr2line) ![](https://img.shields.io/crates/d/addr2line.png)](https://crates.io/crates/addr2line) [![](https://docs.rs/addr2line/badge.svg)](https://docs.rs/addr2line/) [![Build Status](https://travis-ci.org/gimli-rs/addr2line.png?branch=master)](https://travis-ci.org/gimli-rs/addr2line) [![Coverage Status](https://coveralls.io/repos/github/gimli-rs/addr2line/badge.svg?branch=master)](https://coveralls.io/github/gimli-rs/addr2line?branch=master)

A cross-platform library for retrieving per-address debug information
executables with DWARF debug symbols.

`addr2line` uses [`gimli`](https://github.com/gimli-rs/gimli) to parse
the debug symbols of an executable, and exposes an interface for finding
the source file, line number, and wrapping function for instruction
addresses within the target program. These lookups can either be
performed programmatically through `Mapping::locate`, or via the
included binary, `addr2line` (named and modelled after the equivalent
utility from [GNU
binutils](https://sourceware.org/binutils/docs/binutils/addr2line.html)).

# Quickstart

 - Add the [`addr2line` crate](https://crates.io/crates/addr2line) to your `Cargo.toml`
 - Add `extern crate addr2line` to your main crate entry file
 - Pass the path of the executable you want to inspect to [`addr2line::Mapping::new`](https://docs.rs/addr2line/*/addr2line/struct.Mapping.html#method.new)
 - Use [`Mapping::locate`](https://docs.rs/addr2line/*/addr2line/struct.Mapping.html#method.locate) to look up debug information for an address

# Usage example

The library is primarily meant to be used to investigate *other*
binaries, but it *can* also be used for introspection. This provides a
neat, self-contained example that approximates the functionality of the
provided `addr2line` binary:

```rust
extern crate addr2line;

fn main() {
    use std::env;
    let us = env::current_exe().expect("not running as an executable");
    let map = addr2line::Mapping::new(&us).expect("debug symbols not found");
    let addr = env::args().skip(1).next().expect("no address passed");
    let addr = u64::from_str_radix(&addr[2..], 16).expect("address not valid");
    let loc = map.locate(addr as u64).expect("invalid debug symbols found");
    if let Some((file, line, func)) = loc {
        print!("0x{:08x} is in {}", addr, file.display());
        if let Some(line) = line {
            print!(" on line {}", line);
        }
        if let Some(func) = func {
            print!(" in function {}", func);
        }
        println!("");
    } else {
        println!("could not locate debug information for 0x{:08x}", addr);
    }
}
```

Let's find an interesting address to look up using `objdump`:

```console
$ cargo build
   Compiling addr2line v0.2.0 (file:///home/jon/dev/minor/addr2line)
   Compiling addr2line-ex v0.1.0 (file:///home/jon/tmp/addr2line-ex)
    Finished debug [unoptimized + debuginfo] target(s) in 2.50 secs
$ objdump -d target/debug/addr2line-ex  | grep -E 'main.*:'
000000000000cd80 <_ZN12addr2line_ex4main17h4c6c4a79f3ebd4beE>:
000000000000d930 <main>:
```

The first result is the `fn main()` we defined, the second is the
[program entry point](https://en.wikipedia.org/wiki/Entry_point), which
does little more than just jump to the Rust `main()`. Let's confirm that
this is indeed the case:

```console
$ cargo run 0xcd80
    Finished debug [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/addr2line-ex 0x000000000000cd80`
0x0000cd80 is in /home/jon/tmp/addr2line-ex/src/main.rs on line 3
```

Exactly where we thought it would be! Unfortunately, we don't also see
the expected "in function main" message. This is because `Mapping::new`
does not extract and expose function-level debug information. To get
that, you need to use `Mapping::with_functions` instead.

```console
$ sed -i 's/Mapping::new/Mapping::with_functions/' src/main.rs
$ cargo run 0xcd80
   Compiling addr2line-ex v0.1.0 (file:///home/jon/tmp/addr2line-ex)
    Finished debug [unoptimized + debuginfo] target(s) in 0.41 secs
     Running `target/debug/addr2line-ex 0xcd80`
0x0000cd80 is in /home/jon/tmp/addr2line-ex/src/main.rs on line 3 in function _ZN12addr2line_ex4mainE
```

Perfect! Let's see if it also works for an address *inside* `main`.
What about the call to `from_str_radix`?

```console
$ objdump -d target/debug/addr2line-ex  | grep -E 'call.*from_str_radix'
    cf79:	e8 d2 c3 0d 00       	callq  e9350 <_ZN4core3num21_$LT$impl$u20$u64$GT$14from_str_radix17h6e264457c3d6c0cfE>
   e9354:	e8 37 da ff ff       	callq  e6d90 <_ZN4core3num14from_str_radix17he4ded7f141af381aE>
   e9369:	e8 02 d9 ff ff       	callq  e6c70 <_ZN4core3num14from_str_radix17h7674eac951972f07E>
```

The first one is the one in our main (look at the addresses in the
left-most column). Let's check:

```console
$ cargo run 0xcf79
    Finished debug [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/addr2line-ex 0xcf79`
0x0000cf79 is in /home/jon/tmp/addr2line-ex/src/main.rs on line 8 in function _ZN12addr2line_ex4mainE
```

How about that? Quite neat!

## Using the provided binary

The library ships with a binary that is written to emulate the
`addr2line` included with GNU binutils. We can use this instead of our
little program above to get the same information:

```console
$ cargo install addr2line
$ ~/.cargo/bin/addr2line -e target/debug/addr2line-ex -f 0xcd80
_ZN12addr2line_ex4mainE
/home/jon/tmp/addr2line-ex/src/main.rs:3
$ addr2line -e target/debug/addr2line-ex -f 0xcd80
_ZN12addr2line_ex4mainE
/home/jon/tmp/addr2line-ex/src/main.rs:3
```
