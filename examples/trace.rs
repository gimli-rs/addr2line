extern crate unwind;
extern crate fallible_iterator;
extern crate backtrace;

use fallible_iterator::FallibleIterator;
use unwind::{Unwinder, DwarfUnwinder};

fn bar() {
    DwarfUnwinder::default().trace(|mut x| {
        while let Some(frame) = x.next().unwrap() {
            backtrace::resolve(x.registers()[16].unwrap() as *mut std::os::raw::c_void, |sym| println!("{:?} ({:?}:{:?})", sym.name(), sym.filename(), sym.lineno()));
            println!("{:?}", frame);
        }
    });
}

fn foo() {
    bar()
}

fn main() {
    foo();
    println!("down");
}
