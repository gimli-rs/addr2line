extern crate addr2line;
extern crate fallible_iterator;
extern crate findshlibs;
extern crate gimli;
extern crate memmap;
extern crate object;

use std::fs::File;

use addr2line::Context;
use findshlibs::{IterationControl, SharedLibrary, TargetSharedLibrary};

#[test]
fn correctness() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let ctx = Context::new(file).unwrap();

    let mut bias = None;
    TargetSharedLibrary::each(|lib| {
        bias = Some(lib.virtual_memory_bias().0 as u64);
        IterationControl::Break
    });

    let ip = (test_function as u64).wrapping_sub(bias.unwrap());

    let mut frames = ctx.find_frames(ip).unwrap();
    let frame = frames.next().unwrap().unwrap();
    let name = frame.function.as_ref().unwrap().demangle().unwrap();
    // Old rust versions generate DWARF with wrong linkage name,
    // so only check the start.
    if !name.starts_with("correctness::test_function") {
        panic!("incorrect name '{}'", name);
    }
}

fn test_function() {}

#[test]
fn zero_sequence() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let ctx = Context::new(file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_location(probe).unwrap().is_none());
    }
}

#[test]
fn zero_function() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let ctx = Context::new(file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_frames(probe).unwrap().next().unwrap().is_none());
    }
}

#[test]
fn iter_lines_function() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let ctx = Context::new(file).unwrap();
    let info = ctx.copy_line_infos();
    //check that we include at least the current file in the mapping
    assert!(info.iter().any(|line| line.file.ends_with(file!())));
}
