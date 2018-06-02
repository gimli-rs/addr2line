extern crate addr2line;
extern crate fallible_iterator;
extern crate findshlibs;
extern crate gimli;
extern crate memmap;
extern crate object;
extern crate typed_arena;

use std::fs::File;

use findshlibs::{IterationControl, SharedLibrary, TargetSharedLibrary};
use addr2line::Context;
use typed_arena::Arena;

#[test]
fn correctness() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let arena = Arena::new();
    let ctx = Context::new(&arena, file).unwrap();

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
    assert!(name.starts_with("correctness::test_function"));
}

fn test_function() {}

#[test]
fn zero_sequence() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let arena = Arena::new();
    let ctx = Context::new(&arena, file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_location(probe).unwrap().is_none());
    }
}

#[test]
fn zero_function() {
    let file = File::open("/proc/self/exe").unwrap();
    let map = unsafe { memmap::Mmap::map(&file).unwrap() };
    let file = &object::File::parse(&*map).unwrap();
    let arena = Arena::new();
    let ctx = Context::new(&arena, file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_frames(probe).unwrap().next().unwrap().is_none());
    }
}
