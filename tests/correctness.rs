extern crate addr2line;
extern crate fallible_iterator;
extern crate memmap;
extern crate object;
extern crate unwind;

use fallible_iterator::FallibleIterator;
use unwind::{DwarfUnwinder, Unwinder};
use addr2line::Context;

// ugly hack :(
#[inline(never)]
fn no_tailcall_please() {
    print!("")
}

#[test]
fn correctness() {
    test_frame_1();
    no_tailcall_please();
}

#[inline(never)]
fn test_frame_1() {
    test_frame_2();
    no_tailcall_please();
}

#[inline(never)]
fn test_frame_2() {
    test_frame_3();
    no_tailcall_please();
}

static REFERENCE: &'static [&'static str] = &[
    "_ZN6unwind8{{impl}}14trace<closure>E",
    "_ZN11correctness12test_frame_3E",
    "_ZN11correctness12test_frame_2E",
    "_ZN11correctness12test_frame_1E",
    "_ZN11correctness11correctnessE",
];

#[inline(never)]
fn test_frame_3() {
    DwarfUnwinder::default().trace(|frames| {
        let map = memmap::Mmap::open_path("/proc/self/exe", memmap::Protection::Read).unwrap();
        let file = &object::File::parse(unsafe { map.as_slice() }).unwrap();
        let mut ctx = Context::new(file).unwrap();
        let mut trace: Vec<String> = Vec::new();
        while let Some(frame) = frames.next().unwrap() {
            let ip = frames.registers()[16].unwrap() - 1 - frame.object_base;
            let mut frames = ctx.find_frames(ip).unwrap();
            while let Some(frame) = frames.next().unwrap() {
                trace.push(frame.function.unwrap().raw_name().unwrap().into_owned());
            }
        }

        assert_eq!(&trace[..REFERENCE.len()], REFERENCE);
    });
    no_tailcall_please();
}

#[test]
fn zero_sequence() {
    let map = memmap::Mmap::open_path("/proc/self/exe", memmap::Protection::Read).unwrap();
    let file = &object::File::parse(unsafe { map.as_slice() }).unwrap();
    let mut ctx = Context::new(file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_location(probe).unwrap().is_none());
    }
}

#[test]
fn zero_function() {
    let map = memmap::Mmap::open_path("/proc/self/exe", memmap::Protection::Read).unwrap();
    let file = &object::File::parse(unsafe { map.as_slice() }).unwrap();
    let mut ctx = Context::new(file).unwrap();
    for probe in 0..10 {
        assert!(ctx.find_frames(probe).unwrap().next().unwrap().is_none());
    }
}
