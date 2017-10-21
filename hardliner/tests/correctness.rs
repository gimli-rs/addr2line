extern crate hardliner;
extern crate unwind;
extern crate object;
extern crate memmap;
extern crate fallible_iterator;

use fallible_iterator::FallibleIterator;
use unwind::{Unwinder, DwarfUnwinder};

#[test]
fn correctness() {
    test_frame_1();
}

#[inline(never)]
fn test_frame_1() { test_frame_2() }

#[inline(never)]
fn test_frame_2() { test_frame_3() }

static REFERENCE: &'static [&'static str] = &[
    "_ZN6unwind8{{impl}}14trace<closure>E",
    "_ZN11correctness12test_frame_3E",
    "_ZN11correctness12test_frame_2E",
    "_ZN11correctness12test_frame_1E",
    "_ZN11correctness11correctnessE"
];

#[inline(never)]
fn test_frame_3() {
    DwarfUnwinder::default().trace(|mut frames| {
        let map = memmap::Mmap::open_path("/proc/self/exe", memmap::Protection::Read).unwrap();
        let file = &object::File::parse(unsafe { map.as_slice() }).unwrap();
        let ctx = hardliner::Context::new(file).unwrap().parse_functions().unwrap();
        let mut trace: Vec<String> = Vec::new();
        while let Some(frame) = frames.next().unwrap() {
            let ip = frames.registers()[16].unwrap() - 1 - frame.object_base;
            let mut frames = ctx.query(ip).unwrap();
            while let Some(frame) = frames.next().unwrap() {
                trace.push(frame.function.unwrap().raw_name().unwrap().into_owned());
            }
        }
        
        assert_eq!(&trace[..REFERENCE.len()], REFERENCE);
    });
}
