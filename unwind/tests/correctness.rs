extern crate unwind;
extern crate backtrace;
extern crate fallible_iterator;

use unwind::{Unwinder, DwarfUnwinder};
use fallible_iterator::FallibleIterator;

#[test]
fn correctness() {
    test_frame_1();
}

#[inline(never)]
fn test_frame_1() { test_frame_2() }

#[inline(never)]
fn test_frame_2() { test_frame_3() }

#[inline(never)]
fn test_frame_3() {
    let bt = backtrace::Backtrace::new_unresolved();
    let ref_trace: Vec<u64> = bt.frames().iter().map(|x| x.ip() as u64).collect();

    let ref_trace = &ref_trace[5..]; // skip 5 (backtrace-rs internals)

    for i in ref_trace {
        println!("{:08x}", i);
    }
    println!();
        
    DwarfUnwinder::default().trace(move |mut frames| {
        // skip 2 (unwind-rs + test_frame_3)
        frames.next().unwrap();
        frames.next().unwrap();

        let mut our_trace = Vec::new();
        while let Some(_) = frames.next().unwrap() {
            our_trace.push(frames.registers()[16].unwrap() - 1);
        }

        for i in &our_trace {
            println!("{:08x}", i);
        }
        
        assert_eq!(our_trace, ref_trace);
    });
}
