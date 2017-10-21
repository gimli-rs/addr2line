extern crate backtrace;
extern crate findshlibs;
extern crate test;

use std::env;
use std::process::Command;
use std::path::Path;
use std::ffi::OsStr;

use backtrace::Backtrace;
use findshlibs::{IterationControl, TargetSharedLibrary, SharedLibrary};
use test::{TestDescAndFn, TestDesc, TestFn, TestName, ShouldPanic};

fn make_trace() -> Vec<String> {
    fn foo() -> Backtrace {
        bar()
    }
    #[inline(never)]
    fn bar() -> Backtrace {
        baz()
    }
    #[inline(always)]
    fn baz() -> Backtrace {
        Backtrace::new_unresolved()
    }

    let mut base_addr = None;
    TargetSharedLibrary::each(|lib| {
        base_addr = Some(lib.virtual_memory_bias().0);
        IterationControl::Break
    });
    let addrfix = - base_addr.unwrap();

    let trace = foo();
    trace.frames().iter().map(|x| format!("{:p}", (x.ip() as *const u8).wrapping_offset(addrfix))).collect()
}

fn run_cmd<P: AsRef<OsStr>>(exe: P, me: &Path, flags: Option<&str>, trace: &[String]) -> Vec<u8> {
    let mut cmd = Command::new(exe);
    cmd.env("LC_ALL", "C"); // GNU addr2line is localized, we aren't
    cmd.env("RUST_BACKTRACE", "1"); // if a child crashes, we want to know why

    if let Some(flags) = flags {
        cmd.arg(flags);
    }
    cmd.arg("--exe").arg(me).args(trace);

    let output = cmd.output().unwrap();

    assert!(output.status.success());
    output.stdout
}

fn run_test(flags: Option<&str>) {
    let me = env::current_exe().unwrap();
    let mut exe = me.clone();
    assert!(exe.pop());
    assert!(exe.pop());
    exe.push("examples");
    exe.push("addr2line");

    assert!(exe.is_file());

    let trace = make_trace();

    let theirs = run_cmd("addr2line", &me, flags, &trace);
    let ours = run_cmd(&exe, &me, flags, &trace);
    assert_eq!(theirs, ours);
}

static FLAGS: &'static str = "aipsfC";

fn make_tests() -> Vec<TestDescAndFn> {
    (0 .. (1 << FLAGS.len())).map(|bits| {
        if bits == 0 {
            None
        } else {
            let mut param = String::new();
            param.push('-');
            for (i, flag) in FLAGS.chars().enumerate() {
                if (bits & (1 << i)) != 0 {
                    param.push(flag);
                }
            }
            Some(param)
        }
    }).map(|param| TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(format!("addr2line {}", param.as_ref().map_or("", String::as_str))),
            ignore: false,
            should_panic: ShouldPanic::No,
            allow_fail: false,
        },
        testfn: TestFn::DynTestFn(Box::new(move || run_test(param.as_ref().map(String::as_str)))),
    }).collect()
}

fn main() {
    let args: Vec<_> = env::args().collect();
    test::test_main(&args, make_tests());
}
