extern crate backtrace;
extern crate findshlibs;

use std::env;
use std::process::Command;
use std::path::Path;
use std::ffi::OsStr;

use backtrace::Backtrace;
use findshlibs::{IterationControl, TargetSharedLibrary, SharedLibrary};

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
    println!("running {:?}", cmd);
    let output = cmd.output().unwrap();
    println!("{:?}", output);
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

#[test]
fn test_flags() {
    // test no flags
    run_test(None);

    let flags = "aipsfC";
    // test every subset of those flags by mapping flags to bits
    // and iterating through the n-bit number
    for bits in 1 .. (1 << flags.len()) {
        let mut param = String::new();
        param.push('-');
        for (i, flag) in flags.chars().enumerate() {
            if (bits & (1 << i)) != 0 {
                param.push(flag);
            }
        }

        run_test(Some(&param));
    }
}
