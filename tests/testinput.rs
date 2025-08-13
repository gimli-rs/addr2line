use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

use libtest_mimic::{Arguments, Failed, Trial};

fn main() {
    if cfg!(target_os = "windows") {
        // Tests don't currently handle newline differences on Windows.
        return;
    }

    let args = Arguments::from_args();
    libtest_mimic::run(&args, make_tests()).exit();
}

fn make_tests() -> Vec<Trial> {
    let testinput = PathBuf::from("testinput/dwarf");
    let testoutput = PathBuf::from("testoutput/dwarf");

    let mut tests = Vec::new();
    let mut dirs = vec![(testinput, testoutput)];
    while let Some((in_dir, out_dir)) = dirs.pop() {
        for entry in out_dir.read_dir().unwrap() {
            let entry = entry.unwrap();
            let out_path = entry.path();
            let mut in_path = in_dir.clone();
            in_path.push(entry.file_name());

            let file_type = entry.file_type().unwrap();
            if file_type.is_dir() {
                dirs.push((in_path, out_path));
            } else if file_type.is_file() {
                tests.push(Trial::test(
                    format!("addr2line -e {}", in_path.display()),
                    move || run_test(in_path, out_path, &["-afi", "--all"]),
                ));
            }
        }
    }

    let in_path = PathBuf::from("testinput/dwarf/base-gcc-g2");
    const FLAGS: &str = "aipsf";
    for bits in 1..(1 << FLAGS.len()) {
        let mut flags = String::new();
        flags.push('-');
        for (i, flag) in FLAGS.chars().enumerate() {
            if (bits & (1 << i)) != 0 {
                flags.push(flag);
            }
        }
        let in_path = in_path.clone();
        let out_path = PathBuf::from(format!("testoutput/flags/base{}", flags));
        tests.push(Trial::test(format!("addr2line {}", flags), move || {
            run_test(in_path, out_path, &[&flags, "--all"])
        }));
    }

    let in_path = PathBuf::from("testinput/dwarf/base-gcc-split");
    let out_path = PathBuf::from("testoutput/flags/base-gcc-split-issue-352");
    tests.push(Trial::test("addr2line issue-352", move || {
        run_test(in_path, out_path, &["-afi", "0x1060"])
    }));

    tests
}

fn run_test(in_path: PathBuf, out_path: PathBuf, flags: &[&str]) -> Result<(), Failed> {
    let mut exe = env::current_exe().unwrap();
    assert!(exe.pop());
    if exe.file_name().unwrap().to_str().unwrap() == "deps" {
        assert!(exe.pop());
    }
    exe.push("addr2line");
    assert!(exe.is_file());

    let mut cmd = Command::new(exe);
    cmd.env("RUST_BACKTRACE", "1");
    cmd.arg("--exe").arg(in_path).args(flags.iter());

    let output = cmd.output().unwrap();
    assert!(output.status.success());
    let out_data = output.stdout;

    if env::var_os("ADDR2LINE_TESTOUTPUT_UPDATE").is_some() {
        fs::write(out_path, &out_data).unwrap();
        return Ok(());
    }

    let expect_out_data = fs::read(out_path).unwrap();
    if out_data != expect_out_data {
        let out_str = String::from_utf8_lossy(&out_data);
        let expect_out_str = String::from_utf8_lossy(&expect_out_data);
        return Err(format!(
            "\
            output mismatch\n\
            expected:\n\
            {expect_out_str}\n\
            actual:\n\
            {out_str}\n\
            Set ADDR2LINE_TESTOUTPUT_UPDATE=1 to update expected output"
        )
        .into());
    }

    Ok(())
}
