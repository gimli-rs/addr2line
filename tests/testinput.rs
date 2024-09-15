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
                    move || run_test(in_path, out_path),
                ));
            }
        }
    }
    tests
}

fn run_test(in_path: PathBuf, out_path: PathBuf) -> Result<(), Failed> {
    let mut exe = env::current_exe().unwrap();
    assert!(exe.pop());
    if exe.file_name().unwrap().to_str().unwrap() == "deps" {
        assert!(exe.pop());
    }
    exe.push("addr2line");
    assert!(exe.is_file());

    let mut cmd = Command::new(exe);
    cmd.env("RUST_BACKTRACE", "1");
    cmd.arg("--exe").arg(in_path).arg("--all").arg("-afi");

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
        return Err(
            format!("output mismatch\nexpected:\n{expect_out_str}\nactual:\n{out_str}").into(),
        );
    }

    Ok(())
}
