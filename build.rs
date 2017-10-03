extern crate gcc;

fn main() {
    gcc::Build::new()
               .file("src/unwind_helper.c")
               .compile("unwind_helper");
}
