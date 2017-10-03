extern crate unwind;

struct Bomb(String);

impl Drop for Bomb {
    fn drop(&mut self) {
        println!("bomb dropped: {:?}", self.0);
    }
}

fn bar() {
    panic!("test panic");
}

fn foo() {
    let _b = Bomb("why though".to_owned());
    bar()
}

fn main() {
    foo();
    println!("down");
}
