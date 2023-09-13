struct A; // Big struct
struct B; // Also big struct
enum Thing { A(A), B(B), }

trait CommonBehaviour {
    fn greet(&self);
}

impl CommonBehaviour for A {
    fn greet(&self) {
        println!("Hello from A");
    }
}

impl CommonBehaviour for B {
    fn greet(&self) {
        println!("Hello from B");
    }
}

fn use_thing<T: CommonBehaviour>(thing: T) {
    for i in 0..100000 {
        thing.greet();
    }
}

fn main() {
    let thing: Thing = load_thing();
    use_thing(&thing);
}