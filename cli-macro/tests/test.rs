
use cli_macro::*;

struct Unsupported;

enum DifferentRW<A, B> {
    R(A),
    W(B),
}

enum RW<A, B> {
    R(A),
    W(B),
}

trait ArgumentContainer {
    type Contains;
}

impl ArgumentContainer for () {
    type Contains = String;
}

#[generate_registers(to = RW_Two, unsupported = Unsupported, newtype=TestNew)]
enum Test {
    /// Hello
    A(RW<(), ()>),
    /// World!
    B(RW<Unsupported, ()>),
    C(RW<(), Unsupported>),
    D(RW<Unsupported, Unsupported>),
}

#[test]
fn foo() {

}

