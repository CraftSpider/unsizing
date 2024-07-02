#![feature(arbitrary_self_types)]

use unsizing::{unsize, unsize_to};
// Now to figure out how to do constructors
// We want custom unsizing for one.

#[unsize]
pub struct Foo {
    a: u32,
    b: f32,
    c: [u8],
    d: [i32],
}

#[unsize_to(Foo)]
#[repr(C)]
#[derive(PartialEq, Clone, Debug)]
pub struct FooSized<const LEN_1: usize, const LEN_2: usize> {
    a: u32,
    b: f32,
    c: [u8; LEN_1],
    d: [i32; LEN_2],
}

#[unsize]
pub struct Bar {
    a: usize,
    b: f32,
    #[meta(a)]
    c: str,
}

#[unsize]
pub struct Baz(u32, Foo, u32);

#[unsize_to(Baz)]
#[derive(Debug)]
pub struct BazSized<const LEN_1: usize, const LEN_2: usize>(u32, FooSized<LEN_1, LEN_2>, u32);

fn main() {
    use unsizing::{Pointee, Ptr, Ref, Unsize};

    let foo_sized = FooSized {
        a: 1,
        b: 2.0,
        c: [1, 2, 3],
        d: [-1, -2, -3],
    };
    let foo: Ref<'_, Foo> = foo_sized.unsize();

    assert_eq!(*foo.a(), 1);
    assert_eq!(*foo.b(), 2.0);
    assert_eq!(*foo.c(), [1u8, 2, 3]);
    assert_eq!(*foo.d(), [-1i32, -2, -3]);

    let baz_sized = BazSized(10, foo_sized.clone(), 10);
    let baz: Ref<'_, Baz> = baz_sized.unsize();

    dbg!(&baz_sized);
    dbg!(Ptr::from_ref(baz));
    unsafe { dbg!(Pointee::layout(Ptr::from_ref(baz))) };

    assert_eq!(*baz.field_0(), 10);
    assert_eq!(*baz.field_1().a(), 1);
    assert_eq!(*baz.field_1().b(), 2.0);
    assert_eq!(*baz.field_1().c(), [1u8, 2, 3]);
    assert_eq!(*baz.field_1().d(), [-1i32, -2, -3]);
    assert_eq!(*baz.field_2(), 10);
}
