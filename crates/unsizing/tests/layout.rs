#![feature(arbitrary_self_types)]

use unsizing::{unsize, Pointee, Ptr};

#[unsize]
struct PaddedStart {
    a: u8,
    b: u32,
}

#[test]
fn test_padded_start() {
    let layout = unsafe { PaddedStart::layout(Ptr::null()) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);
}

#[unsize]
struct PaddedEnd {
    a: u32,
    b: u8,
}

#[test]
fn test_padded_end() {
    let layout = unsafe { PaddedEnd::layout(Ptr::null()) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);
}

#[unsize]
struct MaybeAligned {
    a: [u8],
    b: [u32],
    c: [u8],
}

#[test]
fn test_maybe_aligned() {
    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 0, 0))) };
    assert_eq!(layout.size(), 0);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((1, 0, 0))) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 1, 0))) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((1, 1, 0))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 0, 1))) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((1, 0, 1))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 1, 1))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((1, 1, 1))) };
    assert_eq!(layout.size(), 12);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((4, 0, 0))) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((4, 1, 0))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 0, 4))) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((0, 1, 4))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((4, 0, 4))) };
    assert_eq!(layout.size(), 8);
    assert_eq!(layout.align(), 4);

    let layout = unsafe { MaybeAligned::layout(Ptr::null_meta((4, 1, 4))) };
    assert_eq!(layout.size(), 12);
    assert_eq!(layout.align(), 4);
}
