use crate::{Pointee, Ptr, Ref};

pub trait Unsize<U: ?Sized + Pointee>: Sized {
    fn unsize_meta() -> U::Meta;
    fn unsize(&self) -> Ref<'_, U> {
        unsafe {
            Ptr::from_raw_parts(Ptr::from(self).cast(), Self::unsize_meta()).as_ref_unchecked()
        }
    }
}

impl<T> Unsize<T> for T {
    fn unsize_meta() -> <T as Pointee>::Meta {
        ()
    }

    fn unsize(&self) -> Ref<'_, T> {
        Ref::from(self)
    }
}

impl<T, const N: usize> Unsize<[T]> for [T; N] {
    fn unsize_meta() -> <[T] as Pointee>::Meta {
        N
    }

    fn unsize(&self) -> Ref<'_, [T]> {
        unsafe { Ptr::from_raw_parts(Ptr::from(self).cast(), N).as_ref_unchecked() }
    }
}
