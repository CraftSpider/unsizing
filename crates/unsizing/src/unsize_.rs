use crate::{Pointee, Ptr, Ref};

pub trait Unsize<U: ?Sized + Pointee> {
    fn unsize(&self) -> Ref<'_, U>;
}

impl<T> Unsize<T> for T {
    fn unsize(&self) -> Ref<'_, T> {
        Ref::from(self)
    }
}

impl<T, const N: usize> Unsize<[T]> for [T; N] {
    fn unsize(&self) -> Ref<'_, [T]> {
        unsafe { Ptr::from_raw_parts(Ptr::from(self).cast(), N).as_ref_unchecked() }
    }
}
