use crate::Pointee;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

pub struct Ref<'a, T: ?Sized + Pointee> {
    _phantom: PhantomData<&'a T>,
    meta: T::Meta,
    ptr: NonNull<()>,
}

impl<'a, T: ?Sized + Pointee> Ref<'a, T> {
    pub(crate) fn new(ptr: NonNull<()>, meta: T::Meta) -> Ref<'a, T> {
        Ref {
            _phantom: PhantomData,
            meta,
            ptr,
        }
    }

    pub(crate) fn into_raw_parts(self) -> (NonNull<()>, T::Meta) {
        (self.ptr, self.meta)
    }
}

impl<T: ?Sized + Pointee> Copy for Ref<'_, T> {}
impl<T: ?Sized + Pointee> Clone for Ref<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: ?Sized + Pointee> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        const {
            assert!(T::IS_STD_UNSIZED, "Attempted to dereference a `Ref` of non-STD supported unsized type. This is only provided as a limitation of arbitrary_self_types")
        }
        let ptr = T::from_raw_parts(self.ptr.as_ptr(), self.meta);
        unsafe { &*ptr }
    }
}

impl<T: Pointee<Meta = ()>> From<&T> for Ref<'_, T> {
    fn from(value: &T) -> Self {
        Ref::new(NonNull::from(value).cast(), ())
    }
}

// TODO: Should these exist? They'll cause a somewhat iffy compiler error if called on custom unsized types.
// impl<T: ?Sized + Pointee + PartialEq> PartialEq for Ref<'_, T> {
//     fn eq(&self, other: &Self) -> bool {
//         *self == *other
//     }
// }
//
// impl<T: ?Sized + Pointee + Debug> Debug for Ref<'_, T> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         T::fmt(self, f)
//     }
// }

pub struct RefMut<'a, T: ?Sized + Pointee> {
    _phantom: PhantomData<&'a mut T>,
    meta: T::Meta,
    ptr: NonNull<()>,
}

impl<'a, T: ?Sized + Pointee> RefMut<'a, T> {
    pub(crate) fn new(ptr: NonNull<()>, meta: T::Meta) -> RefMut<'a, T> {
        RefMut {
            _phantom: PhantomData,
            meta,
            ptr,
        }
    }

    pub(crate) fn into_raw_parts(self) -> (NonNull<()>, T::Meta) {
        (self.ptr, self.meta)
    }
}

impl<'a, T: ?Sized + Pointee> Deref for RefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        const {
            assert!(T::IS_STD_UNSIZED, "Attempted to dereference a `RefMut` of non-STD supported unsized type. This is only provided as a limitation of arbitrary_self_types")
        }
        let ptr = T::from_raw_parts(self.ptr.as_ptr(), self.meta);
        unsafe { &*ptr }
    }
}

impl<'a, T: ?Sized + Pointee> DerefMut for RefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        const {
            assert!(T::IS_STD_UNSIZED, "Attempted to dereference a `RefMut` of non-STD supported unsized type. This is only provided as a limitation of arbitrary_self_types")
        }
        let ptr = T::from_raw_parts(self.ptr.as_ptr(), self.meta);
        unsafe { &mut *ptr }
    }
}
