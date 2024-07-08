use crate::{Pointee, Ref, RefMut};
use std::ptr::NonNull;
use std::{fmt, ptr};

pub struct Ptr<T: ?Sized + Pointee> {
    meta: T::Meta,
    ptr: *const (),
}

impl<T: ?Sized + Pointee> Ptr<T> {
    fn new(ptr: *const (), meta: T::Meta) -> Ptr<T> {
        Ptr { ptr, meta }
    }

    pub fn null() -> Ptr<T>
    where
        T::Meta: Default,
    {
        Ptr {
            meta: T::Meta::default(),
            ptr: ptr::null(),
        }
    }

    pub fn null_meta(meta: T::Meta) -> Ptr<T> {
        Ptr {
            meta,
            ptr: ptr::null(),
        }
    }

    pub fn from_raw_parts(ptr: Ptr<()>, meta: T::Meta) -> Ptr<T> {
        Ptr::new(ptr.addr(), meta)
    }

    pub fn from_ref(r: Ref<'_, T>) -> Ptr<T> {
        let (ptr, meta) = Ref::into_raw_parts(r);
        Ptr::new(ptr.as_ptr(), meta)
    }

    pub fn addr(self) -> *const () {
        self.ptr
    }

    pub fn metadata(self) -> T::Meta {
        self.meta
    }

    pub unsafe fn byte_add(mut self, bytes: usize) -> Ptr<T> {
        self.ptr = self.ptr.byte_add(bytes);
        self
    }

    pub fn byte_align_offset(self, align: usize) -> usize {
        self.ptr.cast::<u8>().align_offset(align)
    }

    pub unsafe fn align_to(mut self, align: usize) -> Ptr<T> {
        let offset = self.byte_align_offset(align);
        self.ptr = self.ptr.byte_add(offset);
        self
    }

    pub fn cast<U: ?Sized + Pointee<Meta = T::Meta>>(self) -> Ptr<U> {
        Ptr::new(self.ptr, self.meta)
    }

    pub fn cast_meta<U: ?Sized + Pointee>(self, meta: U::Meta) -> Ptr<U> {
        Ptr::new(self.ptr, meta)
    }

    pub fn cast_mut(self) -> PtrMut<T> {
        PtrMut::new(self.ptr.cast_mut(), self.meta)
    }

    pub unsafe fn as_ref<'a>(self) -> Option<Ref<'a, T>> {
        NonNull::new(self.ptr.cast_mut()).map(|ptr| Ref::new(ptr, self.meta))
    }

    pub unsafe fn as_ref_unchecked<'a>(self) -> Ref<'a, T> {
        let ptr = NonNull::new(self.ptr.cast_mut()).unwrap_unchecked();
        Ref::new(ptr, self.meta)
    }
}

impl<T> Ptr<T> {
    pub unsafe fn read(self) -> T {
        self.ptr.cast::<T>().read()
    }
}

impl<T: ?Sized + Pointee> Copy for Ptr<T> {}
impl<T: ?Sized + Pointee> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Pointee<Meta = ()>> From<*const T> for Ptr<T> {
    fn from(value: *const T) -> Self {
        Ptr::new(value.cast(), ())
    }
}

impl<T: Pointee<Meta = ()>> From<&T> for Ptr<T> {
    fn from(value: &T) -> Self {
        Ptr::from(value as *const T)
    }
}

impl<T: ?Sized + Pointee> fmt::Debug for Ptr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.ptr)
    }
}

pub struct PtrMut<T: ?Sized + Pointee> {
    meta: T::Meta,
    ptr: *mut (),
}

impl<T: ?Sized + Pointee> PtrMut<T> {
    fn new(ptr: *mut (), meta: T::Meta) -> PtrMut<T> {
        PtrMut { ptr, meta }
    }

    pub fn from_mut(r: RefMut<'_, T>) -> PtrMut<T> {
        let (ptr, meta) = RefMut::into_raw_parts(r);
        PtrMut::new(ptr.as_ptr(), meta)
    }

    pub fn cast_const(self) -> Ptr<T> {
        Ptr::new(self.ptr.cast_const(), self.meta)
    }

    pub unsafe fn as_ref<'a>(self) -> Option<Ref<'a, T>> {
        NonNull::new(self.ptr).map(|ptr| Ref::new(ptr, self.meta))
    }

    pub unsafe fn as_ref_unchecked<'a>(self) -> Ref<'a, T> {
        let ptr = NonNull::new(self.ptr).unwrap_unchecked();
        Ref::new(ptr, self.meta)
    }

    pub unsafe fn as_mut<'a>(self) -> Option<RefMut<'a, T>> {
        NonNull::new(self.ptr).map(|ptr| RefMut::new(ptr, self.meta))
    }

    pub unsafe fn as_mut_unchecked<'a>(self) -> RefMut<'a, T> {
        let ptr = NonNull::new(self.ptr).unwrap_unchecked();
        RefMut::new(ptr, self.meta)
    }
}

impl<T> PtrMut<T> {
    pub unsafe fn read(self) -> T {
        self.ptr.cast::<T>().read()
    }

    pub unsafe fn write(self, val: T) {
        self.ptr.cast::<T>().write(val)
    }
}

impl<T: Pointee<Meta = ()>> From<*mut T> for PtrMut<T> {
    fn from(value: *mut T) -> Self {
        PtrMut::new(value.cast(), ())
    }
}
