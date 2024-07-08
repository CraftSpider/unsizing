use crate::Ptr;
use std::{fmt, mem, ptr};

pub struct Layout<T: ?Sized + Pointee> {
    fields: T::Fields,
    size: usize,
    align: usize,
}

impl<T: ?Sized + Pointee> Layout<T> {
    pub fn new(fields: T::Fields, size: usize, align: usize) -> Layout<T> {
        Layout {
            fields,
            size,
            align,
        }
    }

    pub fn fields(&self) -> &T::Fields {
        &self.fields
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn align(&self) -> usize {
        self.align
    }
}

impl<T: ?Sized + Pointee> fmt::Debug for Layout<T>
where
    T::Fields: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Layout")
            .field("fields", &self.fields)
            .field("size", &self.size)
            .field("align", &self.align)
            .finish()
    }
}

pub trait Pointee {
    const IS_STD_UNSIZED: bool;
    type Meta: 'static + Copy + Send + Sync + Unpin;
    type Fields;

    unsafe fn layout(ptr: Ptr<Self>) -> Layout<Self>;

    #[doc(hidden)]
    fn from_raw_parts(ptr: *mut (), meta: Self::Meta) -> *mut Self {
        #![allow(unused_variables)]
        unreachable!(
            "`from_raw_parts` is an implementation detail for IS_STD_UNSIZED types, and should not \
            be invoked on custom unsized types."
        )
    }
    #[doc(hidden)]
    fn metadata(ptr: *const Self) -> Self::Meta {
        #![allow(unused_variables)]
        unreachable!(
            "`metadata` is an implementation detail for IS_STD_UNSIZED types, and should not \
            be invoked on custom unsized types."
        )
    }
}

pub trait MetaAlign: Pointee {
    fn align(meta: Self::Meta) -> usize;
}

impl<T: ?Sized + FixedAlign> MetaAlign for T {
    fn align(_: Self::Meta) -> usize {
        <T as FixedAlign>::align()
    }
}

pub trait FixedAlign: Pointee {
    fn align() -> usize;
}

pub trait MetaSize: Pointee {
    fn size(meta: Self::Meta) -> usize;
}

impl<T: ?Sized + FixedSize> MetaSize for T {
    fn size(_: Self::Meta) -> usize {
        <T as FixedSize>::size()
    }
}

/// This is equivalent to `Sized`, basically
pub trait FixedSize: Pointee {
    fn size() -> usize;
}

impl<T> Pointee for T {
    const IS_STD_UNSIZED: bool = true;
    type Meta = ();
    type Fields = ();

    unsafe fn layout(_: Ptr<Self>) -> Layout<Self>
    where
        Self: Sized,
    {
        Layout {
            fields: (),
            size: <Self as FixedSize>::size(),
            align: <Self as FixedAlign>::align(),
        }
    }

    fn from_raw_parts(ptr: *mut (), _: Self::Meta) -> *mut Self {
        ptr.cast::<T>()
    }

    fn metadata(_: *const Self) -> Self::Meta {}
}

impl<T> FixedAlign for T {
    fn align() -> usize {
        mem::align_of::<Self>()
    }
}

impl<T> FixedSize for T {
    fn size() -> usize {
        mem::size_of::<Self>()
    }
}

impl<T> Pointee for [T] {
    const IS_STD_UNSIZED: bool = true;
    type Meta = usize;
    type Fields = ();

    unsafe fn layout(ptr: Ptr<Self>) -> Layout<Self> {
        let meta = ptr.metadata();
        Layout {
            fields: (),
            size: <Self as MetaSize>::size(meta),
            align: <Self as FixedAlign>::align(),
        }
    }

    fn from_raw_parts(ptr: *mut (), meta: Self::Meta) -> *mut Self {
        ptr::slice_from_raw_parts_mut(ptr.cast::<T>(), meta)
    }

    fn metadata(ptr: *const Self) -> Self::Meta {
        ptr.len()
    }
}

impl<T> FixedAlign for [T] {
    fn align() -> usize {
        mem::align_of::<T>()
    }
}

impl<T> MetaSize for [T] {
    fn size(meta: Self::Meta) -> usize {
        meta * mem::size_of::<T>()
    }
}

impl Pointee for str {
    const IS_STD_UNSIZED: bool = true;
    type Meta = usize;
    type Fields = ();

    unsafe fn layout(ptr: Ptr<Self>) -> Layout<Self> {
        let meta = ptr.metadata();
        Layout {
            fields: (),
            size: <Self as MetaSize>::size(meta),
            align: <Self as FixedAlign>::align(),
        }
    }

    fn from_raw_parts(ptr: *mut (), meta: Self::Meta) -> *mut Self {
        ptr::slice_from_raw_parts_mut(ptr, meta) as *mut str
    }

    fn metadata(ptr: *const Self) -> Self::Meta {
        (ptr as *const [u8]).len()
    }
}

impl FixedAlign for str {
    fn align() -> usize {
        mem::align_of::<u8>()
    }
}

impl MetaSize for str {
    fn size(meta: Self::Meta) -> usize {
        meta * mem::size_of::<u8>()
    }
}

// TODO: Support for DynMetadata under a feature. Runs into https://github.com/rust-lang/rust/issues/20400
//       so gets a bit fancy to work around that.
/*
mod private {
    use super::*;

    pub trait Thin: std::ptr::Pointee<Metadata = ()> {}

    impl<T: ?Sized + std::ptr::Pointee<Metadata = ()>> Thin for T {}

    pub trait Usize: std::ptr::Pointee<Metadata = usize> {}

    impl<T: ?Sized + std::ptr::Pointee<Metadata = usize>> Usize for T {}

    pub trait Dyn<D: ?Sized>: std::ptr::Pointee<Metadata = DynMetadata<D>> {}

    impl<D: ?Sized + std::ptr::Pointee<Metadata = DynMetadata<D>>> Dyn<D> for D {}

    trait PointeeHelper<T> {}

    // This currently covers all possible pointees that aren't the slices we rely on.
    impl<T: ?Sized + Thin> PointeeHelper<()> for T {}
    impl<T: ?Sized + Usize> PointeeHelper<usize> for [T] {}
    impl<T: ?Sized + Usize> PointeeHelper<usize> for str {}
    impl<T: ?Sized + Dyn<T>> PointeeHelper<DynMetadata<T>> for T {}

    impl<T: ?Sized + PointeeHelper<<T as std::ptr::Pointee>::Metadata>> Pointee for T {
        const IS_STD_UNSIZED: bool = true;
        type Meta = <T as std::ptr::Pointee>::Metadata;
        type Fields = ();

        unsafe fn layout(ptr: Ptr<Self>) -> Layout<Self> {
            let meta = ptr.metadata();
            Layout {
                fields: (),
                size: <Self as MetaSize>::size(meta),
                align: <Self as MetaAlign>::align(meta),
            }
        }
    }

    impl<T: ?Sized + PointeeHelper<<T as std::ptr::Pointee>::Metadata>> MetaAlign for
}
 */
