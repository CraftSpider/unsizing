//! This is tricky and sensitive code, so...
//!
//! A type's metadata may be stored in a field *before* it.
//!
//! We want alignment to be able to depend on fields, but then nesting is hard - alignment of our
//! child becomes dependent on fields at offsets we need to know its alignment to access.
//!
//! This is fine when we assume we already have a pointer, but then when we try to calculate a field
//! pointer, we realize that we can't. It may be worth splitting `Pointee`, or adding a subtrait.
//! Fields of a `Pointee` must be `MetaAlign`.
//!
//! `MetaAlign` should be implemented for all types that don't use inline metadata OR are
//! `FixedAlign`.

#![warn(
    // missing_docs,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    missing_abi,
    noop_method_call,
    semicolon_in_expressions_from_macros,
    unused_import_braces,
    unused_lifetimes,
    clippy::cargo,
    clippy::missing_panics_doc,
    clippy::doc_markdown,
    clippy::ptr_as_ptr,
    clippy::cloned_instead_of_copied,
    clippy::unreadable_literal
)]

pub use unsizing_proc::{unsize, unsize_to};

mod inline;
mod pointee;
mod ptr;
mod refs;
mod unsize_;

pub use inline::Inline;
pub use pointee::{FixedAlign, FixedSize, Layout, MetaAlign, MetaSize, Pointee};
pub use ptr::{Ptr, PtrMut};
pub use refs::{Ref, RefMut};
pub use unsize_::Unsize;

#[doc(hidden)]
pub mod __impl {
    use crate::{Pointee, Ptr, Ref, Unsize};
    use std::marker::PhantomData;

    pub trait EnumHelper {
        type Phantom;
    }

    pub struct Metadata<T: ?Sized + Pointee>(pub T::Meta);

    pub fn unsize_check<T: ?Sized + Pointee, R: ?Sized + Pointee>(
        r: Ref<'_, T>,
        _: fn(Ref<'_, R>) -> Ref<'_, T>,
    ) -> T::Meta {
        Ptr::from_ref(r).metadata()
    }

    pub fn unsize_check_meta<T, U, R>(_: fn(Ref<'_, R>) -> Ref<'_, U>) -> U::Meta
    where
        T: ?Sized + Pointee + Unsize<U>,
        U: ?Sized + Pointee,
        R: ?Sized + Pointee,
    {
        T::unsize_meta()
    }

    pub fn unsize_check_meta_enum<T, U>(_: PhantomData<U>) -> U::Meta
    where
        T: ?Sized + Pointee + Unsize<U>,
        U: ?Sized + Pointee,
    {
        T::unsize_meta()
    }

    pub fn align_up(size: usize, align: usize) -> usize {
        if size % align == 0 {
            size
        } else {
            size + align - size % align
        }
    }
}
