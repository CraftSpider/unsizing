use crate::Pointee;

pub struct Inline<T: Pointee> {
    meta: T::Meta,
    data: T,
}
