#![feature(arbitrary_self_types)]

use unsizing::{unsize, Unsize};
use unsizing_proc::unsize_to;

/// There are multiple options we wish to support for unsized enums:
/// - Metadata is a struct of variant metadata. When unsizing, we know each field from the
///   sized-types of that variant we're unsizing from.
///   - Pros: Easy unsizing, always calculable layout, variant-changing is possible without altering
///     the metadata
///   - Cons: Doesn't support `!MetaSized` types, since they might not be in the active variant
/// - Metadata is a layout and a union of variant metadata. When unsizing, we record the current
///   layout and metadata of the active variant.
///   - Pros: Smaller metadata in some cases, supports `!MetaSized` types
///   - Cons: Variant changing is impossible without altering the metadata
/// - Metadata is a union of variant metadata. We don't support unsizing, instead always
///   constructing an instance in-place.
///   - Pros: Smallest metadata. Supports `!MetaSized` types
///   - Cons: No unsizing, so construction is hard. Variant changing is impossible.
/// - Metadata is an *enum* of variant metadata. We support unsizing from the type
///   stored in the variant. This is like a fixed-count `dyn`, really.
///   - Pros: Small data storage in-memory, allows unsizing
///   - Cons: Variant changing is impossible
///
///
/// | Metadata Kind  | Unsize | Variant Change | `!MetaSized` | Comments |
/// |----------------|--------|----------------|--------------|----------|
/// | Struct         | ✓      | ✓              | ✗            | Most obvious choice for default |
/// | Layout + Union | ✓      | ~              | ✓            | Variant changing would require metadata changing |
/// | Enum           | ✓      | ✗              | ✓            | Smallest data in-memory |
/// | Union          | ✗      | ✗              | ✓            | Smallest metadata |
#[unsize]
#[repr(u8)]
pub enum SliceOption {
    None,
    Some([u8]),
}

#[unsize_to(SliceOption)]
#[repr(u8)]
pub enum SliceOptionSized<const N: usize> {
    None,
    Some([u8; N]),
}

fn main() {
    use unsizing::{Pointee, Ptr, Ref};

    let opt_size = SliceOptionSized::<3>::None;
    let opt: Ref<'_, SliceOption> = opt_size.unsize();

    let layout = unsafe { SliceOption::layout(Ptr::from_ref(opt)) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 1);

    match opt.as_ref() {
        SliceOptionRef::None => (),
        SliceOptionRef::Some(_) => panic!("This shouldn't happen"),
    }

    let opt_size = SliceOptionSized::Some([1, 2, 3]);
    let opt: Ref<'_, SliceOption> = opt_size.unsize();

    let layout = unsafe { SliceOption::layout(Ptr::from_ref(opt)) };
    assert_eq!(layout.size(), 4);
    assert_eq!(layout.align(), 1);

    match opt.as_ref() {
        SliceOptionRef::None => panic!("This shouldn't happen"),
        SliceOptionRef::Some(f) => assert_eq!(*f, [1, 2, 3]),
    }
}
