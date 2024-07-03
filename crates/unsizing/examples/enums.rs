#![feature(arbitrary_self_types)]

use unsizing::{unsize, Unsize};

/*
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

/// How do we want enums to work? Should they support variant-changing? Many questions.
/// Lets say we are working with ones still storing the tag inline - in the allocation.
///
/// Metadata is a union of the variants individual metadatas, and maybe some extra info
/// If we want to support variant-changing updates, metadata needs to keep around the max size
/// and align we support. Then when we change the variant, we alter the metadata which lets
/// us change how we read the fields, but we don't alter the size/align. This also means
/// we can do a check on variant swapping that we aren't violating those rules.
///
/// In turn, this means we'll want a way to say 'use this variant, but give it this size/align'
/// for allocating.
///
/// The question is whether to bother supporting it at all. It may be a stretch feature - skip
/// it for now, add it later as an option. After all, structs can't change their metadata normally.
/// This would be a fundamental change, allowing metadata to be altered implicitly and not as
/// a pointer cast.
*/
#[unsize]
// #[repr(u8)]
pub enum SliceOption {
    None,
    Some([u8]),
}

// #[unsize_to(SliceOption)]
#[repr(isize)]
pub enum SliceOptionSized<const N: usize> {
    None,
    Some([u8; N]),
}

impl<const N: usize> Unsize<SliceOption> for SliceOptionSized<N> {
    fn unsize(&self) -> unsizing::Ref<'_, SliceOption> {
        use unsizing::Ptr;
        unsafe {
            Ptr::from_raw_parts(
                Ptr::from(self).cast(),
                SliceOptionMeta {
                    None: (),
                    Some: (N,),
                },
            )
            .as_ref_unchecked()
        }
    }
}

fn main() {
    use unsizing::Ref;

    let opt_size = SliceOptionSized::<3>::None;
    let opt: Ref<'_, SliceOption> = opt_size.unsize();

    match opt.as_ref() {
        SliceOptionRef::None => (),
        SliceOptionRef::Some(_) => panic!("This shouldn't happen"),
    }

    let opt_size = SliceOptionSized::Some([1, 2, 3]);
    let opt: Ref<'_, SliceOption> = opt_size.unsize();

    match opt.as_ref() {
        SliceOptionRef::None => panic!("This shouldn't happen"),
        SliceOptionRef::Some(f) => assert_eq!(*f, [1, 2, 3]),
    }
}
