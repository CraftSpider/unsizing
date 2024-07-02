#![feature(arbitrary_self_types)]

use std::mem::MaybeUninit;
use unsizing::{Layout, MetaAlign, Pointee, Ptr, Ref, RefMut, Unsize};

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
// #[unsize]
// #[repr(u8)]
// pub enum SliceOption {
//     None,
//     Some([u8]),
// }

pub struct SliceOption([MaybeUninit<u8>]);

type SliceOptionRepr = u8;

#[derive(Copy, Clone)]
pub struct SliceOptionMeta {
    None: (),
    Some: (usize,),
}

#[derive(Clone)]
pub enum SliceOptionFields {
    None,
    Some(Ptr<[u8]>),
}

pub enum SliceOptionRef<'a> {
    None,
    Some(Ref<'a, [u8]>),
}

pub enum SliceOptionRefMut<'a> {
    None,
    Some(RefMut<'a, [u8]>),
}

impl SliceOption {
    fn as_ref<'a>(self: Ref<'a, SliceOption>) -> SliceOptionRef<'a> {
        let layout = unsafe { Self::layout(Ptr::from_ref(self)) };
        match layout.fields() {
            SliceOptionFields::None => SliceOptionRef::None,
            SliceOptionFields::Some(field_0) => {
                SliceOptionRef::Some(unsafe { field_0.as_ref_unchecked() })
            }
        }
    }
}

impl Pointee for SliceOption {
    const IS_STD_UNSIZED: bool = false;
    // We need to track the size/align of the original enum location - since it may be greater than
    // what we see just from our current state. (EG SliceOptionSized::<[u32; 4]::None - our unsized
    // size is 1, align 1, but actual size/align is 20/4).
    type Meta = SliceOptionMeta;
    type Fields = SliceOptionFields;

    unsafe fn layout(ptr: Ptr<Self>) -> Layout<Self> {
        let variant = ptr.cast_meta::<SliceOptionRepr>(()).read();
        let meta = ptr.metadata();

        let align = [
            <SliceOptionRepr as MetaAlign>::align(()),
            <[u8] as MetaAlign>::align(meta.Some.0),
        ]
        .into_iter()
        .max()
        .unwrap_or(1);

        let repr_size = unsizing::__impl::align_up(size_of::<SliceOptionRepr>(), align);
        let ptr = ptr.byte_add(repr_size);

        let mut fields = MaybeUninit::uninit();
        let sizes = [
            {
                let mut size = 0;
                if variant == 0 {
                    fields.write(SliceOptionFields::None);
                }
                size
            },
            {
                let mut size = 0;
                /* struct size/align code here */
                let field_0 = {
                    let field_align = <[u8] as unsizing::MetaAlign>::align(meta.Some.0);
                    let field_size = <[u8] as unsizing::MetaSize>::size(meta.Some.0);
                    let offset = ptr.byte_add(size).byte_align_offset(field_align);
                    let ptr = ptr.byte_add(size + offset).cast_meta::<[u8]>(meta.Some.0);
                    size += offset + field_size;
                    ptr
                };
                if variant == 1 {
                    fields.write(SliceOptionFields::Some(field_0));
                }
                size
            },
        ];

        // SAFETY: Validity precondition that tag is a valid variant
        let fields = fields.assume_init();
        let var_size = sizes.into_iter().max().unwrap_or(0);
        // Size is largest variant + enough space for tag
        Layout::new(fields, repr_size + var_size, align)
    }
}

// #[unsize_to(SliceOption)]
#[repr(u8)]
pub enum SliceOptionSized<const N: usize> {
    None,
    Some([u8; N]),
}

impl<const N: usize> Unsize<SliceOption> for SliceOptionSized<N> {
    fn unsize(&self) -> Ref<'_, SliceOption> {
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
