use crate::util::{ItemExt, MultiError, ReprTy};
use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};

use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    AttrStyle, Attribute, Error, Field, FieldMutability, Fields, FieldsNamed, FieldsUnnamed,
    GenericParam, Generics, Index, Item, ItemEnum, ItemStruct, Lifetime, LifetimeParam,
    MacroDelimiter, Member, Meta, MetaList, Type, TypeTuple, Variant, Visibility,
};

#[derive(Clone)]
enum MetaLoc {
    Field(Ident),
    Tuple(Index),
    Variant(Ident, Index),
}

impl ToTokens for MetaLoc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let toks = match self {
            MetaLoc::Field(name) => quote_spanned!(name.span() => #name.read()),
            MetaLoc::Tuple(idx) => quote_spanned!(idx.span() => meta.#idx),
            MetaLoc::Variant(var, idx) => quote_spanned!(var.span() => meta.#var.#idx),
        };
        tokens.extend(toks);
    }
}

fn process_field_attr(
    meta: &mut Option<MetaLoc>,
    field: &Field,
    attr: &Attribute,
) -> Result<(), MultiError> {
    if attr.path().is_ident("meta") {
        if meta.is_some() {
            return Err(Error::new(attr.span(), "second `#[meta]` attribute on field").into());
        }
        *meta = Some(match &attr.meta {
            Meta::List(l) => {
                let field = syn::parse2::<Member>(l.tokens.clone())?;
                let name = match field {
                    Member::Named(name) => name,
                    Member::Unnamed(idx) => Ident::new(&format!("field_{}", idx.index), idx.span()),
                };
                Ok(MetaLoc::Field(name))
            }
            Meta::Path(_) | Meta::NameValue(_) => Err(Error::new(
                attr.span(),
                "`#[meta]` attribute expects a field identifier or index",
            )),
        }?);
        Ok(())
    } else {
        Err(Error::new(
            field.span(),
            format!(
                "`#[unsize]` doesn't support field attribute {:?}",
                attr.path()
            ),
        )
        .into())
    }
}

fn process_field(
    field_names: &mut Vec<Ident>,
    field_tys: &mut Vec<Type>,
    metadata: &mut Vec<MetaLoc>,
    idx: usize,
    field: &Field,
    enum_var: Option<&Ident>,
) -> Result<(), MultiError> {
    let mut meta = None;
    let mut errs = MultiError::empty();

    for attr in &field.attrs {
        errs.and(process_field_attr(&mut meta, field, attr));
    }

    let name = match field.ident.as_ref() {
        Some(name) => name.clone(),
        None => Ident::new(&format!("field_{}", idx), field.span()),
    };

    field_names.push(name);
    field_tys.push(field.ty.clone());
    metadata.push(meta.unwrap_or_else(|| match enum_var {
        Some(var) => MetaLoc::Variant(var.clone(), Index::from(idx)),
        None => MetaLoc::Tuple(Index::from(idx)),
    }));

    errs.empty_or(())
}

fn impls_struct(s: &ItemStruct, attrs: &Attrs<'_>) -> Result<TokenStream, MultiError> {
    if attrs.repr == ReprTy::None {
        return Err(Error::new(
            s.ident.span(),
            "Missing repr type on `#[unsize]` item. The implicit `repr(Rust)` is unsupported.",
        )
        .into());
    } else if attrs.repr != ReprTy::C {
        return Err(Error::new(
            s.ident.span(),
            "Invalid repr for `#[unsize]` on a struct. Expected `C`",
        )
        .into());
    }

    let name = &s.ident;
    let fields = &s.fields;
    let mut field_names = Vec::new();
    let mut field_tys = Vec::new();
    let mut read_meta = Vec::new();

    if let Fields::Unit = fields {
        let err = Error::new(
            fields.span(),
            "`#[unsize]` currently doesn't support unit structs",
        )
        .into();
        return Err(err);
    }
    fields
        .iter()
        .enumerate()
        .fold(MultiError::empty(), |mut err, (idx, field)| {
            err.and(process_field(
                &mut field_names,
                &mut field_tys,
                &mut read_meta,
                idx,
                field,
                None,
            ));
            err
        })
        .empty_or(())?;

    let is_meta_layout = read_meta
        .iter()
        .all(|meta| matches!(meta, MetaLoc::Tuple(_)));

    let field_names_mut = field_names
        .iter()
        .map(|name| {
            let mut new_name = name.to_string();
            new_name.push_str("_mut");
            Ident::new(&new_name, name.span())
        })
        .collect::<Vec<_>>();

    let field_vis = fields.iter().map(|field| &field.vis).collect::<Vec<_>>();

    let metadata = field_tys
        .iter()
        .zip(&read_meta)
        .map(|(ty, meta)| match meta {
            MetaLoc::Tuple(_) => quote_spanned!(ty.span() => <#ty as unsizing::Pointee>::Meta),
            MetaLoc::Field(_) => quote_spanned!(ty.span() => ()),
            MetaLoc::Variant(_, _) => unreachable!(),
        })
        .collect::<Vec<_>>();

    let ptr_fields = field_tys
        .iter()
        .map(|ty| quote_spanned!(ty.span() => unsizing::Ptr<#ty>));

    let idxs = field_tys
        .iter()
        .enumerate()
        .map(|(idx, _)| Index::from(idx))
        .collect::<Vec<_>>();

    let layout_impl = if is_meta_layout {
        quote_spanned!(name.span() =>
            impl unsizing::MetaSize for #name {
                fn size(meta: <Self as unsizing::Pointee>::Meta) -> usize {
                    let mut size = 1;
                    #(
                    size += <#field_tys as unsizing::MetaSize>::size(#read_meta);
                    )*
                    size
                }
            }

            impl unsizing::MetaAlign for #name {
                fn align(meta: <Self as unsizing::Pointee>::Meta) -> usize {
                    let mut align = 1;
                    #(
                    let field_align = <#field_tys as unsizing::MetaAlign>::align(#read_meta);
                    align = std::cmp::max(align, field_align);
                    )*
                    align
                }
            }
        )
    } else {
        TokenStream::new()
    };

    Ok(quote_spanned!(fields.span() =>
        impl #name {
            #(
            #field_vis fn #field_names<'a>(self: unsizing::Ref<'a, Self>) -> unsizing::Ref<'a, #field_tys> {
                let ptr = unsafe { <Self as unsizing::Pointee>::layout(unsizing::Ptr::from_ref(self)).fields().#idxs };
                unsafe { ptr.as_ref_unchecked() }
            }

            #field_vis fn #field_names_mut<'a>(self: unsizing::RefMut<'a, Self>) -> unsizing::RefMut<'a, #field_tys> {
                let ptr = unsafe { <Self as unsizing::Pointee>::layout(unsizing::PtrMut::from_mut(self).cast_const()).fields().#idxs }.cast_mut();
                unsafe { ptr.as_mut_unchecked() }
            }
            )*
        }

        impl unsizing::Pointee for #name {
            const IS_STD_UNSIZED: bool = false;
            type Meta = ( #( #metadata, )* );
            type Fields = ( #( #ptr_fields, )* );

            unsafe fn layout(ptr: unsizing::Ptr<Self>) -> unsizing::Layout<Self> {
                let meta = ptr.metadata();
                let mut size = 0;
                let mut align = 1;

                #(
                let #field_names = {
                    let field_align = <#field_tys as unsizing::MetaAlign>::align(#read_meta);
                    let offset = ptr.byte_add(size).byte_align_offset(field_align);
                    let ptr = ptr.byte_add(size + offset)
                        .cast_meta::<#field_tys>(#read_meta);
                    let layout = <#field_tys as unsizing::Pointee>::layout(ptr);
                    size += offset + layout.size();
                    align = std::cmp::max(align, layout.align());
                    ptr
                };
                )*

                if size % align != 0 {
                    size += align - (size % align);
                }

                unsizing::Layout::new(( #( #field_names, )* ), size, align)
            }
        }

        #layout_impl
    ))
}

fn make_meta_ty(e: &ItemEnum) -> ItemStruct {
    let name = Ident::new(&(e.ident.to_string() + "Meta"), e.ident.span());

    let fields = e.variants.iter().fold(
        FieldsNamed {
            brace_token: Default::default(),
            named: Punctuated::default(),
        },
        |mut fields, variant| {
            let tys = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = &field.ty;
                    syn::parse2::<Type>(
                        quote_spanned!(ty.span() => <#ty as unsizing::Pointee>::Meta),
                    )
                    .unwrap()
                })
                .collect();
            fields.named.push(Field {
                attrs: vec![],
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: Some(variant.ident.clone()),
                colon_token: Default::default(),
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: tys,
                }),
            });
            fields
        },
    );
    ItemStruct {
        attrs: vec![
            Attribute {
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                bracket_token: Default::default(),
                meta: Meta::List(MetaList {
                    path: Ident::new("derive", name.span()).into(),
                    delimiter: MacroDelimiter::Paren(Default::default()),
                    tokens: quote_spanned!(name.span() => Copy, Clone),
                }),
            },
            Attribute {
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                bracket_token: Default::default(),
                meta: Meta::List(MetaList {
                    path: Ident::new("allow", name.span()).into(),
                    delimiter: MacroDelimiter::Paren(Default::default()),
                    tokens: quote_spanned!(name.span() => non_snake_case),
                }),
            },
        ],
        vis: e.vis.clone(),
        struct_token: Default::default(),
        ident: name,
        generics: Default::default(),
        fields: Fields::Named(fields),
        semi_token: None,
    }
}

fn make_phantom_ty(e: &ItemEnum) -> ItemStruct {
    let name = Ident::new(&(e.ident.to_string() + "Phantom"), e.ident.span());

    let fields = e.variants.iter().fold(
        FieldsNamed {
            brace_token: Default::default(),
            named: Punctuated::default(),
        },
        |mut fields, variant| {
            let tys = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = &field.ty;
                    syn::parse2::<Type>(quote_spanned!(ty.span() => std::marker::PhantomData<#ty>))
                        .unwrap()
                })
                .collect();
            fields.named.push(Field {
                attrs: vec![],
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: Some(variant.ident.clone()),
                colon_token: Default::default(),
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: tys,
                }),
            });
            fields
        },
    );
    ItemStruct {
        attrs: vec![
            Attribute {
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                bracket_token: Default::default(),
                meta: Meta::List(MetaList {
                    path: Ident::new("derive", name.span()).into(),
                    delimiter: MacroDelimiter::Paren(Default::default()),
                    tokens: quote_spanned!(name.span() => Copy, Clone),
                }),
            },
            Attribute {
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                bracket_token: Default::default(),
                meta: Meta::List(MetaList {
                    path: Ident::new("allow", name.span()).into(),
                    delimiter: MacroDelimiter::Paren(Default::default()),
                    tokens: quote_spanned!(name.span() => non_snake_case),
                }),
            },
        ],
        vis: e.vis.clone(),
        struct_token: Default::default(),
        ident: name,
        generics: Default::default(),
        fields: Fields::Named(fields),
        semi_token: None,
    }
}

fn make_field_ty(e: &ItemEnum) -> ItemEnum {
    let name = Ident::new(&(e.ident.to_string() + "Fields"), e.ident.span());

    let variants = e
        .variants
        .iter()
        .map(|variant| {
            let fields = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = &field.ty;
                    Field {
                        attrs: vec![],
                        vis: Visibility::Inherited,
                        mutability: FieldMutability::None,
                        ident: None,
                        colon_token: None,
                        ty: syn::parse2(quote_spanned!(field.span() => unsizing::Ptr<#ty>))
                            .unwrap(),
                    }
                })
                .collect();

            let fields = Fields::Unnamed(FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: fields,
            });

            Variant {
                attrs: vec![],
                ident: variant.ident.clone(),
                fields,
                discriminant: None,
            }
        })
        .collect();

    ItemEnum {
        attrs: vec![],
        vis: e.vis.clone(),
        enum_token: Default::default(),
        ident: name,
        generics: Default::default(),
        brace_token: Default::default(),
        variants,
    }
}

fn make_ref_tys(e: &ItemEnum) -> (ItemEnum, ItemEnum) {
    let ref_name = Ident::new(&(e.ident.to_string() + "Ref"), e.ident.span());
    let mut_name = Ident::new(&(e.ident.to_string() + "Mut"), e.ident.span());

    let ref_variants = e
        .variants
        .iter()
        .map(|variant| {
            let mut out = variant.clone();
            out.fields.iter_mut().for_each(|field| {
                let ty = &field.ty;
                let ty = syn::parse2(quote!(unsizing::Ref<'a, #ty>)).unwrap();
                field.ty = ty;
            });
            out
        })
        .collect();

    let mut_variants = e
        .variants
        .iter()
        .map(|variant| {
            let mut out = variant.clone();
            out.fields.iter_mut().for_each(|field| {
                let ty = &field.ty;
                let ty = syn::parse2(quote!(unsizing::RefMut<'a, #ty>)).unwrap();
                field.ty = ty;
            });
            out
        })
        .collect();

    let params = Punctuated::from_iter([GenericParam::Lifetime(LifetimeParam {
        attrs: vec![],
        lifetime: Lifetime::new("'a", e.ident.span()),
        colon_token: None,
        bounds: Default::default(),
    })]);

    let ref_ty = ItemEnum {
        attrs: vec![],
        vis: e.vis.clone(),
        enum_token: Default::default(),
        ident: ref_name,
        generics: Generics {
            lt_token: None,
            params: params.clone(),
            gt_token: None,
            where_clause: None,
        },
        brace_token: Default::default(),
        variants: ref_variants,
    };
    let mut_ty = ItemEnum {
        attrs: vec![],
        vis: e.vis.clone(),
        enum_token: Default::default(),
        ident: mut_name,
        generics: Generics {
            lt_token: None,
            params,
            gt_token: None,
            where_clause: None,
        },
        brace_token: Default::default(),
        variants: mut_variants,
    };
    (ref_ty, mut_ty)
}

fn impls_enum(e: &ItemEnum, attrs: &Attrs<'_>) -> Result<TokenStream, MultiError> {
    let mut errs = MultiError::empty();

    let repr_ty = Ident::new(
        match attrs.repr {
            ReprTy::C => "isize",
            ReprTy::U8 => "u8",
            ReprTy::I8 => "i8",
            ReprTy::U16 => "u16",
            ReprTy::I16 => "i16",
            ReprTy::U32 => "u32",
            ReprTy::I32 => "i32",
            ReprTy::U64 => "u64",
            ReprTy::I64 => "i64",
            ReprTy::USize => "usize",
            ReprTy::ISize => "isize",
            ReprTy::None => {
                errs.push(Error::new(e.ident.span(),
                                 "Missing repr type on `#[unsize]` item. The implicit `repr(Rust)` is unsupported."));
                ""
            }
        },
        Span::mixed_site(),
    );

    let name = &e.ident;
    let vis = &e.vis;
    let variants = &e.variants;

    let meta_ty = make_meta_ty(e);
    let phantom_ty = make_phantom_ty(e);
    let field_ty = make_field_ty(e);
    let (ref_ty, mut_ty) = make_ref_tys(e);

    let mut all_field_tys = Vec::new();
    let mut all_field_metas = Vec::new();
    let mut variant_names = Vec::new();
    let mut variant_vals = Vec::new();
    let mut variant_fields = Vec::new();
    let mut cur_var_val = 0;
    for variant in variants {
        variant_names.push(variant.ident.clone());
        let var_val = match &variant.discriminant {
            None => {
                let var_val = Literal::i32_unsuffixed(cur_var_val);
                cur_var_val += 1;
                quote_spanned!(variant.span() => #var_val)
            }
            Some((_, val)) => val.to_token_stream(),
        };
        variant_vals.push(var_val);

        let mut field_names = Vec::new();
        let mut field_tys = Vec::new();
        let mut read_meta = Vec::new();
        for (field_idx, field) in variant.fields.iter().enumerate() {
            all_field_tys.push(field.ty.clone());
            errs.and(process_field(
                &mut field_names,
                &mut field_tys,
                &mut read_meta,
                field_idx,
                field,
                Some(&variant.ident),
            ));
            all_field_metas.push(read_meta.last().unwrap().clone());
        }
        assert!(read_meta
            .iter()
            .all(|loc| matches!(loc, MetaLoc::Variant(..))));
        variant_fields.push((field_names, field_tys, read_meta));
    }

    let (variant_size_field_calc, field_tuple): (Vec<_>, Vec<_>) = variant_fields
        .iter()
        .map(|(field_names, field_tys, read_meta)| {
            let a = quote_spanned!(
                name.span() =>
                #(
                let #field_names = {
                    let field_align = <#field_tys as unsizing::MetaAlign>::align(#read_meta);
                    let field_size = <#field_tys as unsizing::MetaSize>::size(#read_meta);
                    let offset = ptr.byte_add(size).byte_align_offset(field_align);
                    let ptr = ptr.byte_add(size + offset).cast_meta::<#field_tys>(#read_meta);
                    size += offset + field_size;
                    ptr
                };
                )*
            );
            let b = quote_spanned!(
                name.span() =>
                #( #field_names, )*
            );
            (a, b)
        })
        .unzip();

    let (var_match_fields, var_as_ref, var_as_mut) = variant_fields.iter().zip(variants).fold(
        (Vec::new(), Vec::new(), Vec::new()),
        |(mut var_match_fields, mut var_as_ref, mut var_as_mut), ((field_names, _, _), variant)| {
            let members = variant
                .fields
                .iter()
                .enumerate()
                .map(|(idx, f)| match &f.ident {
                    Some(name) => Member::Named(name.clone()),
                    None => Member::Unnamed(Index::from(idx)),
                })
                .collect::<Vec<_>>();
            var_match_fields.push(quote_spanned!(name.span() => #( #field_names, )* ));
            var_as_ref.push(
                quote_spanned!(name.span() => #( #members: #field_names.as_ref_unchecked(), )* ),
            );
            var_as_mut.push(
                quote_spanned!(name.span() => #( #members: #field_names.cast_mut().as_mut_unchecked(), )* ),
            );
            (var_match_fields, var_as_ref, var_as_mut)
        },
    );

    errs.empty_or(())?;

    let meta_ty_name = &meta_ty.ident;
    let phantom_ty_name = &phantom_ty.ident;
    let field_ty_name = &field_ty.ident;
    let ref_ty_name = &ref_ty.ident;
    let mut_ty_name = &mut_ty.ident;

    Ok(quote_spanned!(name.span() =>
        #meta_ty
        #field_ty
        #ref_ty
        #mut_ty

        impl #name {
            #vis fn as_ref<'a>(self: unsizing::Ref<'a, Self>) -> #ref_ty_name<'a> {
                let layout = unsafe { <Self as unsizing::Pointee>::layout(unsizing::Ptr::from_ref(self)) };
                #[allow(unused_unsafe, clippy::init_numbered_fields)]
                match layout.fields() {
                    #(
                    #field_ty_name::#variant_names( #var_match_fields ) => unsafe { #ref_ty_name::#variant_names { #var_as_ref } },
                    )*
                }
            }

            #vis fn as_mut<'a>(self: unsizing::RefMut<'a, Self>) -> #mut_ty_name<'a> {
                let layout = unsafe { <Self as unsizing::Pointee>::layout(unsizing::PtrMut::from_mut(self).cast_const()) };
                #[allow(unused_unsafe, clippy::init_numbered_fields)]
                match layout.fields() {
                    #(
                    #field_ty_name::#variant_names( #var_match_fields ) => unsafe { #mut_ty_name::#variant_names { #var_as_mut } },
                    )*
                }
            }
        }

        impl unsizing::Pointee for #name {
            const IS_STD_UNSIZED: bool = false;
            // We need to track the size/align of the original enum location - since it may be greater than
            // what we see just from our current state. (EG SliceOptionSized::<[u32; 4]::None - our unsized
            // size is 1, align 1, but actual size/align is 20/4).
            type Meta = #meta_ty_name;
            type Fields = #field_ty_name;

            unsafe fn layout(ptr: unsizing::Ptr<Self>) -> unsizing::Layout<Self> {
                let variant = ptr.cast_meta::<#repr_ty>(()).read();
                let meta = ptr.metadata();

                let align = [
                    <#repr_ty as unsizing::MetaAlign>::align(()),
                    #(
                    <#all_field_tys as unsizing::MetaAlign>::align(#all_field_metas)
                    )*
                ]
                .into_iter()
                .max()
                .unwrap_or(1);

                let repr_size = unsizing::__impl::align_up(size_of::<#repr_ty>(), align);
                let ptr = ptr.byte_add(repr_size);

                let mut fields = std::mem::MaybeUninit::uninit();
                let sizes = [
                    #(
                    {
                        #[allow(unused_mut)]
                        let mut size = 0;
                        #variant_size_field_calc
                        if variant == #variant_vals {
                            fields.write(#field_ty_name::#variant_names( #field_tuple ));
                        }
                        size
                    },
                    )*
                ];

                // SAFETY: Validity precondition that tag is a valid variant
                let fields = fields.assume_init();
                let var_size = sizes.into_iter().max().unwrap_or(0);
                // Size is largest variant + enough space for tag
                unsizing::Layout::new(fields, repr_size + var_size, align)
            }
        }

        #phantom_ty

        impl unsizing::__impl::EnumHelper for #name {
            type Phantom = #phantom_ty_name;
        }
    ))
}

#[derive(Default)]
struct Attrs<'a> {
    docs: Vec<&'a Attribute>,
    repr: ReprTy,
}

pub fn _impl(ts: TokenStream) -> Result<TokenStream, MultiError> {
    let item = syn::parse2::<Item>(ts)?;
    let mut errs = MultiError::empty();

    let mut attrs = Attrs::default();
    for attr in item.attrs() {
        if attr.path().is_ident("repr") {
            if let Some(repr) = errs.and(ReprTy::parse_attr(attr, "unsize")) {
                attrs.repr = repr;
            }
        } else if attr.path().is_ident("doc") {
            attrs.docs.push(attr);
        } else {
            errs.push(Error::new(
                attr.span(),
                format!(
                    "`#[unsize]` currently doesn't support attribute {}",
                    attr.path().to_token_stream()
                ),
            ));
        }
    }

    if let Some(generics) = item.generics() {
        if !generics.to_token_stream().is_empty() {
            errs.push(Error::new(
                generics.span(),
                "`#[unsize]` currently doesn't support generics",
            ));
        }
    }

    // If we have errors before here, emit now - item-level errors mean field errors are likely
    // redundant.
    errs.empty_or(())?;

    let impls = match &item {
        Item::Struct(s) => impls_struct(s, &attrs),
        Item::Enum(e) => impls_enum(e, &attrs),
        _ => Err(Error::new(
            item.span(),
            "unsupported item type for `#[unsize]`, expected struct or enum",
        )
        .into()),
    }?;

    let name = item.ident();
    let vis = item.vis().unwrap();
    let docs = &attrs.docs;

    Ok(quote_spanned!(
        item.span() =>
        // size_of_val on this is correct, but align_of_val is a lie - we just use a high enough alignment
        // that it shouldn't cause issues.
        #( #docs )*
        #[repr(C, align(16))]
        #vis struct #name([std::mem::MaybeUninit<u8>]);

        #impls
    ))
}
