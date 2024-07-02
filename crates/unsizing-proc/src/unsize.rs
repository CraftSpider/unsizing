use crate::util::MultiError;
use proc_macro2::{Ident, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    Attribute, Error, Field, Fields, Index, Item, ItemEnum, ItemStruct, Member, Meta, Token, Type,
    Variant,
};

enum MetaLoc {
    Field(Ident),
    Tuple(Index),
}

impl ToTokens for MetaLoc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let toks = match self {
            MetaLoc::Field(name) => quote_spanned!(name.span() => #name.read()),
            MetaLoc::Tuple(idx) => quote_spanned!(idx.span() => meta.#idx),
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
    metadata.push(meta.unwrap_or_else(|| MetaLoc::Tuple(Index::from(idx))));

    errs.empty_or(())
}

fn pointee_impls_struct(name: &Ident, fields: &Fields) -> Result<TokenStream, MultiError> {
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

    let metadata = field_tys
        .iter()
        .zip(&read_meta)
        .map(|(ty, meta)| match meta {
            MetaLoc::Tuple(_) => quote_spanned!(ty.span() => <#ty as unsizing::Pointee>::Meta),
            MetaLoc::Field(_) => quote_spanned!(ty.span() => ()),
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
            fn #field_names<'a>(self: unsizing::Ref<'a, Self>) -> unsizing::Ref<'a, #field_tys> {
                let ptr = unsafe { <Self as unsizing::Pointee>::layout(unsizing::Ptr::from_ref(self)).fields().#idxs };
                dbg!(ptr);
                unsafe { ptr.as_ref_unchecked() }
            }

            fn #field_names_mut<'a>(self: unsizing::RefMut<'a, Self>) -> unsizing::RefMut<'a, #field_tys> {
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

fn process_struct(s: ItemStruct) -> Result<TokenStream, MultiError> {
    let val_span = s.span();
    let mut errs = MultiError::empty();
    let ItemStruct {
        vis,
        struct_token,
        ident: name,
        fields,
        generics,
        attrs,
        ..
    } = s;

    if !attrs.is_empty() {
        errs.push(Error::new(
            attrs.first().unwrap().span(),
            "`#[unsize]` currently doesn't support other item-level attributes",
        ));
    }
    if !generics.to_token_stream().is_empty() {
        errs.push(Error::new(
            generics.span(),
            "`#[unsize]` currently doesn't support generics",
        ));
    }

    // If we have errors before here, emit now - struct-level errors mean field errors are likely
    // redundant.
    errs.empty_or(())?;

    let pointee_impl = pointee_impls_struct(&name, &fields)?;
    // size_of_val on this is correct, but align_of_val is a lie - we just use a high enough alignment
    // that it shouldn't cause issues.
    // TODO: Provide a way to manually increase this alignment.
    Ok(quote_spanned!(
        val_span.span() =>
        #[repr(C, align(16))]
        #vis #struct_token #name([std::mem::MaybeUninit<u8>]);

        #pointee_impl
    ))
}

fn pointee_impls_enum(name: &Ident, variants: &Punctuated<Variant, Token![,]>) {}

fn process_enum(e: ItemEnum) -> Result<TokenStream, MultiError> {
    Err(Error::new(e.ident.span(), "`#[unsize]` is not yet supported on enums").into())
    /*let val_span = e.span();
    let mut errs = MultiError::empty();
    let ItemEnum {
        vis,
        ident: name,
        variants,
        generics,
        attrs,
        ..
    } = e;

    if !attrs.is_empty() {
        errs.push(Error::new(
            attrs.first().unwrap().span(),
            "`#[unsize]` currently doesn't support other item-level attributes",
        ));
    }
    if !generics.to_token_stream().is_empty() {
        errs.push(Error::new(
            generics.span(),
            "`#[unsize]` currently doesn't support generics",
        ));
    }

    // If we have errors before here, emit now - struct-level errors mean field errors are likely
    // redundant.
    errs.empty_or(())?;

    let pointee_impl = pointee_impls_enum(&name, &variants)?;
    // size_of_val on this is correct, but align_of_val is a lie - we just use a high enough alignment
    // that it shouldn't cause issues.
    // TODO: Provide a way to manually increase this alignment.
    Ok(quote_spanned!(
        val_span.span() =>
        #[repr(C, align(16))]
        #vis struct #name([std::mem::MaybeUninit<u8>]);

        #pointee_impl
    ))
     */
}

pub fn _impl(ts: TokenStream) -> Result<TokenStream, MultiError> {
    let val = syn::parse2::<Item>(ts)?;
    match val {
        Item::Struct(s) => process_struct(s),
        Item::Enum(e) => process_enum(e),
        _ => Err(Error::new(
            val.span(),
            format!("unsupported item type for `#[unsize]`, expected struct or enum"),
        )
        .into()),
    }
}
