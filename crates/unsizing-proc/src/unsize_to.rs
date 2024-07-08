use crate::util::{ItemExt, MultiError, ReprTy};
use proc_macro2::{Ident, TokenStream};
use quote::quote_spanned;
use syn::parse::Parser;
use syn::spanned::Spanned;
use syn::{
    Error, GenericArgument, GenericParam, Index, Item, ItemEnum, ItemStruct, Member, Path, Type,
    TypePath,
};

fn struct_unsize_impl(
    unsize_to: Path,
    repr_ty: ReprTy,
    s: &ItemStruct,
) -> Result<TokenStream, MultiError> {
    if repr_ty != ReprTy::C {
        return Err(Error::new(
            s.ident.span(),
            "Invalid repr for `#[unsize_to]` on a struct. Expected `C`",
        )
        .into());
    }
    let name = &s.ident;

    let params = &s.generics.params;
    let generic_args = s
        .generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Lifetime(lt) => GenericArgument::Lifetime(lt.lifetime.clone()),
            GenericParam::Type(ty) => GenericArgument::Type(Type::Path(TypePath {
                path: ty.ident.clone().into(),
                qself: None,
            })),
            GenericParam::Const(c) => GenericArgument::Type(Type::Path(TypePath {
                path: c.ident.clone().into(),
                qself: None,
            })),
        })
        .collect::<Vec<_>>();
    let bounds = &s.generics.where_clause;

    let field_access = s
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field)| match &field.ident {
            Some(name) => Member::Named(name.clone()),
            None => Member::Unnamed(Index::from(idx)),
        })
        .collect::<Vec<_>>();
    let field_names = s
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field)| match &field.ident {
            Some(name) => name.clone(),
            None => Ident::new(&format!("field_{}", idx), field.span()),
        })
        .collect::<Vec<_>>();
    let field_tys = s.fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

    Ok(quote_spanned!(s.span() =>
        impl < #params > unsizing::Unsize<#unsize_to> for #name <#( #generic_args, )*>
        #bounds
        {
            fn unsize_meta() -> <#unsize_to as unsizing::Pointee>::Meta {
                (
                #(
                    unsizing::__impl::unsize_check_meta::<#field_tys, _, _>(#unsize_to::#field_names),
                )*
                )
            }
        }
    ))
}

fn enum_unsize_impl(unsize_to: Path, _: ReprTy, e: &ItemEnum) -> Result<TokenStream, MultiError> {
    let name = &e.ident;

    let params = &e.generics.params;
    let generic_args = e
        .generics
        .params
        .iter()
        .map(|param| match param {
            GenericParam::Lifetime(lt) => GenericArgument::Lifetime(lt.lifetime.clone()),
            GenericParam::Type(ty) => GenericArgument::Type(Type::Path(TypePath {
                path: ty.ident.clone().into(),
                qself: None,
            })),
            GenericParam::Const(c) => GenericArgument::Type(Type::Path(TypePath {
                path: c.ident.clone().into(),
                qself: None,
            })),
        })
        .collect::<Vec<_>>();
    let bounds = &e.generics.where_clause;

    // Unlike a struct, which simply unsizes its fields recursively, we instead have to get the
    // metadata of all our field types.
    let (variant_names, variant_metas): (Vec<_>, Vec<_>) = e
        .variants
        .iter()
        .map(|variant| {
            let field_idxs = variant
                .fields
                .iter()
                .enumerate()
                .map(|(idx, _)| Index::from(idx))
                .collect::<Vec<_>>();
            let field_tys = variant.fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

            let var_name = &variant.ident;
            let meta = quote_spanned!(variant.span() => {
                type Phantom = <#unsize_to as unsizing::__impl::EnumHelper>::Phantom;
                #[allow(unused_variables)]
                let phantom = unsafe { ::core::mem::zeroed::<Phantom>() };
                (
                #(
                unsizing::__impl::unsize_check_meta_enum::<#field_tys, _>(phantom.#var_name.#field_idxs),
                )*
                )
            });

            (var_name, meta)
        })
        .unzip();

    let out = quote_spanned!(e.span() =>
        impl < #params > unsizing::Unsize<#unsize_to> for #name <#( #generic_args, )*>
        #bounds
        {
            fn unsize_meta() -> <#unsize_to as unsizing::Pointee>::Meta {
                type Metadata = <#unsize_to as unsizing::Pointee>::Meta;
                Metadata {
                    #(
                    #variant_names: #variant_metas,
                    )*
                }
            }
        }
    );

    println!("Output: {}", out.to_string());

    Ok(out)
}

pub fn _impl(extra: TokenStream, input: TokenStream) -> Result<TokenStream, MultiError> {
    let mut unsize_to = None;
    syn::meta::parser(|meta| {
        if !meta.input.is_empty() {
            Err(Error::new(
                meta.path.span(),
                "Invalid input for `#[unsize_to]`",
            ))
        } else {
            unsize_to = Some(meta.path);
            Ok(())
        }
    })
    .parse2(extra)?;

    let item: Item = syn::parse2(input.clone())?;

    let mut repr_ty = ReprTy::None;
    for attr in item.attrs() {
        if attr.path().is_ident("repr") {
            repr_ty = ReprTy::parse_attr(attr, "unsize_to")?;
        }
    }

    if repr_ty == ReprTy::None {
        return Err(Error::new(item.ident().span(), "Missing repr type on `#[unsize_to]` item. The implicit `repr(Rust)` is UB to manually unsize.").into());
    }

    let unsize_impl = match item {
        Item::Struct(s) => struct_unsize_impl(unsize_to.unwrap(), repr_ty, &s),
        Item::Enum(e) => enum_unsize_impl(unsize_to.unwrap(), repr_ty, &e),
        _ => Err(Error::new(
            item.ident().span(),
            "Invalid item type for `#[unsize_to]`. Expected `struct` or `enum`",
        )
        .into()),
    }?;

    Ok(quote_spanned!(input.span() =>
        #input
        #unsize_impl
    ))
}
