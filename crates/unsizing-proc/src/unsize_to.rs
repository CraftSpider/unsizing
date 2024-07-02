use crate::util::MultiError;
use proc_macro2::{Ident, TokenStream};
use quote::quote_spanned;
use syn::parse::Parser;
use syn::spanned::Spanned;
use syn::{Error, GenericArgument, GenericParam, Index, ItemStruct, Member, Path, Type, TypePath};

fn unsize_impl(unsize_to: Path, s: &ItemStruct) -> Result<TokenStream, MultiError> {
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

    Ok(quote_spanned!(s.span() =>
        impl < #params > unsizing::Unsize<#unsize_to> for #name <#( #generic_args, )*>
        #bounds
        {
            fn unsize(&self) -> unsizing::Ref<'_, #unsize_to> {
                #(
                let #field_names = unsizing::__impl::unsize_check(unsizing::Unsize::unsize(&self.#field_access), #unsize_to::#field_names);
                )*

                let out = unsafe {
                    unsizing::Ptr::from_raw_parts(unsizing::Ptr::from(self).cast(), ( #( #field_names, )* ))
                        .as_ref_unchecked()
                };

                out
            }
        }
    ))
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

    let s: ItemStruct = syn::parse2(input.clone())?;

    let mut repr_found = false;
    for attr in &s.attrs {
        if attr.path().is_ident("repr") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("C") {
                    repr_found = true;
                    Ok(())
                } else {
                    Err(Error::new(
                        meta.path.span(),
                        "Invalid repr attribute for `#[unsize_to]`",
                    ))
                }
            })?;
        }
    }

    let unsize_impl = unsize_impl(unsize_to.unwrap(), &s)?;

    Ok(quote_spanned!(input.span() =>
        #input
        #unsize_impl
    ))
}
