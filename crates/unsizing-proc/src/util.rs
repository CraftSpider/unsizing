use proc_macro2::{Ident, TokenStream};
use syn::spanned::Spanned;
use syn::{Attribute, Error, Generics, Item, Visibility};

pub trait ItemExt {
    fn attrs(&self) -> &[Attribute];
    fn vis(&self) -> Option<&Visibility>;
    fn ident(&self) -> &Ident;
    fn generics(&self) -> Option<&Generics>;
}

impl ItemExt for Item {
    fn attrs(&self) -> &[Attribute] {
        match self {
            Item::Const(c) => &c.attrs,
            Item::Enum(e) => &e.attrs,
            Item::ExternCrate(ec) => &ec.attrs,
            Item::Fn(f) => &f.attrs,
            Item::ForeignMod(fm) => &fm.attrs,
            Item::Impl(i) => &i.attrs,
            Item::Macro(m) => &m.attrs,
            Item::Mod(m) => &m.attrs,
            Item::Static(s) => &s.attrs,
            Item::Struct(s) => &s.attrs,
            Item::Trait(t) => &t.attrs,
            Item::TraitAlias(ta) => &ta.attrs,
            Item::Type(t) => &t.attrs,
            Item::Union(u) => &u.attrs,
            Item::Use(u) => &u.attrs,
            _ => unimplemented!(),
        }
    }

    fn vis(&self) -> Option<&Visibility> {
        Some(match self {
            Item::Const(c) => &c.vis,
            Item::Enum(e) => &e.vis,
            Item::ExternCrate(ec) => &ec.vis,
            Item::Fn(f) => &f.vis,
            Item::Mod(m) => &m.vis,
            Item::Static(s) => &s.vis,
            Item::Struct(s) => &s.vis,
            Item::Trait(t) => &t.vis,
            Item::TraitAlias(ta) => &ta.vis,
            Item::Type(t) => &t.vis,
            Item::Union(u) => &u.vis,
            Item::Use(u) => &u.vis,
            _ => return None,
        })
    }

    fn ident(&self) -> &Ident {
        match self {
            Item::Struct(s) => &s.ident,
            Item::Enum(e) => &e.ident,
            Item::Union(u) => &u.ident,
            Item::Type(t) => &t.ident,
            Item::Trait(t) => &t.ident,
            _ => unimplemented!(),
        }
    }

    fn generics(&self) -> Option<&Generics> {
        Some(match self {
            Item::Const(c) => &c.generics,
            Item::Enum(e) => &e.generics,
            Item::Fn(f) => &f.sig.generics,
            Item::Impl(i) => &i.generics,
            Item::Struct(s) => &s.generics,
            Item::Trait(t) => &t.generics,
            Item::TraitAlias(ta) => &ta.generics,
            Item::Type(t) => &t.generics,
            Item::Union(u) => &u.generics,
            _ => return None,
        })
    }
}

#[derive(Copy, Clone, Default, PartialEq)]
pub enum ReprTy {
    C,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    USize,
    ISize,
    #[default]
    None,
}

impl ReprTy {
    pub fn parse_attr(attr: &Attribute, name: &str) -> Result<ReprTy, Error> {
        let mut repr_ty = ReprTy::None;
        attr.parse_nested_meta(|meta| {
            if let Some(ident) = meta.path.get_ident() {
                match &*ident.to_string() {
                    "C" => repr_ty = ReprTy::C,
                    "u8" => repr_ty = ReprTy::U8,
                    "i8" => repr_ty = ReprTy::I8,
                    "u16" => repr_ty = ReprTy::U16,
                    "i16" => repr_ty = ReprTy::I16,
                    "u32" => repr_ty = ReprTy::U32,
                    "i32" => repr_ty = ReprTy::I32,
                    "u64" => repr_ty = ReprTy::U64,
                    "i64" => repr_ty = ReprTy::I64,
                    "usize" => repr_ty = ReprTy::USize,
                    "isize" => repr_ty = ReprTy::ISize,
                    _ => {
                        return Err(Error::new(
                            meta.path.span(),
                            format!("Invalid repr attribute for `#[{name}]`"),
                        ))
                    }
                }
                Ok(())
            } else {
                Err(Error::new(
                    meta.path.span(),
                    format!("Invalid repr attribute for `#[{name}]`"),
                ))
            }
        })?;
        Ok(repr_ty)
    }
}

pub struct MultiError {
    errors: Vec<Error>,
}

impl MultiError {
    pub fn empty() -> MultiError {
        MultiError { errors: Vec::new() }
    }

    pub fn push(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn extend(&mut self, other: MultiError) {
        self.errors.extend(other.errors);
    }

    pub fn and<T, E: Into<MultiError>>(&mut self, res: Result<T, E>) -> Option<T> {
        match res {
            Ok(val) => Some(val),
            Err(e) => {
                self.extend(e.into());
                None
            }
        }
    }

    pub fn empty_or<T>(self, val: T) -> Result<T, MultiError> {
        if self.errors.is_empty() {
            Ok(val)
        } else {
            Err(self)
        }
    }

    pub fn empty_or_else<T>(self, f: impl FnOnce() -> T) -> Result<T, MultiError> {
        if self.errors.is_empty() {
            Ok(f())
        } else {
            Err(self)
        }
    }

    pub fn into_compile_errors(self) -> TokenStream {
        let mut ts = TokenStream::new();
        for err in self.errors {
            ts.extend(err.into_compile_error());
        }
        ts
    }
}

impl From<Error> for MultiError {
    fn from(value: Error) -> Self {
        MultiError {
            errors: vec![value],
        }
    }
}
