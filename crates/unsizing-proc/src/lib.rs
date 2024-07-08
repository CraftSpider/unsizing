use proc_macro::TokenStream;

mod unsize;
mod unsize_to;
mod util;

#[proc_macro_attribute]
pub fn unsize(extra: TokenStream, input: TokenStream) -> TokenStream {
    assert!(extra.is_empty());
    match unsize::_impl(input.into()) {
        Ok(ts) => ts.into(),
        Err(e) => e.into_compile_errors().into(),
    }
}

// TODO: Maybe make this a derive.
#[proc_macro_attribute]
pub fn unsize_to(extra: TokenStream, input: TokenStream) -> TokenStream {
    match unsize_to::_impl(extra.into(), input.into()) {
        Ok(ts) => ts.into(),
        Err(e) => e.into_compile_errors().into(),
    }
}
