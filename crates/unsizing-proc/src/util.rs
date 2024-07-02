use proc_macro2::TokenStream;
use syn::Error;

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

    pub fn and<T>(&mut self, res: Result<T, MultiError>) -> Option<T> {
        match res {
            Ok(val) => Some(val),
            Err(e) => {
                self.extend(e);
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
