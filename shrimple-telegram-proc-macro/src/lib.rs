mod request;
mod r#type;
mod utils;

use {
    proc_macro2::{TokenStream, TokenTree},
    quote_into::ToTokens,
    std::ops::BitOrAssign,
    syn::{parse::Parser, spanned::Spanned, Attribute},
};

fn to_snake_case(src: &str) -> String {
    let mut res = String::new();
    for c in src.chars() {
        if c.is_ascii_uppercase() {
            if !res.is_empty() {
                res.push('_');
            }
            res.extend(c.to_lowercase().next());
        } else {
            res.push(c);
        }
    }
    res
}

trait ParseAttrMeta: Sized + BitOrAssign {
    const NAME: &'static str;

    fn parse(tokens: TokenStream) -> syn::Result<Self>;
    fn from_attr(attr: Attribute) -> RequestAttrParseResult<Self> {
        if !attr.path.is_ident(Self::NAME) {
            return RequestAttrParseResult::WrongAttr(attr);
        }

        let tokens = match attr.tokens.into_iter().next() {
            Some(TokenTree::Group(group)) => group.stream(),
            None => TokenStream::new(),
            Some(x) => {
                return RequestAttrParseResult::Error(syn::Error::new(
                    x.span(),
                    "expected args wrapped in parens",
                ))
            }
        };

        if tokens.is_empty() {
            return RequestAttrParseResult::Error(syn::Error::new(
                attr.path.span(),
                "no arguments found",
            ));
        }

        match Self::parse(tokens) {
            Ok(x) => RequestAttrParseResult::Ok(x),
            Err(e) => RequestAttrParseResult::Error(e),
        }
    }

    fn from_attrs(
        attrs: impl IntoIterator<Item = Attribute>,
    ) -> syn::Result<(Vec<Attribute>, Option<Self>)> {
        let mut res = None;
        let mut rest = vec![];

        for attr in attrs {
            match Self::from_attr(attr) {
                RequestAttrParseResult::WrongAttr(a) => rest.push(a),
                RequestAttrParseResult::Error(e) => return Err(e),
                RequestAttrParseResult::Ok(x) => match &mut res {
                    Some(old_x) => *old_x |= x,
                    None => res = Some(x),
                },
            }
        }

        Ok((rest, res))
    }
}

enum RequestAttrParseResult<T> {
    WrongAttr(Attribute),
    Error(syn::Error),
    Ok(T),
}

#[proc_macro_attribute]
pub fn telegram_request(
    attr: proc_macro::TokenStream,
    tokens: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match request::Input::parse(attr.into()).parse(tokens) {
        Ok(input) => input.to_token_stream(),
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn telegram_type(
    attr: proc_macro::TokenStream,
    tokens: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match r#type::Input::parse(attr.into()).parse(tokens) {
        Ok(input) => input.to_token_stream(),
        Err(e) => e.to_compile_error(),
    }
    .into()
}
