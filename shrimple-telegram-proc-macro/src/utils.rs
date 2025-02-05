use proc_macro2::{Ident, Span, TokenStream, TokenTree};

pub(crate) fn expect_punct<const CH: char>(
    tokens: &mut impl Iterator<Item = TokenTree>,
    last_span: impl FnOnce() -> Span,
) -> syn::Result<()> {
    match tokens.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == CH => Ok(()),
        t => Err(syn::Error::new(
            t.map_or_else(last_span, |t| t.span()),
            format_args!("expected `{CH}`"),
        )),
    }
}

pub(crate) fn expect_ident(
    tokens: &mut impl Iterator<Item = TokenTree>,
    last_span: impl FnOnce() -> Span,
) -> syn::Result<Ident> {
    match tokens.next() {
        Some(TokenTree::Ident(i)) => Ok(i),
        t => Err(syn::Error::new(t.map_or_else(last_span, |t| t.span()), "expected identifier")),
    }
}

pub(crate) fn expect_group(
    tokens: &mut impl Iterator<Item = TokenTree>,
    last_span: impl FnOnce() -> Span,
) -> syn::Result<TokenStream> {
    match tokens.next() {
        Some(TokenTree::Group(g)) => Ok(g.stream()),
        t => Err(syn::Error::new(
            t.map_or_else(last_span, |t| t.span()),
            "expected a group of tokens",
        )),
    }
}

pub(crate) fn merge_spans_and_args<Arg>(
    lhs: &mut Option<(Span, Vec<Arg>)>,
    rhs: Option<(Span, Vec<Arg>)>,
) {
    match (lhs, rhs) {
        (_, None) => (),
        (lhs @ None, rhs @ Some(_)) => *lhs = rhs,
        (Some((left_span, left_args)), Some((right_span, mut right_args))) => {
            *left_span = left_span.join(right_span).unwrap_or(Span::call_site());
            left_args.append(&mut right_args);
        }
    }
}
