use {
    proc_macro2::{Span, TokenStream, TokenTree},
    quote_into::{quote_into, ToTokens},
    std::iter,
    syn::{
        parse::{Parse, ParseStream}, parse_macro_input, punctuated::{Pair, Punctuated}, spanned::Spanned, Attribute, Field, GenericParam, Ident, ItemStruct, Lifetime, Token, Type
    },
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

enum RequestAttr {
    ResponseType(TokenStream),
    Optional(Span),
    ViaInto(Span),
}

impl RequestAttr {
    fn from_attr(attr: Attribute) -> syn::Result<impl Iterator<Item = syn::Result<Self>>> {
        let is_our_attr = attr.path.is_ident("shrimple_telegram");

        let mut tokens = attr
            .tokens
            .into_iter()
            .next()
            .and_then(|tree| match tree {
                TokenTree::Group(group) => Some(group.stream()),
                _ => None,
            })
            .filter(|stream| !stream.is_empty())
            .map(TokenStream::into_iter)
            .ok_or_else(|| syn::Error::new(attr.path.span(), "no arguments found"))?;

        Ok(iter::from_fn(move || {
            if !is_our_attr {
                return None;
            }

            Some(Ok(loop {
                break match tokens.next()? {
                    TokenTree::Punct(punct) if punct.as_char() == ',' => continue,
                    TokenTree::Ident(ident) => match () {
                        _ if ident == "optional" => Self::Optional(ident.span()),
                        _ if ident == "via_into" => Self::ViaInto(ident.span()),
                        _ if ident == "response_type" => {
                            if !matches!(tokens.next(), Some(TokenTree::Punct(p)) if p.as_char() == '=')
                            {
                                return Some(Err(syn::Error::new(
                                    ident.span(),
                                    "expected `=` after `response_type`",
                                )));
                            }
                            Self::ResponseType(tokens.by_ref().collect())
                        }
                        _ => return Some(Err(syn::Error::new(ident.span(), "unknown specifier"))),
                    },
                    t => {
                        return Some(Err(syn::Error::new(
                            t.span(),
                            format_args!("unexpected token: {t:?}"),
                        )))
                    }
                };
            }))
        }))
    }
}

struct RequestField {
    name: Ident,
    ty: Type,
    required: bool,
    provided_as_impl_into: bool,
}

impl RequestField {
    fn to_type_tokens(&self, tokens: &mut TokenStream) {
        if self.provided_as_impl_into {
            quote_into!(tokens += impl Into<);
        }
        self.ty.to_tokens(tokens);
        if self.provided_as_impl_into {
            quote_into!(tokens += >);
        }
    }

    /// Including the trailing comma.
    /// This will generate the arg tokens even if the field is optional
    fn to_arg_tokens(&self, tokens: &mut TokenStream) {
        quote_into!(tokens += #(self.name): );
        self.to_type_tokens(tokens);
        quote_into!(tokens += ,);
    }

    fn to_field_init_tokens(&self, tokens: &mut TokenStream) {
        quote_into!(tokens += #(self.name));
        if !self.required {
            quote_into!(tokens += : ::core::default::Default::default());
        } else if self.provided_as_impl_into {
            quote_into!(tokens += : #(self.name).into());
        }
        quote_into!(tokens += ,);
    }

    fn to_setter_tokens(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            fn #(self.name)(mut self, #{self.to_arg_tokens(tokens)}) -> Self {
                self.#(self.name) = #(self.name) #{
                    if self.provided_as_impl_into {
                        quote_into!(tokens += .into());
                    }
                };
                self
            }
        }
    }
}

impl RequestField {
    fn from_field(src: Field) -> syn::Result<Self> {
        let mut required = true;
        let mut provided_as_impl_into = false;
        let ty = src.ty;

        for attr in src.attrs {
            for attr in RequestAttr::from_attr(attr)? {
                match attr? {
                    RequestAttr::ResponseType(stream) => {
                        return Err(syn::Error::new(
                            stream.span(),
                            "`response_type` can only be applied to the whole struct",
                        ))
                    }
                    RequestAttr::Optional(_) => required = false,
                    RequestAttr::ViaInto(_) => provided_as_impl_into = true,
                }
            }
        }

        Ok(Self {
            name: src
                .ident
                .ok_or_else(|| syn::Error::new(ty.span(), "The field must have a name"))?,
            ty,
            required,
            provided_as_impl_into,
        })
    }
}

struct RequestStruct {
    response_type: TokenStream,
    name: Ident,
    lifetimes: Punctuated<Lifetime, Token![,]>,
    fields: Vec<RequestField>,
}

impl Parse for RequestStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let r#struct = input.parse::<ItemStruct>()?;
        let span = r#struct.span();

        let mut response_type = None;
        for attr in r#struct.attrs {
            for attr in RequestAttr::from_attr(attr)? {
                match attr? {
                    RequestAttr::ResponseType(ty) => response_type = Some(ty),
                    RequestAttr::Optional(span) => {
                        return Err(syn::Error::new(
                            span,
                            "`optional` can only be applied to fields",
                        ))
                    }
                    RequestAttr::ViaInto(span) => {
                        return Err(syn::Error::new(
                            span,
                            "`via_into` can only be applied to fields",
                        ))
                    }
                }
            }
        }
        let response_type = response_type.ok_or_else(|| {
            syn::Error::new(span, "No `#[shrimple_telegram(response_type = ...)]` provided")
        })?;

        if let Some(clause) = r#struct.generics.where_clause {
            return Err(syn::Error::new(
                clause.span(),
                "Telegram request may not be constrained by a `where` clause",
            ));
        }

        let mut lifetimes = Punctuated::new();
        for (param, comma) in r#struct.generics.params.into_pairs().map(Pair::into_tuple) {
            match param {
                GenericParam::Lifetime(lit) => {
                    if !lit.bounds.is_empty() {
                        return Err(syn::Error::new(
                            lit.bounds.span(),
                            "Telegram requests may not have bounds on lifetimes",
                        ));
                    }
                    lifetimes.extend([Pair::new(lit.lifetime, comma)]);
                }
                _ => {
                    return Err(syn::Error::new(
                        comma.span(),
                        "Telegram request may only be generic over lifetimes",
                    ))
                }
            }
        }

        let fields = match r#struct.fields {
            syn::Fields::Named(fields_named) => fields_named
                .named
                .into_iter()
                .map(RequestField::from_field)
                .collect::<syn::Result<_>>()?,
            syn::Fields::Unnamed(fields_unnamed) => {
                return Err(syn::Error::new_spanned(
                    fields_unnamed,
                    "Telegram requests must have named fields",
                ))
            }
            syn::Fields::Unit => vec![],
        };

        Ok(Self { name: r#struct.ident, fields, response_type, lifetimes })
    }
}

impl RequestStruct {
    fn make_request_impl(&self, dst: &mut TokenStream) {
        quote_into! { dst +=
            impl<#(self.lifetimes)> crate::Request for #(self.name)<#(self.lifetimes)> {
                const NAME: &'static str = ::core::stringify!(#(self.name));
                type Response = #(self.response_type);
            }
        };
    }

    fn make_constructor(&self, tokens: &mut TokenStream) {
        let struct_name = &self.name;
        let method_name = Ident::new(&to_snake_case(&struct_name.to_string()), struct_name.span());

        quote_into! { tokens +=
            impl crate::Bot {
                pub fn #method_name<#(self.lifetimes)>(
                    &self,
                    #{
                        for field in &self.fields {
                            if field.required && field.name != "bot" {
                                field.to_arg_tokens(tokens);
                            }
                        }
                    }
                ) -> #(self.name)<#(self.lifetimes)> {
                    let bot = self.clone();
                    #struct_name {
                        #{
                            for field in &self.fields {
                                field.to_field_init_tokens(tokens);
                            }
                        }
                    }
                }
            }
        }
    }

    fn make_setters(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> #(self.name)<#(self.lifetimes)> {#{
                for field in &self.fields {
                    field.to_setter_tokens(tokens);
                }
            }}
        }
    }

    fn make_into_future_impl(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> IntoFuture for #(self.name)<#(self.lifetimes)> {
                type IntoFuture = ::std::pin::Pin<Box<dyn Future>>;
                type Output = crate::Result<#(self.response_type)>;

                fn into_future(self) -> Self::IntoFuture {
                    let mut url = self.bot.base().clone();
                    if let Ok(segments) = url.path_segments_mut() {
                        segments.push(stringify!(#(self.name)));
                    }
                    let req = self.bot.client().get(url).json(&self);

                    Box::pin(async move {
                        let (client, req) = req?.build_split();
                        let req = req?;

                        match client.execute(req).await?.json().await? {
                            TelegramResponse { ok: true, result: Some(result), .. } => Ok(result),
                            TelegramResponse { mut description, .. } => Err({
                                description.insert_str(0, ": Telegram API error: ");
                                description.insert_str(0, R::NAME);
                                crate::Error::Response {
                                    method_name: stringify!(#(self.name)),
                                    description: description.into(),
                                }
                            }),
                        }
                    })
                }
            }
        }
    }
}

#[proc_macro_derive(Request, attributes(shrimple_telegram))]
pub fn derive_request(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as RequestStruct);
    let mut res = TokenStream::new();
    input.make_request_impl(&mut res);
    input.make_constructor(&mut res);
    input.make_setters(&mut res);
    res.into()
}
