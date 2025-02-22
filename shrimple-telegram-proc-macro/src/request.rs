use {
    crate::{to_snake_case, ParseAttrMeta},
    proc_macro2::{Span, TokenStream, TokenTree},
    quote_into::{quote_into, ToTokens},
    std::ops::BitOrAssign,
    syn::{
        parse::{ParseStream, Parser},
        parse_quote_spanned,
        punctuated::{Pair, Punctuated},
        spanned::Spanned,
        Attribute, Field, GenericParam, Ident, ItemStruct, Lifetime, Token, Type, Visibility,
    },
};

struct InputSpecs {
    response_type: TokenStream,
}

impl BitOrAssign for InputSpecs {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = rhs;
    }
}

impl ParseAttrMeta for InputSpecs {
    const NAME: &'static str = "telegram_request";

    fn parse(tokens: TokenStream) -> syn::Result<Self> {
        let all = tokens.span();
        let mut tokens = tokens.into_iter();

        let response_type = loop {
            match tokens
                .next()
                .ok_or_else(|| syn::Error::new(all, "no `response_type` provided"))?
            {
                TokenTree::Punct(punct) if punct.as_char() == ',' => continue,
                TokenTree::Ident(ident) => {
                    if ident == "response_type" {
                        if !matches!(tokens.next(), Some(TokenTree::Punct(p)) if p.as_char() == '=')
                        {
                            return Err(syn::Error::new(
                                ident.span(),
                                "expected `=` after `response_type`",
                            ));
                        }
                        break tokens.by_ref().collect();
                    } else {
                        return Err(syn::Error::new(ident.span(), "unknown specifier"));
                    }
                }
                t => return Err(syn::Error::new(t.span(), "unexpected token")),
            }
        };

        Ok(Self { response_type })
    }
}

#[derive(Default)]
struct FieldSpecs {
    optional: Option<Span>,
    via_into: Option<Span>,
    payload: Option<Span>,
}

impl BitOrAssign for FieldSpecs {
    fn bitor_assign(&mut self, rhs: Self) {
        self.optional = self.optional.or(rhs.optional);
        self.via_into = self.via_into.or(rhs.via_into);
        self.payload = self.payload.or(rhs.payload);
    }
}

impl ParseAttrMeta for FieldSpecs {
    const NAME: &'static str = "telegram_request";

    fn parse(tokens: TokenStream) -> syn::Result<Self> {
        let mut res = Self::default();

        for token in tokens {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == ',' => continue,
                TokenTree::Ident(ident) => {
                    if ident == "optional" {
                        res.optional = Some(ident.span());
                    } else if ident == "via_into" {
                        res.via_into = Some(ident.span());
                    } else if ident == "payload" {
                        res.payload = Some(ident.span());
                    } else {
                        return Err(syn::Error::new(ident.span(), "unknown specifier"));
                    }
                }
                t => return Err(syn::Error::new(t.span(), "unexpected token")),
            }
        }

        Ok(res)
    }
}

struct InputField {
    attrs: Vec<Attribute>,
    name: Ident,
    ty: Type,
    specs: FieldSpecs,
}

impl InputField {
    fn from_field(src: Field) -> syn::Result<Self> {
        if !matches!(src.vis, Visibility::Public(_)) {
            return Err(syn::Error::new(
                src.vis.span(),
                "Telegram requests' fields must be public",
            ));
        }

        let span = src.span();
        let (mut attrs, specs) = FieldSpecs::from_attrs(src.attrs)?;
        let specs = specs.unwrap_or(FieldSpecs::default());

        if let Some(span) = specs.optional {
            attrs.push(parse_quote_spanned! {
                span => #[serde(skip_serializing_if = "IsDefault::is_default")]
            });
        }

        Ok(Self {
            attrs,
            specs,
            ty: src.ty,
            name: src.ident.ok_or_else(|| syn::Error::new(span, "The field must have a name"))?,
        })
    }

    fn to_type_tokens(&self, tokens: &mut TokenStream) {
        if self.specs.via_into.is_some() {
            quote_into!(tokens += impl Into<);
        }
        self.ty.to_tokens(tokens);
        if self.specs.via_into.is_some() {
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
        if self.specs.optional.is_some() {
            quote_into!(tokens += : ::core::default::Default::default());
        } else if self.specs.via_into.is_some() {
            quote_into!(tokens += : #(self.name).into());
        }
        quote_into!(tokens += ,);
    }

    fn to_setter_tokens(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            pub fn #(self.name)(mut self, #{self.to_arg_tokens(tokens)}) -> Self {
                self.#(self.name) = #(self.name) #{
                    if self.specs.via_into.is_some() {
                        quote_into!(tokens += .into());
                    }
                };
                self
            }
        }
    }
}

pub struct Input {
    attrs: Vec<Attribute>,
    specs: InputSpecs,
    name: Ident,
    lifetimes: Punctuated<Lifetime, Token![,]>,
    fields: Vec<InputField>,
}

impl Input {
    pub fn parse(attr: TokenStream) -> impl Parser<Output = Self> {
        move |input: ParseStream| {
            let attrs = Attribute::parse_outer(input)?;
            let vis = input.parse::<Visibility>()?;
            if !matches!(vis, Visibility::Public(_)) {
                return Err(syn::Error::new(
                    vis.span(),
                    "Telegram requests must be part of the public API",
                ));
            }

            let r#struct = input.parse::<ItemStruct>()?;
            let specs = InputSpecs::parse(attr)?;

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
                    .map(InputField::from_field)
                    .collect::<syn::Result<Vec<_>>>()?,
                syn::Fields::Unnamed(fields_unnamed) => {
                    return Err(syn::Error::new_spanned(
                        fields_unnamed,
                        "Telegram requests must have named fields",
                    ))
                }
                syn::Fields::Unit => vec![],
            };

            if let Some(extra) = fields.iter().filter_map(|f| f.specs.payload).nth(1) {
                return Err(syn::Error::new(extra, "Only 0 or 1 payload fields may be defined"));
            }

            let mut first_optional_id = None;
            if !fields.iter().enumerate().is_sorted_by_key(|(i, f)| {
                if f.specs.optional.is_some() && first_optional_id.is_none() {
                    first_optional_id = Some(i);
                }
                f.specs.optional.is_some()
            }) {
                return Err(syn::Error::new(
                    fields[first_optional_id.unwrap()].name.span(),
                    "optional fields must come after all required ones",
                ));
            }

            Ok(Self { attrs, name: r#struct.ident, fields, specs, lifetimes })
        }
    }

    fn make_struct(&self, tokens: &mut TokenStream) {
        for attr in &self.attrs {
            quote_into!(tokens += #attr);
        }

        let mut doc_name = self.name.to_string();
        doc_name.make_ascii_lowercase();
        quote_into! { tokens +=
            #[derive(Clone, ::serde::Serialize)]
            #[doc = #(format!("[Official docs](https://core.telegram.org/bots/api#{doc_name})"))]
            pub struct #(self.name)<#(self.lifetimes)> {
                #[serde(skip_serializing)]
                bot: crate::Bot,
                #{
                    for field in &self.fields {
                        for attr in &field.attrs {
                            quote_into!(tokens += #attr);
                        }
                        quote_into!(tokens += pub #(field.name): #(field.ty),);
                    }
                }
            }
        }
    }

    fn make_simple_request_impl(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> crate::Request for #(self.name)<#(self.lifetimes)> {
                const NAME: &'static str = ::core::stringify!(#(self.name));
                type Response = #(self.specs.response_type);

                fn to_future(&self) -> impl ::std::future::Future<Output = crate::Result<Self::Response>> {
                    let json = ::serde_json::to_string(self)
                        .expect(#(format!("serilaized {} into JSON", self.name)));
                    self.bot.request(#(self.name.to_string()), json)
                }
            }
        };
    }

    fn make_multipart_request_impl(&self, tokens: &mut TokenStream, payload_field: &InputField) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> crate::Request for #(self.name)<#(self.lifetimes)> {
                const NAME: &'static str = ::core::stringify!(#(self.name));
                type Response = #(self.specs.response_type);

                fn to_future(&self) -> impl ::std::future::Future<Output = crate::Result<Self::Response>> {
                    if let Some(payload) = crate::Multipart::get_payload(&self.#(payload_field.name)) {
                        let form = crate::serialise_into_form(self, payload);
                        self.bot.multipart_request(#(self.name.to_string()), form)
                    } else {
                        let json = ::serde_json::to_string(self)
                            .expect(#(format!("serilaized {} into JSON", self.name)));
                        self.bot.request(#(self.name.to_string()), json)
                    }
                }
            }
        };
    }

    fn make_simple_into_future_impl(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> ::std::future::IntoFuture for #(self.name)<#(self.lifetimes)> {
                type IntoFuture = ::std::pin::Pin<Box<dyn ::std::future::Future<Output = Self::Output> + Send + Sync + 'static>>;
                type Output = crate::Result<#(self.specs.response_type)>;

                fn into_future(self) -> Self::IntoFuture {
                    let json = ::serde_json::to_string(&self)
                        .expect(#(format!("serilaized {} into JSON", self.name)));
                    self.bot.request(#(self.name.to_string()), json)
                }
            }
        }
    }

    fn make_multipart_into_future_impl(
        &self,
        tokens: &mut TokenStream,
        payload_field: &InputField,
    ) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> ::std::future::IntoFuture for #(self.name)<#(self.lifetimes)> {
                type IntoFuture = ::std::pin::Pin<Box<dyn ::std::future::Future<Output = Self::Output> + Send + Sync + 'static>>;
                type Output = crate::Result<#(self.specs.response_type)>;

                fn into_future(mut self) -> Self::IntoFuture {
                    if let Some(payload) = crate::Multipart::get_payload_mut(&mut self.#(payload_field.name)) {
                        let form = crate::serialise_into_form(&self, payload);
                        self.bot.multipart_request(#(self.name.to_string()), form)
                    } else {
                        let json = ::serde_json::to_string(&self)
                            .expect(#(format!("serilaized {} into JSON", self.name)));
                        self.bot.request(#(self.name.to_string()), json)
                    }
                }
            }
        }
    }

    fn make_debug_impl(&self, tokens: &mut TokenStream) {
        quote_into! { tokens +=
            impl<#(self.lifetimes)> ::std::fmt::Debug for #(self.name)<#(self.lifetimes)> {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    f.debug_struct(stringify!(#(self.name)))
                    #{
                        for field in &self.fields {
                            quote_into! { tokens +=
                                .field(stringify!(#(field.name)), &self.#(field.name))
                            }
                        }
                    }
                        .finish_non_exhaustive()
                }
            }
        }
    }

    fn make_constructor(&self, tokens: &mut TokenStream) {
        let method_name = Ident::new(&to_snake_case(&self.name.to_string()), self.name.span());
        let mut doc_name = self.name.to_string();
        doc_name.make_ascii_lowercase();

        quote_into! { tokens +=
            impl crate::Bot {
                #[doc = #(format!("[Official docs](https://core.telegram.org/bots/api#{doc_name})"))]
                pub fn #method_name<#(self.lifetimes)>(
                    &self,
                    #{
                        for field in &self.fields {
                            if field.specs.optional.is_none() {
                                field.to_arg_tokens(tokens);
                            }
                        }
                    }
                ) -> #(self.name)<#(self.lifetimes)> {
                    #(self.name) {
                        bot: self.clone(),
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
}

impl ToTokens for Input {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.make_struct(tokens);
        if let Some(payload_field) = self.fields.iter().find(|f| f.specs.payload.is_some()) {
            self.make_multipart_request_impl(tokens, payload_field);
            self.make_multipart_into_future_impl(tokens, payload_field);
        } else {
            self.make_simple_request_impl(tokens);
            self.make_simple_into_future_impl(tokens);
        }
        self.make_debug_impl(tokens);
        self.make_constructor(tokens);
        self.make_setters(tokens);
    }
}
