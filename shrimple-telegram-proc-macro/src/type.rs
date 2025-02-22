use {
    crate::{
        utils::{expect_group, expect_punct, merge_spans_and_args},
        ParseAttrMeta,
    },
    proc_macro2::{Delimiter, Group, Literal, Span, TokenStream, TokenTree},
    quote_into::{quote_into, ToTokens},
    std::{
        collections::HashMap,
        fmt::Write,
        iter::{repeat, zip},
        mem::take,
        ops::BitOrAssign,
    },
    syn::{
        parse::{ParseStream, Parser},
        punctuated::Punctuated,
        spanned::Spanned,
        Attribute, Field, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, ItemEnum,
        ItemStruct, Token, Type, Variant, Visibility,
    },
};

#[derive(Default)]
struct InputSpecs {
    copy: Option<Span>,
    untagged: Option<Span>,
    partial_eq: Option<Span>,
    common_fields: Option<(Span, Vec<CommonField>)>,
    phantom_fields: Option<(Span, Vec<(Ident, Type)>)>,
    name_all: Option<(Span, Vec<Ident>)>,
}

impl BitOrAssign for InputSpecs {
    fn bitor_assign(&mut self, rhs: Self) {
        self.copy = self.copy.or(rhs.copy);
        self.untagged = self.untagged.or(rhs.untagged);
        self.partial_eq = self.partial_eq.or(rhs.partial_eq);
        merge_spans_and_args(&mut self.common_fields, rhs.common_fields);
        merge_spans_and_args(&mut self.phantom_fields, rhs.phantom_fields);
        merge_spans_and_args(&mut self.name_all, rhs.name_all);
    }
}

impl ParseAttrMeta for InputSpecs {
    const NAME: &'static str = "telegram_type";

    fn parse(tokens: TokenStream) -> syn::Result<Self> {
        let mut tokens = tokens.into_iter();
        let mut res = Self::default();

        while let Some(token) = tokens.next() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == ',' => (),
                TokenTree::Ident(ident) => {
                    if ident == "copy" {
                        res.copy = Some(ident.span());
                    } else if ident == "untagged" {
                        res.untagged = Some(ident.span());
                    } else if ident == "partial_eq" {
                        res.partial_eq = Some(ident.span());
                    } else if ident == "common_fields" {
                        let group = expect_group(&mut tokens, || ident.span())?;

                        let fields_parser = |input: ParseStream| {
                            Punctuated::<_, Token![,]>::parse_terminated_with(
                                input,
                                Field::parse_named,
                            )
                        };

                        res.common_fields = Some((
                            ident.span(),
                            fields_parser
                                .parse2(group)?
                                .into_iter()
                                .filter_map(|Field { mut attrs, ident, ty, .. }| {
                                    let mut optional = false;
                                    attrs.retain(|attr| {
                                        optional = attr.path.is_ident("optional");
                                        !optional
                                    });
                                    Some(CommonField { name: ident?, ty, optional, paths: vec![] })
                                })
                                .collect(),
                        ));
                    } else if ident == "phantom_fields" {
                        let group = expect_group(&mut tokens, || ident.span())?;

                        let fields_parser = |input: ParseStream| {
                            Punctuated::<_, Token![,]>::parse_terminated_with(
                                input,
                                Field::parse_named,
                            )
                        };

                        res.phantom_fields = Some((
                            ident.span(),
                            fields_parser
                                .parse2(group)?
                                .into_iter()
                                .filter_map(|Field { ident, ty, .. }| Some((ident?, ty)))
                                .collect(),
                        ));
                    } else if ident == "name_all" {
                        let mut group = expect_group(&mut tokens, || ident.span())?.into_iter();
                        let mut idents = vec![];

                        while let Some(token) = group.next() {
                            match token {
                                TokenTree::Ident(i) => {
                                    if !idents.is_empty() {
                                        expect_punct::<','>(&mut group, || i.span())?;
                                    }
                                    idents.push(i);
                                }
                                t => {
                                    return Err(syn::Error::new(t.span(), "expected an identifier"))
                                }
                            }
                        }

                        if let Some(extra) = group.next() {
                            return Err(syn::Error::new(extra.span(), "extra tokens"));
                        }

                        res.name_all = Some((ident.span(), idents));
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

#[derive(Default)]
struct VariantSpecs {
    nested_fields: Vec<(Ident, TokenStream)>,
    phantom_fields: Option<(Span, Vec<(Ident, Type)>)>,
    name_all: Option<(Span, Vec<Ident>)>,
}

impl BitOrAssign for VariantSpecs {
    fn bitor_assign(&mut self, mut rhs: Self) {
        self.nested_fields.append(&mut rhs.nested_fields);
        merge_spans_and_args(&mut self.phantom_fields, rhs.phantom_fields);
        merge_spans_and_args(&mut self.name_all, rhs.name_all);
    }
}

impl ParseAttrMeta for VariantSpecs {
    const NAME: &'static str = "telegram_type";

    fn parse(tokens: TokenStream) -> syn::Result<Self> {
        fn parse_nested_field(
            mut it: impl Iterator<Item = TokenTree>,
        ) -> syn::Result<Option<(Ident, TokenStream)>> {
            let name = match it.next() {
                Some(TokenTree::Ident(ident)) => ident,
                None => return Ok(None),
                Some(t) => return Err(syn::Error::new(t.span(), "expected an identifier")),
            };

            match it.next() {
                Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                t => return Err(syn::Error::new(t.span(), "expected `=`")),
            }

            let path = it
                .take_while(|t| !matches!(t, TokenTree::Punct(p) if p.as_char() == ','))
                .collect();
            Ok(Some((name, path)))
        }

        let mut res = Self::default();
        let mut tokens = tokens.into_iter();

        while let Some(token) = tokens.next() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == ',' => continue,
                TokenTree::Ident(ident) => {
                    if ident == "nested_fields" {
                        let tokens = match tokens.next() {
                            Some(TokenTree::Group(group)) => group.stream(),
                            None => TokenStream::new(),
                            Some(x) => return Err(syn::Error::new(
                                x.span(),
                                "expected nested fields wrapped in parens, e.g. `(x = field.x)`",
                            )),
                        };

                        if tokens.is_empty() {
                            return Err(syn::Error::new(ident.span(), "no nested fields found"));
                        }
                        let mut tokens = tokens.into_iter();

                        while let Some(field) = parse_nested_field(&mut tokens)? {
                            res.nested_fields.push(field);
                        }
                    } else if ident == "phantom_fields" {
                        let group = expect_group(&mut tokens, || ident.span())?;

                        let fields_parser = |input: ParseStream| {
                            Punctuated::<_, Token![,]>::parse_terminated_with(
                                input,
                                Field::parse_named,
                            )
                        };

                        res.phantom_fields = Some((
                            ident.span(),
                            fields_parser
                                .parse2(group)?
                                .into_iter()
                                .filter_map(|Field { ident, ty, .. }| Some((ident?, ty)))
                                .collect(),
                        ));
                    } else if ident == "name_all" {
                        let mut group = expect_group(&mut tokens, || ident.span())?.into_iter();
                        let mut new_names = vec![];

                        while let Some(token) = group.next() {
                            match token {
                                TokenTree::Ident(i) => {
                                    if !new_names.is_empty() {
                                        expect_punct::<','>(&mut group, || i.span())?;
                                    }
                                    new_names.push(i);
                                }
                                t => {
                                    return Err(syn::Error::new(t.span(), "expected an identifier"))
                                }
                            }
                        }

                        if let Some(extra) = group.next() {
                            return Err(syn::Error::new(extra.span(), "extra tokens"));
                        }

                        res.name_all = Some((ident.span(), new_names));
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

fn apply_mods_to_fields(
    fields: &Fields,
    name_all_spec_span: Span,
    new_names: Vec<Ident>,
    phantom_fields_spec_span: Span,
    phantom_fields: Vec<(Ident, Type)>,
) -> syn::Result<Vec<Field>> {
    let mut res: Vec<Field> = fields.iter().cloned().collect();
    if res.len() != new_names.len() && !new_names.is_empty() {
        return Err(syn::Error::new(
            name_all_spec_span,
            "number of field names doesn't match the number of fields",
        ));
    }

    let mut fields_named = !matches!(fields, Fields::Unnamed(_));
    for (field, new_name) in zip(&mut res, new_names) {
        field.ident = Some(new_name);
        fields_named = true;
    }

    if !fields_named && !phantom_fields.is_empty() {
        return Err(syn::Error::new(
            phantom_fields_spec_span,
            "cannot add phantom fields to a set of unnamed fields",
        ));
    }

    res.extend(phantom_fields.into_iter().map(|(name, ty)| Field {
        attrs: vec![],
        vis: Visibility::Inherited,
        colon_token: Some(Token![:](name.span())),
        ident: Some(name),
        ty,
    }));

    Ok(res)
}

pub struct Input {
    attrs: Vec<Attribute>,
    name: Ident,
    generics: Generics,
    copy: Option<Span>,
    untagged: Option<Span>,
    partial_eq: Option<Span>,
    kind: InputKind,
    repr: Option<Repr>,
}

struct Repr {
    name: Ident,
    kind: ReprKind,
}

impl Repr {
    fn new(name: &Ident, kind: ReprKind) -> Self {
        Self { name: Ident::new(&format!("{name}Repr"), name.span()), kind }
    }

    fn to_tokens(
        &self,
        tokens: &mut TokenStream,
        original_name: &Ident,
        generics: &Generics,
        untagged: Option<Span>,
        attrs: &[Attribute],
        original: &InputKind,
    ) {
        quote_into!(tokens += #[derive(Deserialize, Serialize)] );
        if untagged.is_some() {
            quote_into!(tokens += #[serde(untagged)])
        } else {
            quote_into!(tokens += #[serde(rename_all = "snake_case")]);
        }

        for attr in attrs {
            attr.to_tokens(tokens);
        }

        match (&self.kind, original) {
            (ReprKind::Enum { variants }, InputKind::Enum { variants: original_variants, .. }) => {
                quote_into! { tokens +=
                    enum #(self.name) #generics {#{
                        for v in variants {
                            quote_into!(tokens += #v,);
                        }
                    }}

                    #[automatically_derived]
                    impl #generics From<#(self.name) #generics> for #original_name #generics {
                        fn from(value: #(self.name) #generics) -> Self {
                            match value {#{
                                for (v, original_v) in zip(variants, original_variants) {
                                    make_variant_pattern(&self.name, &v.ident, &v.fields, tokens);
                                    quote_into!(tokens += => Self::#(original_v.ident));
                                    make_field_adapter_expr(&v.fields, &original_v.fields, tokens);
                                    quote_into!(tokens += ,);
                                }
                            }}
                        }
                    }

                    #[automatically_derived]
                    impl #generics From<#original_name #generics> for #(self.name) #generics {
                        fn from(value: #original_name #generics) -> Self {
                            match value {#{
                                for (original_v, v) in zip(original_variants, variants) {
                                    make_variant_pattern(original_name, &original_v.ident, &original_v.fields, tokens);
                                    quote_into!(tokens += => Self::#(v.ident));
                                    make_field_adapter_expr(&original_v.fields, &v.fields, tokens);
                                    quote_into!(tokens += ,);
                                }
                            }}
                        }
                    }
                }
            }

            (ReprKind::Struct { fields }, InputKind::Struct { fields: original_fields }) => {
                quote_into! { tokens +=
                    struct #(self.name) #generics #fields

                    #[automatically_derived]
                    impl #generics From<#(self.name) #generics> for #original_name #generics {
                        fn from(#{make_struct_pattern(&self.name, fields, tokens)}: #(self.name) #generics)
                            -> Self
                        {
                            Self #{make_field_adapter_expr(fields, original_fields, tokens)}
                        }
                    }

                    #[automatically_derived]
                    impl #generics From<#original_name #generics> for #(self.name) #generics {
                        fn from(#{make_struct_pattern(original_name, original_fields, tokens)}: #original_name #generics)
                            -> Self
                        {
                            Self #{make_field_adapter_expr(original_fields, fields, tokens)}
                        }
                    }
                }
            }

            _ => unreachable!("repr kind and original input kind don't match"),
        }
    }
}

enum ReprKind {
    Enum { variants: Vec<Variant> },
    Struct { fields: Fields },
}

#[derive(Default)]
struct EnumReprBuilder {
    variants: Vec<Option<Variant>>,
}

impl EnumReprBuilder {
    fn add_variant(
        &mut self,
        v_id: usize,
        v: &Variant,
        name_all_spec_span: Span,
        new_names: Vec<Ident>,
        phantom_fields_spec_span: Span,
        phantom_fields: Vec<(Ident, Type)>,
    ) -> syn::Result<()> {
        if new_names.is_empty() && phantom_fields.is_empty() {
            return Ok(());
        }

        let v = Variant {
            attrs: v.attrs.clone(),
            ident: v.ident.clone(),
            fields: FieldsNamed {
                brace_token: syn::token::Brace { span: v.ident.span() },
                named: apply_mods_to_fields(
                    &v.fields,
                    name_all_spec_span,
                    new_names,
                    phantom_fields_spec_span,
                    phantom_fields,
                )?
                .into_iter()
                .collect(),
            }
            .into(),
            discriminant: v.discriminant.clone(),
        };

        let n_variants = self.variants.len();
        match self.variants.get_mut(v_id) {
            Some(slot) => *slot = Some(v),
            None => self.variants.extend(repeat(None).take(v_id - n_variants).chain([Some(v)])),
        }

        Ok(())
    }

    fn finish<'v>(
        mut self,
        name: &Ident,
        all_variants: impl IntoIterator<Item = &'v Variant>,
    ) -> Option<Repr> {
        if self.variants.is_empty() {
            return None;
        }

        let mut all_variants = all_variants.into_iter();
        for (src_v, own_v) in
            zip(all_variants.by_ref().take(self.variants.len()), &mut self.variants)
        {
            if own_v.is_none() {
                *own_v = Some(src_v.clone());
            }
        }

        let variants = self
            .variants
            .into_iter()
            .map(|v| v.expect("number of repr variants & source variants matches"))
            .chain(all_variants.cloned())
            .collect();
        Some(Repr::new(name, ReprKind::Enum { variants }))
    }
}

enum InputKind {
    /// the field paths in `common_fields` refer to unnamed fields as `field0`, `field1` etc.
    Enum {
        variants: Vec<Variant>,
        common_fields: Vec<CommonField>,
    },
    Struct {
        fields: Fields,
    },
}

struct CommonField {
    optional: bool,
    name: Ident,
    ty: Type,
    paths: Vec<TokenStream>,
}

impl CommonField {
    fn make_getter_return_type(&self, dst: &mut TokenStream) {
        if self.optional {
            quote_into!(dst += Option<);
        }
        quote_into!(dst += &#(self.ty));
        if self.optional {
            quote_into!(dst += >);
        }
    }
}

/// Unnamed fields are referred to as `field0`, `field1`, etc.
fn make_struct_pattern(name: &Ident, fields: &Fields, tokens: &mut TokenStream) {
    if *fields == Fields::Unit {
        name.to_tokens(tokens);
        return;
    }

    let mut unnamed_name = String::with_capacity(7);
    _ = unnamed_name.write_str("field");

    quote_into! { tokens +=
        #name {#{
            for (i, field) in fields.iter().enumerate() {
                match &field.ident {
                    Some(name) => quote_into!(tokens += #name,),
                    None => {
                        _ = write!(unnamed_name, "{i}");
                        quote_into!(tokens += #(Literal::usize_unsuffixed(i)): #(Ident::new(&unnamed_name, field.span())),);
                        unnamed_name.truncate(5);
                    }
                }
            }
        }}
    }
}

fn make_variant_pattern(
    enum_name: &Ident,
    name: &Ident,
    fields: &Fields,
    tokens: &mut TokenStream,
) {
    quote_into!(tokens += #enum_name::);
    make_struct_pattern(name, fields, tokens);
}

/// Makes an expression that creates an enum variant / struct from given fields, mapping them
/// according to their name/index
///
/// If `src` has extra fields, they're ignored.
///
/// If `dst` has extra fields, they're filled in with `Default::default()`
///
/// If both field sets are named, they're supposed to have the same ordering.
fn make_field_adapter_expr(src: &Fields, dst: &Fields, tokens: &mut TokenStream) {
    let mut fields = TokenStream::new();

    match (src, dst) {
        (_, Fields::Unit) => return,

        (Fields::Named(src), Fields::Named(dst)) => {
            for field in src.named.iter().take(dst.named.len()) {
                quote_into!(fields += #(field.ident),);
            }

            for field in dst.named.iter().skip(src.named.len()) {
                quote_into!(fields += #(field.ident): Default::default(),);
            }
        }

        (Fields::Named(src), Fields::Unnamed(dst)) => {
            for field in src.named.iter().take(dst.unnamed.len()) {
                quote_into!(fields += #(field.ident),);
            }

            for _ in dst.unnamed.iter().skip(src.named.len()) {
                quote_into!(fields += Default::default(),);
            }
        }

        (Fields::Unnamed(src), Fields::Named(dst)) => {
            let mut unnamed_name = String::with_capacity(7);
            _ = unnamed_name.write_str("field");

            let n_src_fields = src.unnamed.len();
            for (id, field) in dst.named.iter().enumerate() {
                quote_into!(fields += #(field.ident): );
                if id < n_src_fields {
                    _ = write!(unnamed_name, "{id}");
                    let unnamed_field = Ident::new(&unnamed_name, field.ident.span());
                    quote_into!(fields += #unnamed_field,);
                    unnamed_name.truncate(5);
                } else {
                    quote_into!(fields += Default::default(),);
                }
            }
        }

        (Fields::Unnamed(src), Fields::Unnamed(dst)) => {
            let mut unnamed_name = String::with_capacity(7);
            _ = unnamed_name.write_str("field");

            for id in 0..dst.unnamed.len().min(src.unnamed.len()) {
                _ = write!(unnamed_name, "{id}");
                let unnamed_field = Ident::new(&unnamed_name, Span::call_site());
                quote_into!(fields += #unnamed_field,);
                unnamed_name.truncate(5);
            }

            for _ in 0..dst.unnamed.len().saturating_sub(src.unnamed.len()) {
                quote_into!(fields += Default::default(),);
            }
        }

        (
            Fields::Unit,
            Fields::Named(FieldsNamed { named: f, .. })
            | Fields::Unnamed(FieldsUnnamed { unnamed: f, .. }),
        ) => {
            for field in f {
                quote_into!(fields += #(field.ident) #(field.colon_token) Default::default(),);
            }
        }
    }

    let delim = match dst {
        Fields::Named(_) => Delimiter::Brace,
        Fields::Unnamed(_) => Delimiter::Parenthesis,
        Fields::Unit => return,
    };

    tokens.extend([TokenTree::Group(Group::new(delim, fields))]);
}

impl Input {
    fn from_struct(src: ItemStruct, attr: TokenStream) -> syn::Result<Self> {
        let specs = InputSpecs::parse(attr)?;

        if let Some((span, _)) = specs.common_fields {
            return Err(syn::Error::new(span, "only applicable to enums"));
        }

        for field in src.fields.iter() {
            if !matches!(field.vis, Visibility::Public(_)) {
                return Err(syn::Error::new(field.span(), "Telegram types' fields must be public"));
            }
        }

        let repr = if specs.phantom_fields.is_some() || specs.name_all.is_some() {
            let (name_all_spec_span, new_names) =
                specs.name_all.unwrap_or_else(|| (Span::call_site(), vec![]));
            let (phantom_fields_spec_span, phantom_fields) =
                specs.phantom_fields.unwrap_or_else(|| (Span::call_site(), vec![]));
            let fields = apply_mods_to_fields(
                &src.fields,
                name_all_spec_span,
                new_names,
                phantom_fields_spec_span,
                phantom_fields,
            )?;
            Some(Repr::new(
                &src.ident,
                ReprKind::Struct {
                    fields: FieldsNamed {
                        brace_token: syn::token::Brace { span: name_all_spec_span },
                        named: fields.into_iter().collect(),
                    }
                    .into(),
                },
            ))
        } else {
            None
        };

        Ok(Self {
            attrs: src.attrs,
            name: src.ident,
            generics: src.generics,
            kind: InputKind::Struct { fields: src.fields },
            copy: specs.copy,
            untagged: specs.untagged,
            partial_eq: specs.partial_eq,
            repr,
        })
    }

    fn from_enum(mut src: ItemEnum, attr: TokenStream) -> syn::Result<Self> {
        let specs = InputSpecs::parse(attr)?;

        if let (Some((s, _)), _) | (_, Some((s, _))) = (specs.phantom_fields, specs.name_all) {
            return Err(syn::Error::new(s, "only applicable to structs or enum variants"));
        }

        let mut common_fields = specs
            .common_fields
            .into_iter()
            .flat_map(|(_, fields)| fields)
            .map(|field| CommonField {
                paths: vec![TokenStream::new(); src.variants.len()],
                ..field
            })
            .collect::<Vec<_>>();

        let mut repr_builder = EnumReprBuilder::default();

        for (v_id, v) in src.variants.iter_mut().enumerate() {
            let (attrs, specs) = VariantSpecs::from_attrs(take(&mut v.attrs))?;
            let specs = specs.unwrap_or_default();
            v.attrs = attrs;

            for (field_name, field_path) in specs.nested_fields {
                let field_id = common_fields
                    .iter()
                    .position(|field| field.name == field_name)
                    .ok_or_else(|| {
                        syn::Error::new(
                            field_name.span(),
                            "this field isn't defined as a common field",
                        )
                    })?;

                common_fields[field_id].paths[v_id] = field_path;
            }

            let (name_all_spec_span, new_names) =
                specs.name_all.unwrap_or_else(|| (Span::call_site(), vec![]));
            let (phantom_fields_spec_span, phantom_fields) =
                specs.phantom_fields.unwrap_or_else(|| (Span::call_site(), vec![]));
            repr_builder.add_variant(
                v_id,
                v,
                name_all_spec_span,
                new_names,
                phantom_fields_spec_span,
                phantom_fields,
            )?;
        }

        for field in &mut common_fields {
            for (path, variant) in zip(&mut field.paths, &src.variants) {
                if path.is_empty() {
                    if variant
                        .fields
                        .iter()
                        .any(|f| f.ident.as_ref().is_some_and(|i| *i == field.name))
                    {
                        if field.optional {
                            quote_into!(path += Some(&#(field.name)));
                        } else {
                            quote_into!(path += &#(field.name));
                        }
                    } else {
                        if !field.optional {
                            return Err(syn::Error::new(
                                variant.span(),
                                format!(
                                    "path to common field `{}` is undefined for variant `{}`",
                                    field.name, variant.ident,
                                ),
                            ));
                        }
                        quote_into!(path += None);
                    }
                } else if field.optional {
                    let inner = take(path);
                    quote_into!(path += Some(#inner));
                }
            }
        }

        Ok(Self {
            repr: repr_builder.finish(&src.ident, &src.variants),
            attrs: src.attrs,
            name: src.ident,
            generics: src.generics,
            copy: specs.copy,
            untagged: specs.untagged,
            partial_eq: specs.partial_eq,
            kind: InputKind::Enum { variants: src.variants.into_iter().collect(), common_fields },
        })
    }

    pub fn parse(attr: TokenStream) -> impl Parser<Output = Self> {
        move |input: ParseStream| {
            let attrs = Attribute::parse_outer(input)?;
            let vis = input.parse::<Visibility>()?;
            if !matches!(vis, Visibility::Public(_)) {
                return Err(syn::Error::new(
                    vis.span(),
                    "Telegram types must be part of the public API",
                ));
            }

            if input.peek(Token![struct]) {
                Input::from_struct(ItemStruct { attrs, ..input.parse()? }, attr)
            } else if input.peek(Token![enum]) {
                Input::from_enum(ItemEnum { attrs, ..input.parse()? }, attr)
            } else {
                return Err(syn::Error::new(input.span(), "expected a struct or an enum"));
            }
        }
    }

    fn make_type(&self, dst: &mut TokenStream) {
        quote_into!(dst += #[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]);
        // TODO: apply the span
        if self.copy.is_some() {
            quote_into!(dst += #[derive(Copy)]);
        }

        if self.partial_eq.is_none() {
            quote_into!(dst += #[derive(Eq)]);
        }

        if let Some(repr) = &self.repr {
            let name = Literal::string(&repr.name.to_string());
            quote_into!(dst += #[serde(from = #name, into = #name)]);
        } else if self.untagged.is_some() {
            quote_into!(dst += #[serde(untagged)])
        } else {
            quote_into!(dst += #[serde(rename_all = "snake_case")]);
        }

        for attr in &self.attrs {
            attr.to_tokens(dst);
        }

        match &self.kind {
            InputKind::Enum { variants, .. } => quote_into! { dst +=
                pub enum #(self.name)#(self.generics) {
                    #{
                        for variant in variants {
                            quote_into!(dst += #variant,);
                        }
                    }
                }
            },
            InputKind::Struct { fields } => quote_into! { dst +=
                pub struct #(self.name)#(self.generics) #fields
                    #{if !matches!(fields, Fields::Named(_)) {
                        quote_into!(dst += ;);
                    }}
            },
        }
    }

    fn make_common_fields_getters(&self, dst: &mut TokenStream) {
        let InputKind::Enum { variants, common_fields } = &self.kind else {
            return;
        };

        quote_into! { dst +=
            #[automatically_derived]
            impl #(self.generics) #(self.name)#(self.generics) {#{
                for field in common_fields {
                    quote_into! { dst +=
                        pub fn #(field.name)(&self) -> #{field.make_getter_return_type(dst)} {
                            match self {
                                #{
                                    for (variant, path) in zip(variants, &field.paths) {
                                        make_variant_pattern(&self.name, &variant.ident, &variant.fields, dst);
                                        quote_into!(dst += => #path,);
                                    }
                                }
                            }
                        }
                    }
                }
            }}
        }
    }

    fn make_default_impl(&self, dst: &mut TokenStream) {
        let InputKind::Enum { variants, .. } = &self.kind else {
            return;
        };

        if variants.iter().any(|v| v.ident == "None") {
            quote_into! { dst +=
                impl #(self.generics) ::std::default::Default for #(self.name) #(self.generics) {
                    fn default() -> Self {
                        Self::None
                    }
                }
            }
        }
    }

    fn make_from_impls(&self, dst: &mut TokenStream) {
        let InputKind::Enum { variants, .. } = &self.kind else {
            return;
        };

        let mut occurences = HashMap::<&Type, (&Ident, usize)>::new();
        for variant in variants {
            let mut fields = variant.fields.iter();
            let Some(field) =
                fields.next().filter(|f| f.ident.is_none() && fields.next().is_none())
            else {
                continue;
            };
            occurences.entry(&field.ty).or_insert((&variant.ident, 0)).1 += 1;
        }

        for (ty, (variant_name, n_occurences)) in occurences {
            if n_occurences != 1 {
                continue;
            }

            quote_into! { dst +=
                impl #(self.generics) From<#ty> for #(self.name) #(self.generics) {
                    fn from(value: #ty) -> Self {
                        Self::#variant_name(value)
                    }
                }
            }
        }
    }

    fn make_repr(&self, dst: &mut TokenStream) {
        if let Some(repr) = &self.repr {
            repr.to_tokens(dst, &self.name, &self.generics, self.untagged, &self.attrs, &self.kind);
        }
    }
}

impl ToTokens for Input {
    fn to_tokens(&self, dst: &mut TokenStream) {
        self.make_type(dst);
        self.make_common_fields_getters(dst);
        self.make_default_impl(dst);
        self.make_from_impls(dst);
        self.make_repr(dst);
    }
}
