use ident_case::RenameRule;
use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::abort;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};

use crate::helpers;

/// Returns tokens defining a function that allows matching a `struct` field.
///
/// ```rust,no_compile
/// fn field_is(field_matcher: impl Matcher<FieldTy>) -> impl Matcher<Self>;
/// ```
pub fn struct_fn(
    self_ty: &TokenStream,
    (i, syn::Field { ident, ty, vis, .. }): (usize, &syn::Field),
) -> TokenStream {
    let (fn_ident, arg_ident, field_lit) = match ident {
        Some(ident) => (
            format_ident!("{}_is", ident),
            format_ident!("{}_matcher", ident),
            ident.to_string(),
        ),
        None => (
            format_ident!("v{}_is", i),
            format_ident!("v{}_matcher", i),
            i.to_string(),
        ),
    };
    let field_span = ident
        .as_ref()
        .map(|x| x.span())
        .unwrap_or_else(Span::mixed_site);
    let field_ident = Ident::new(&field_lit, field_span);
    let field_lit = syn::LitStr::new(&field_lit, field_span);

    quote! {
        #vis fn #fn_ident(#arg_ident: impl matchrs::Matcher<#ty>) -> impl matchrs::Matcher<#self_ty> {
            matchrs::matchers::helpers::property(
                #field_lit,
                #arg_ident,
                |x: &Self, inner| matchrs::Matcher::match_or_explain(inner, &x.#field_ident),
            )
        }
    }
}

/// Returns tokens defining a function that allows matching an `enum` variant.
///
/// ```rust,no_compile
/// fn is_variant(variant_matcher: impl Matcher<VariantTy>) -> impl Matcher<Self>;
/// ```
pub fn enum_fn(
    self_ty: &TokenStream,
    vis: &syn::Visibility,
    syn::Variant { ident, fields, .. }: &syn::Variant,
) -> TokenStream {
    let ident_str = ident.to_string();
    let variant_lit = syn::LitStr::new(&ident_str, ident.span());
    let ident_snake_case = RenameRule::SnakeCase.apply_to_variant(&ident_str);
    let fn_ident = format_ident!("is_{}", ident_snake_case, span = ident.span());

    let (arg_idents, field_idents) = fields
        .iter()
        .enumerate()
        .map(|(i, syn::Field { ident, .. })| match ident {
            Some(ident) => (format_ident!("{}_matcher", ident), ident.clone()),
            None if fields.len() == 1 => (format_ident!("matcher"), format_ident!("v")),
            None => (format_ident!("matcher{}", i), format_ident!("v{}", i)),
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let field_tys = fields.iter().map(|x| &x.ty).collect::<Vec<_>>();
    let pattern = helpers::match_pattern(fields, &field_idents);

    let (matchers_expr, match_or_explain_expr, tuple_ty) = match arg_idents.len() {
        0 => (
            quote! { matchrs::matchers::ANY },
            quote! { None },
            quote! { () },
        ),
        1 => {
            let arg_ident = &arg_idents[0];
            let field_ident = &field_idents[0];
            let field_ty = &field_tys[0];

            (
                quote! { #arg_ident },
                quote! { matchrs::Matcher::match_or_explain(inner, #field_ident) },
                quote! { #field_ty },
            )
        }
        _ => (
            quote! { ( #( #arg_idents ),* ) },
            quote! { matchrs::Matcher::match_or_explain(inner, &(#( #field_idents ),*)) },
            quote! { ( #( &#field_tys ),* ) },
        ),
    };

    quote! {
        #vis fn #fn_ident(
            #( #arg_idents: impl matchrs::Matcher<#field_tys> ),*
        ) -> impl matchrs::Matcher<#self_ty> {
            matchrs::matchers::helpers::variant::<_, #tuple_ty, _, _>(
                #variant_lit,
                #matchers_expr,
                |x: &Self, inner| match x {
                    Self::#ident #pattern => Some(Some(#match_or_explain_expr?)),
                    _ => Some(None),
                },
            )
        }
    }
}

/// Returns tokens defining a function that allows matching a `union` field.
///
/// ```rust,no_compile
/// unsafe fn field_is(field_matcher: impl Matcher<FieldTy>) -> impl Matcher<Self>;
/// ```
pub fn union_fn(
    self_ty: &TokenStream,
    syn::Field { ident, ty, vis, .. }: &syn::Field,
) -> TokenStream {
    let ident = ident.as_ref().unwrap();
    let (fn_ident, arg_ident, field_lit) = (
        format_ident!("{}_is", ident),
        format_ident!("{}_matcher", ident),
        syn::LitStr::new(&ident.to_string(), ident.span()),
    );

    quote! {
        #vis unsafe fn #fn_ident(#arg_ident: impl matchrs::Matcher<#ty>) -> impl matchrs::Matcher<#self_ty> {
            matchrs::matchers::helpers::property(
                #field_lit,
                #arg_ident,
                |x: &Self, inner| {
                    matchrs::Matcher::match_or_explain(inner, unsafe { &x.#ident })
                },
            )
        }
    }
}

/// Arguments given to the [`Explanation`](crate::derive_explanation) derive.
pub struct ExplanationArgs {
    explanation: Option<syn::Expr>,
    is_empty: Option<syn::Expr>,
}

impl Parse for ExplanationArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut explanation = None;
        let mut is_empty = None;

        loop {
            let key: Ident = input.parse()?;
            let key_str = key.to_string();

            input.parse::<syn::Token![=]>()?;

            match key_str.as_str() {
                "explanation" => explanation = Some(input.parse()?),
                "is_empty" => is_empty = Some(input.parse()?),
                k => abort!(key, "unknown key `{}`", k),
            }

            if input.is_empty() {
                break;
            }

            input.parse::<syn::Token![,]>()?;
        }

        Ok(Self {
            explanation,
            is_empty,
        })
    }
}

/// Returns token defining the `impl`s of an `Explanation`.
pub fn explanation_impls(
    args: ExplanationArgs,
    syn::DeriveInput {
        ident,
        generics,
        data,
        ..
    }: syn::DeriveInput,
) -> TokenStream {
    let struct_data = match data {
        syn::Data::Struct(s) => s,
        _ => abort!(ident, "#[derive(Explanation)] requires a struct"),
    };
    let field_idents = helpers::field_idents(&struct_data.fields);
    let fields_pattern = helpers::match_pattern(&struct_data.fields, &field_idents);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let display_impl = args.explanation.map(|fmt| {
        let mut fmt = crate::helpers::expand_fmt_shorthand(&fmt);

        if args.is_empty.is_some() {
            fmt = quote! { if matchrs::Explanation::is_empty(self) { Ok(()) } else { #fmt } };
        }

        quote! {
            impl #impl_generics std::fmt::Display for #ident #ty_generics #where_clause {
                #[allow(unused_variables)]
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let Self #fields_pattern = self;

                    #fmt
                }
            }
        }
    });

    let explanation_impl = args.is_empty.map(|is_empty| {
        quote! {
            impl #impl_generics matchrs::Explanation for #ident #ty_generics #where_clause {
                #[allow(unused_variables)]
                fn is_empty(&self) -> bool {
                    let Self #fields_pattern = self;

                    #is_empty
                }
            }
        }
    });

    quote! {
        #display_impl
        #explanation_impl
    }
}
