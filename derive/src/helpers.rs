use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

/// Expands a formatting expression into a call to `write!()` if applicable,
/// otherwise returns the tokens making up the expression.
pub fn expand_fmt_shorthand(expr: &syn::Expr) -> TokenStream {
    match expr {
        syn::Expr::Lit(l) if matches!(l.lit, syn::Lit::Str(_)) => {
            quote! { std::write!(f, #l) }
        }
        syn::Expr::Tuple(syn::ExprTuple { elems, .. })
            if matches!(
                elems.first(),
                Some(syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(_),
                    ..
                }))
            ) =>
        {
            let elems = elems.iter();

            quote! { std::write!(f, #( #elems, )* ) }
        }
        x => quote! { #x },
    }
}

/// Returns the names of the given fields.
///
/// If the fields are unnammed, the names will be `v{i}`, with `i` the index of
/// each field.
pub fn field_idents(fields: &syn::Fields) -> Vec<Ident> {
    match fields {
        syn::Fields::Named(named) => named
            .named
            .iter()
            .map(|x| x.ident.clone().unwrap())
            .collect(),
        syn::Fields::Unnamed(unnamed) => (0..unnamed.unnamed.len())
            .map(|i| format_ident!("v{}", i))
            .collect(),
        syn::Fields::Unit => Vec::new(),
    }
}

/// Returns a pattern that can be used to extract the specified fields.
pub fn match_pattern(fields: &syn::Fields, field_idents: &[Ident]) -> TokenStream {
    match fields {
        syn::Fields::Named(_) => {
            quote! { { #( #field_idents, )* } }
        }
        syn::Fields::Unnamed(_) => {
            quote! { ( #( #field_idents, )* ) }
        }
        syn::Fields::Unit => quote! {},
    }
}
