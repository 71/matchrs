use proc_macro_error::proc_macro_error;
use quote::quote;
use syn::parse_macro_input;

mod attr;
mod derive;
pub(crate) mod helpers;

/// See [`matchrs::Match`].
#[proc_macro_derive(Match)]
#[proc_macro_error]
pub fn derive_match(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let syn::DeriveInput {
        ident,
        generics,
        data,
        vis,
        ..
    } = parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let self_ty = quote! { #ident #ty_generics };
    let fns: Vec<_> = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => fields
            .iter()
            .enumerate()
            .map(|v| derive::struct_fn(&self_ty, v))
            .collect(),
        syn::Data::Enum(syn::DataEnum { variants, .. }) => variants
            .iter()
            .map(|v| derive::enum_fn(&self_ty, &vis, v))
            .collect(),
        syn::Data::Union(syn::DataUnion { fields, .. }) => fields
            .named
            .iter()
            .map(|f| derive::union_fn(&self_ty, f))
            .collect(),
    };

    quote! {
        impl #impl_generics #ident #ty_generics #where_clause {
            #( #fns )*
        }
    }
    .into()
}

/// See [`matchrs::Explanation`].
#[proc_macro_derive(Explanation, attributes(matchrs))]
#[proc_macro_error]
pub fn derive_explanation(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let attr = input
        .attrs
        .iter()
        .find(|x| x.path.is_ident("matchrs"))
        .expect("a #[matchrs] attribute must be set");
    let attr_args = attr.parse_args().unwrap();

    derive::explanation_impls(attr_args, input).into()
}

/// See [`matchrs::matcher`].
#[proc_macro_attribute]
#[proc_macro_error]
pub fn matcher(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = syn::parse(attr).unwrap();
    let fn_item = parse_macro_input!(item);

    attr::expand(args, fn_item).into()
}
