use ident_case::RenameRule;
use proc_macro2::{Ident, TokenStream};
use proc_macro_error::abort;
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
};

/// Arguments given to the [`crate::matcher`] attribute.
pub struct MatcherArgs {
    expected: syn::Expr,
    name: Option<Ident>,
    explanation: Option<syn::Expr>,
}

impl Parse for MatcherArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut expected = None;
        let mut name = None;
        let mut explanation = None;

        loop {
            let key: Ident = input.parse()?;
            let key_str = key.to_string();

            input.parse::<syn::Token![=]>()?;

            match key_str.as_str() {
                "expected" => expected = Some(input.parse()?),
                "name" => name = Some(input.parse()?),
                "explanation" => explanation = Some(input.parse()?),
                k => abort!(key, "unknown key `{}`", k),
            }

            if input.is_empty() {
                break;
            }

            input.parse::<syn::Token![,]>()?;
        }

        let expected = expected.expect("`expected` must be specified");

        Ok(Self {
            expected,
            name,
            explanation,
        })
    }
}

/// Expands the body of the [`crate::matcher`] attribute.
pub fn expand(
    MatcherArgs {
        expected,
        name,
        explanation,
    }: MatcherArgs,
    mut fn_item: syn::ItemFn,
) -> TokenStream {
    // Validate function.
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = &mut fn_item;

    if sig.abi.is_some() {
        abort!(sig.abi, "matcher function cannot have an abi specifier");
    }

    if sig.asyncness.is_some() {
        abort!(sig.asyncness, "matcher function cannot be async");
    }

    let is_unsafe = sig.unsafety.is_some();

    if is_unsafe {
        sig.unsafety = None;
    }

    // Compute identifiers.
    let base_name = match name {
        Some(name) => name,
        None => Ident::new(
            &RenameRule::PascalCase.apply_to_field(sig.ident.to_string()),
            sig.ident.span(),
        ),
    };

    let struct_ident = format_ident!("{}Matcher", base_name);
    let struct_doc = syn::LitStr::new(
        &format!(
            "[`Matcher`](matchrs::Matcher) returned by [`{}`].",
            sig.ident
        ),
        sig.span(),
    );

    // Extract and validate value type.
    let value_input = match sig.inputs.pop() {
        Some(last_input) => match last_input.into_value() {
            syn::FnArg::Typed(typed) => typed,
            syn::FnArg::Receiver(r) => {
                abort!(r, "matcher function must take at least one argument")
            }
        },
        None => abort!(
            sig.ident,
            "matcher function must take at least one argument"
        ),
    };
    let value_ty = match value_input.ty.as_ref() {
        syn::Type::Reference(syn::TypeReference {
            lifetime: None,
            mutability: None,
            elem,
            ..
        }) => elem.as_ref().clone(),
        t => abort!(
            t,
            "matcher function must take an immutable reference as last parameter"
        ),
    };

    // Validate input types.
    let mut dummy_field_names = Vec::with_capacity(sig.inputs.len());
    let mut matcher_field_tys = Vec::with_capacity(sig.inputs.len());
    let mut pattern_fields = Vec::with_capacity(sig.inputs.len());

    for (i, input) in sig.inputs.iter_mut().enumerate() {
        let typed = match input {
            syn::FnArg::Typed(typed) => typed,
            syn::FnArg::Receiver(r) => abort!(r, "matcher function cannot take a receiver"),
        };
        let dummy_name = format_ident!("v{}", i);
        let dummy_name_pat_ident = syn::PatIdent {
            ident: dummy_name.clone(),
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            subpat: None,
        };
        let pat = std::mem::replace(typed.pat.as_mut(), syn::Pat::Ident(dummy_name_pat_ident));

        dummy_field_names.push(dummy_name);
        matcher_field_tys.push(typed.ty.as_ref());
        pattern_fields.push(pat);
    }

    let (impl_generics, ty_generics, where_clause) = sig.generics.split_for_impl();

    // Validate and replace return type.
    let explicit_explanation_ty = match &mut sig.output {
        syn::ReturnType::Type(_, rt) => match rt.as_ref() {
            syn::Type::Path(path) if path.path.is_ident("bool") => {
                *rt.as_mut() = parse_quote! { #struct_ident #ty_generics };

                None
            }
            syn::Type::Path(syn::TypePath { path, .. })
                if path.segments.last().unwrap().ident == "Option" =>
            {
                let last_segment = path.segments.last().unwrap();

                let err_ty = match &last_segment.arguments {
                    syn::PathArguments::AngleBracketed(args) if args.args.len() == 1 => {
                        match &args.args[0] {
                            syn::GenericArgument::Type(ty) => quote! { #ty },
                            t => abort!(t, "matcher function explanation must be a type"),
                        }
                    }
                    r => abort!(r, "matcher function must return a `Option<Explanation>`"),
                };

                *rt.as_mut() = parse_quote! { #struct_ident #ty_generics };

                Some(err_ty)
            }
            r => abort!(
                r,
                "matcher function must return a `bool` or `Option<Explanation>`"
            ),
        },
        syn::ReturnType::Default => abort!(sig.ident, "matcher function must return a value"),
    };

    // Compute explanation.
    let (explanation_ty, explanation_ty_def, explanation_expr) =
        if let Some(explanation) = explanation {
            let explanation_ident = format_ident!("{}Explanation", base_name);
            let doc = syn::LitStr::new(
                &format!(
                    "[`Explanation`](matchrs::Explanation) returned by [`{}`].",
                    sig.ident
                ),
                sig.ident.span(),
            );

            (
                quote! { #explanation_ident },
                quote! {
                    #[doc = #doc]
                    #vis struct #explanation_ident(std::string::String);
                },
                quote! { #explanation_ident(#explanation) },
            )
        } else if let Some(explanation_ty) = &explicit_explanation_ty {
            (quote! { #explanation_ty }, quote! {}, quote! {})
        } else {
            (
                quote! { matchrs::NoExplanation },
                quote! {},
                quote! { matchrs::NoExplanation },
            )
        };
    let describe_expr = crate::helpers::expand_fmt_shorthand(&expected);

    // Build output.
    let marker_ty = sig.generics.lt_token.is_some().then(|| {
        let params = sig.generics.type_params().map(|x| &x.ident);
        let lts = sig.generics.lifetimes().map(|x| &x.lifetime);

        quote! {
            std::marker::PhantomData<(*const #value_ty, #(*const #params,)* #(&#lts ()),*)>,
        }
    });
    let marker_expr = marker_ty
        .is_some()
        .then(|| quote! { std::marker::PhantomData, });
    let match_body = if explicit_explanation_ty.is_none() {
        quote! {
            let success = (|| #block)();

            if success {
                return std::option::Option::None;
            }

            std::option::Option::Some( #explanation_expr )
        }
    } else {
        quote! { #block }
    };

    quote! {
        #[doc = #struct_doc]
        #vis struct #struct_ident #impl_generics(
            #marker_ty
            #( #matcher_field_tys, )*
        ) #where_clause;

        #explanation_ty_def

        impl #impl_generics matchrs::Matcher<#value_ty> for #struct_ident #ty_generics #where_clause {
            type Explanation = #explanation_ty;

            #[allow(unused_variables)]
            fn match_or_explain(&self, #value_input) -> std::option::Option<Self::Explanation> {
                let Self( #marker_expr #( #pattern_fields, )* ) = self;

                #match_body
            }

            #[allow(unused_variables)]
            fn describe<'d>(&'d self, options: &'d matchrs::DescribeOptions) -> matchrs::Description<'d, Self> {
                matchrs::Description::new(
                    self,
                    options,
                    |Self( #marker_expr #( #pattern_fields, )* ), options, f| {
                        let switch = |if_positive: &'static str, if_negative: &'static str| {
                            if !options.is_negated {
                                if_positive
                            } else {
                                if_negative
                            }
                        };

                        #describe_expr
                    },
                )
            }
        }

        #( #attrs )*
        #vis #sig {
            #struct_ident( #marker_expr #( #dummy_field_names, )* )
        }
    }
}
