#![feature(if_let_guard)]

use proc_macro::TokenStream;

use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, ItemEnum};

/// Creates an enum with the same variants as the input enum but with no
/// values associated with them
///
/// ```rust
/// #[derive(Kind)]
/// #[kind(EntityKind)]
/// enum Entity {
///     Player {
///         username: String,
///         pos: (f64, f64),
///     },
///     Enemy {
///         life: u32,
///         pos: (f64, f64),
///     }
/// }
/// // Will generate
/// enum EntityKind {
///     Player,
///     Enemy,
/// }
/// ```
#[proc_macro_derive(Kind, attributes(kind))]
pub fn derive_kind(tt: TokenStream) -> TokenStream {
    let i = parse_macro_input!(tt as ItemEnum);

    let mut name = None;
    for attr in i.attrs {
        if !attr.path().is_ident("kind")
        { continue; }
        let a = attr.parse_args::<Ident>().unwrap();
        name = Some(a);
    }
    let Some(name) = name
        else { return quote!{ compile_error!() }.into() };

    let mut variants = Vec::new();

    for v in i.variants {
        let mut name = v.ident;
        for attr in v.attrs {
            if !attr.path().is_ident("kind")
            { continue; }
            name = attr.parse_args::<Ident>().unwrap();
        }
        variants.push(quote! { #name });
    }

    let vis = i.vis;
    let ex = quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #vis enum #name {
            #(#variants),*
        }
    };

    ex.into()
}

#[proc_macro_derive(TryAs)]
pub fn derive_try_as(tt: TokenStream) -> TokenStream {
    let i = parse_macro_input!(tt as ItemEnum);

    let enum_name = i.ident;

    let (im_gen, ty_gen, wh_cla) = i.generics.split_for_impl();

    let mut ex = quote! {};

    for variant in i.variants.iter() {
        let var_name = &variant.ident;
        let var_type = match &variant.fields {
            syn::Fields::Unnamed(u) if let Some(f) = u.unnamed.first() => {
                &f.ty
            },

            _ => {
                return quote! {
                    compile_error!("Only tuble variants are supported")
                }.into();
            }
        };

        ex = quote! {
            #ex
            impl #im_gen crate::try_as::TryAsRef<#var_type> for #enum_name #ty_gen #wh_cla {
                fn try_as_ref(&self) -> Option<&#var_type> {
                    match self {
                        Self::#var_name(v) => Some(v),
                        _ => None,
                    }
                }
            }

            impl #im_gen crate::try_as::TryAsMut<#var_type> for #enum_name #ty_gen #wh_cla {
                fn try_as_mut(&mut self) -> Option<&mut #var_type> {
                    match self {
                        Self::#var_name(v) => Some(v),
                        _ => None,
                    }
                }
            }

            impl #im_gen From<#var_type> for #enum_name #ty_gen #wh_cla {
                fn from(v: #var_type) -> Self {
                    Self::#var_name(v)
                }
            }

            impl #im_gen ::std::convert::TryInto<#var_type> for #enum_name #ty_gen #wh_cla {
                type Error = ();

                fn try_into(self) -> Result<#var_type, ()> {
                    match self {
                        Self::#var_name(v) => Ok(v),
                        _ => Err(()),
                    }
                }
            }
        };
    }
    
    ex.into()
}
