#![feature(if_let_guard)]

use proc_macro::TokenStream;
use proc_macro2::Span;

use quote::quote;
use syn::{parse_macro_input, DeriveInput, DataEnum, ItemEnum};

const UGLY_IDEN_PREFIX: &'static str = "__COMUNE_MACROS_INTERNALS";

#[proc_macro_derive(TryAs)]
pub fn derive_try_as(tt: TokenStream) -> TokenStream {
    let i = parse_macro_input!(tt as ItemEnum);

    let enum_name = i.ident;

    let (im_gen, ty_gen, wh_cla) = i.generics.split_for_impl();

    let mut ex = quote! {};

    let included_trait_name = syn::Ident::new(
        &format!("{UGLY_IDEN_PREFIX}_included_{}", enum_name),
        Span::call_site()
    );

    ex = quote! {
        #ex
        trait #included_trait_name {
            const INDEX: usize;
        }
    };

    ex = quote!{
        #ex
        impl #im_gen #enum_name #ty_gen #wh_cla {
            pub fn index_of<T: #included_trait_name>() -> usize {
                T::INDEX
            }
        }
    };

    for (i, variant) in i.variants.iter().enumerate() {
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

        let str_i = format!("{i}");
        let lit_i = syn::LitInt::new(&str_i, Span::call_site());

        ex = quote! {
            #ex
            impl #im_gen #included_trait_name for #var_type #wh_cla {
                const INDEX: usize = #lit_i;
            }

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
