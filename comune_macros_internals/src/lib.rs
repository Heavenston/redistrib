#![feature(if_let_guard)]

use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, ItemEnum};

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
