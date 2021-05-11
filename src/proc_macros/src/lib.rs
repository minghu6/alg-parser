extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::{ Span };
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, LitStr, Token, parse_macro_input, Path};
use quote::{quote, format_ident};


////////////////////////////////////////////////////////////////////////////////
/////// Meta Macro Tools

struct VecMacroRules {
    name: Ident,
    path: Path,
    inc_op: Ident
}

impl Parse for VecMacroRules {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let path: Path = input.parse()?;

        let inc_op;
        if let Ok(_) = input.parse::<Token![,]>() {
            inc_op = input.parse()?;
        } else {
            inc_op = Ident::new("insert", Span::call_site());
        }

        Ok(Self {
            name,
            path,
            inc_op
        })
    }
}

#[proc_macro]
pub fn make_vec_macro_rules(input: TokenStream) -> TokenStream {
    let VecMacroRules {
        name,
        path,
        inc_op
    } = parse_macro_input!(input as VecMacroRules);

    TokenStream::from(quote! {
        #[macro_export]
        macro_rules! #name {
            ( $($value:expr),* ) => {
                {
                    let mut vec_like = #path::new();

                    $(
                        vec_like.#inc_op($value);
                    )*

                    vec_like
                }
            };
        }
    })
}


#[cfg(test)]
mod tests {

}
