extern crate proc_macro;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, LitStr, Token, parse_macro_input};
use quote::{quote, quote_spanned, format_ident};


struct LitDef {
    name: Ident,
    value: LitStr
}

impl Parse for LitDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=>]>()?;
        let value: LitStr = input.parse()?;

        Ok(LitDef {
            name,
            value
        })
    }
}

/// 循环依赖，只是作为一个废弃示例
#[proc_macro]
pub fn make_regex_node(input: TokenStream) -> TokenStream {
    let LitDef {
        name,
        value
    } = parse_macro_input!(input as LitDef);

    let defined_regex_name =  format_ident!("{}", name.to_string() + "_r");

    TokenStream::from(quote! {
        pub fn #defined_regex_name() -> ::alg_parser::regex::RegexNode {
            ::alg_parser::regex::lit_regex_node(#value)
        }
    })
}

// /// Just implies print --verbose
// #[proc_macro_attribute]
// pub fn verbose(_attr: TokenStream, item: TokenStream) -> TokenStream {
//     item
// }

#[cfg(test)]
mod tests {

}
