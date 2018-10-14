//! Parses a raw `macro_rules!`-string.

use syn;
use proc_macro2::{Delimiter, Ident, Literal, Punct, TokenStream};

use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
pub struct MacroRules {
    pub name: Ident,
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub struct Rule {
    pub matcher: Vec<Matcher>,
    pub expansion: TokenStream,
}

#[derive(Debug)]
pub enum Matcher {
    Punct(Punct),
    Ident(Ident),
    Literal(Literal),
    Group {
        delimiter: Delimiter,
        content: Vec<Matcher>,
    },
    Repeat {
        content: Vec<Matcher>,
        separator: Option<Separator>,
        repetition: Repetition,
    },
    Fragment {
        name: Ident,
        fragment: Fragment,
    },
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, PartialOrd, Ord)]
pub enum Repetition {
    /// `$(...)*`
    Repeated,
    /// `$(...)+`
    AtLeastOnce,
    /// `$(...)?`
    AtMostOnce,
}

#[derive(Debug)]
pub enum Separator {
    Punct(Punct),
    Ident(Ident),
    Literal(Literal),
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, PartialOrd, Ord)]
pub enum Fragment {
    Ident,
    Path,
    Expr,
    Ty,
    Pat,
    Stmt,
    Block,
    Item,
    Meta,
    Tt,
    Vis,
    Literal,
    Lifetime,
}

macro_rules! delimited {
    ($content:ident in $cursor:expr) => {
        if let syn::export::Ok(parens) = syn::group::parse_parens(&$cursor) {
            $content = parens.content;
            Delimiter::Parenthesis
        } else if let syn::export::Ok(braces) = syn::group::parse_braces(&$cursor) {
            $content = braces.content;
            Delimiter::Brace
        } else if let syn::export::Ok(brackets) = syn::group::parse_brackets(&$cursor) {
            $content = brackets.content;
            Delimiter::Bracket
        } else {
            return syn::export::Err($cursor.error("expected delimiter"));
        }
    };
}

mod kw {
    custom_keyword!(macro_rules);

    custom_keyword!(ident);
    custom_keyword!(path);
    custom_keyword!(expr);
    custom_keyword!(ty);
    custom_keyword!(pat);
    custom_keyword!(stmt);
    custom_keyword!(block);
    custom_keyword!(item);
    custom_keyword!(meta);
    custom_keyword!(tt);
    custom_keyword!(vis);
    custom_keyword!(literal);
    custom_keyword!(lifetime);
}

impl Parse for MacroRules {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        input.parse::<kw::macro_rules>()?;
        input.parse::<syn::token::Bang>()?;
        let name = input.parse()?;

        let content;
        delimited!(content in input);
        let mut rules = vec![content.parse()?];
        if content.parse::<syn::token::Semi>().is_ok() {
            rules.extend(content.parse_terminated::<_, syn::token::Semi>(Rule::parse)?);
        }

        Ok(MacroRules {
            name,
            rules: rules.into_iter().collect()
        })
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let matcher;
        delimited!(matcher in input);

        input.parse::<syn::token::FatArrow>()?;

        let expansion;
        delimited!(expansion in input);

        Ok(Rule {
            matcher: {
                let mut v = Vec::new();
                while let Ok(m) = matcher.parse() {
                    v.push(m);
                }
                v
            },
            expansion: expansion.parse()?
        })
    }
}

fn parse_fragment_matcher(input: ParseStream) -> syn::parse::Result<Matcher> {
    input.parse::<syn::token::Dollar>()?;
    let name = input.call(Ident::parse_any)?;
    input.parse::<syn::token::Colon>()?;
    let fragment = input.parse()?;

    Ok(Matcher::Fragment { name, fragment })
}

fn parse_repeat_matcher(input: ParseStream) -> syn::parse::Result<Matcher> {
    input.parse::<syn::token::Dollar>()?;
    let content;
    parenthesized!(content in input);
    let separator = input.parse().ok();
    let repetition = input.parse()?;

    Ok(Matcher::Repeat {
        content: {
            let mut v = Vec::new();
            while let Ok(matcher) = content.parse() {
                v.push(matcher);
            }
            v
        },
        separator,
        repetition
    })
}

fn parse_group_matcher(input: ParseStream) -> syn::parse::Result<Matcher> {
    let content;
    let delimiter = delimited!(content in input);
    Ok(Matcher::Group {
        delimiter,
        content: {
            let mut v = Vec::new();
            while let Ok(matcher) = content.parse() {
                v.push(matcher);
            }
            v
        }
    })
}

fn parse_punct_matcher(input: ParseStream) -> syn::parse::Result<Matcher> {
    if input.parse::<syn::token::Dollar>().is_ok() {
        return Err(input.error(""));
    }

    input.parse().map(Matcher::Punct)
}

impl Parse for Matcher {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if let Ok(fragment) = input.call(parse_fragment_matcher) {
            Ok(fragment)
        } else if let Ok(repetition) = input.call(parse_repeat_matcher) {
            Ok(repetition)
        } else if let Ok(group) = input.call(parse_group_matcher) {
            Ok(group)
        } else if let Ok(punct) = input.call(parse_punct_matcher) {
            Ok(punct)
        } else if let Ok(ident) = input.call(Ident::parse_any) {
            Ok(Matcher::Ident(ident))
        } else if let Ok(syn::Lifetime { ident, .. }) = input.parse() {
            Ok(Matcher::Ident(ident))
        } else if let Ok(literal) = input.parse::<Literal>() {
            Ok(Matcher::Literal(literal))
        } else {
            Err(input.error("expected matcher"))
        }
    }
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if input.parse::<syn::token::Star>().is_ok() {
            Ok(Repetition::Repeated)
        } else if input.parse::<syn::token::Add>().is_ok() {
            Ok(Repetition::AtLeastOnce)
        } else if input.parse::<syn::token::Question>().is_ok() {
            Ok(Repetition::AtMostOnce)
        } else {
            Err(input.error("expected repetition marker"))
        }
    }
}

impl Parse for Separator {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if let Ok(punct) = input.call(|content| {
            if content.parse::<Repetition>().is_ok() {
                return Err(content.error(""));
            }
            content.parse::<Punct>()
        }) {
            Ok(Separator::Punct(punct))
        } else if let Ok(ident) = input.call(Ident::parse_any) {
            Ok(Separator::Ident(ident))
        } else if let Ok(lit) = input.parse::<Literal>() {
            Ok(Separator::Literal(lit))
        } else {
            Err(input.error("expected separator"))
        }
    }
}

impl Parse for Fragment {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if input.parse::<kw::ident>().is_ok() {
            Ok(Fragment::Ident)
        } else if input.parse::<kw::path>().is_ok() {
            Ok(Fragment::Path)
        } else if input.parse::<kw::expr>().is_ok() {
            Ok(Fragment::Expr)
        } else if input.parse::<kw::ty>().is_ok() {
            Ok(Fragment::Ty)
        } else if input.parse::<kw::pat>().is_ok() {
            Ok(Fragment::Pat)
        } else if input.parse::<kw::stmt>().is_ok() {
            Ok(Fragment::Stmt)
        } else if input.parse::<kw::block>().is_ok() {
            Ok(Fragment::Block)
        } else if input.parse::<kw::item>().is_ok() {
            Ok(Fragment::Item)
        } else if input.parse::<kw::meta>().is_ok() {
            Ok(Fragment::Meta)
        } else if input.parse::<kw::tt>().is_ok() {
            Ok(Fragment::Tt)
        } else if input.parse::<kw::vis>().is_ok() {
            Ok(Fragment::Vis)
        } else if input.parse::<kw::literal>().is_ok() {
            Ok(Fragment::Literal)
        } else if input.parse::<kw::lifetime>().is_ok() {
            Ok(Fragment::Lifetime)
        } else {
            Err(input.error("expected fragment keyword"))
        }
    }
}

pub fn parse(src: &str) -> syn::parse::Result<MacroRules> {
    syn::parse_str::<MacroRules>(src)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn issue_5() {
        // Keywords as fragment-names should parse
        let src = r#"macro_rules! a {
    ($self:ident) => { ... };
}"#;
        parse(&src).unwrap();
    }

    #[test]
    fn should_parse() {
        // A more or less random collection of macro_rules!()-blocks which should parse
        // successfully.
        let fixture = &[
r#"macro_rules! a {
(
$item:item
$block:block
$stmt:stmt
$pat:pat
$expr:expr
$ty:ty
$ident:ident
$path:path
$vis:vis
$literal:literal
$meta:meta
$lifetime:lifetime
) => {};
}"#,
r#"macro_rules! vec {
    ( $ elem : expr ; $ n : expr ) => { ... };
    ( $ ( $ x : expr ) , * ) => { ... };
    ( $ ( $ x : expr , ) * ) => { ... };
}"#,
r#"macro_rules! println {
    () => { ... };
    ($fmt:expr) => { ... };
    ($fmt:expr, $($arg:tt)*) => { ... };
}"#,
r#"macro_rules! assert_eq {
    ( $ left : expr , $ right : expr ) => { ... };
    ( $ left : expr , $ right : expr , ) => { ... };
    (
$ left : expr , $ right : expr , $ ( $ arg : tt ) + ) => { ... };
}"#,
r#"macro_rules! panic {
    () => { ... };
    ($msg:expr) => { ... };
    ($msg:expr,) => { ... };
    ($fmt:expr, $($arg:tt)+) => { ... };
}"#,
r#"macro_rules! input_end {
    ($i:expr,) => { ... };
}"#,
r#"macro_rules! apply {
    ($i:expr, $fun:expr, $($args:expr),* ) => { ... };
}"#,
        ][..];
        for src in fixture {
            parse(&src).expect(src);
        }
    }
}
