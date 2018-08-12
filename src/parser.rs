//! Parses a raw `macro_rules!`-string.

use syn;
use proc_macro2::{Delimiter, Ident, Literal, Punct, TokenStream};

use syn::punctuated::Punctuated;
use syn::synom::Synom;
use syn::token::Dollar;

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
    Lifetime,
}

macro_rules! delimited {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        alt!($i,
            parens!($submac!($($args)*)) => { |(_, content)| (Delimiter::Parenthesis, content) } |
            braces!($submac!($($args)*)) => { |(_, content)| (Delimiter::Brace, content) } |
            brackets!($submac!($($args)*)) => { |(_, content)| (Delimiter::Bracket, content) }
        )
    };
}

impl Synom for MacroRules {
    named!(parse -> Self, do_parse!(
        custom_keyword!(macro_rules) >>
        punct!(!) >>
        name: syn!(Ident) >>
        rules: delimited!(call!(Punctuated::<Rule, Token![;]>::parse_terminated_nonempty)) >>
        cond!(rules.0 == Delimiter::Parenthesis || rules.0 == Delimiter::Bracket, punct!(;)) >>
        (MacroRules {
            name,
            rules: rules.1.into_iter().collect(),
        })
    ));
}

impl Synom for Rule {
    named!(parse -> Self, do_parse!(
        matcher: delimited!(many0!(syn!(Matcher))) >>
        punct!(=>) >>
        expansion: delimited!(syn!(TokenStream)) >>
        (Rule {
            matcher: matcher.1,
            expansion: expansion.1,
        })
    ));
}

impl Synom for Matcher {
    named!(parse -> Self, alt!(
        do_parse!(
            syn!(Dollar) >>
            name: syn!(Ident) >>
            punct!(:) >>
            fragment: syn!(Fragment) >>
            (Matcher::Fragment { name, fragment })
        ) |
        do_parse!(
            syn!(Dollar) >>
            content: parens!(many0!(syn!(Matcher))) >>
            separator: option!(syn!(Separator)) >>
            repetition: syn!(Repetition) >>
            (Matcher::Repeat { content: content.1, separator, repetition })
        ) |
        delimited!(many0!(syn!(Matcher))) => {
            |(delimiter, content)| Matcher::Group { delimiter, content }
        } |
        do_parse!(
            not!(syn!(Dollar)) >>
            punct: syn!(Punct) >>
            (Matcher::Punct(punct))
        ) |
        call!(Ident::parse_any) => { Matcher::Ident } |
        syn!(Literal) => { Matcher::Literal }
    ));
}

impl Synom for Repetition {
    named!(parse -> Self, alt!(
        punct!(*) => { |_| Repetition::Repeated } |
        punct!(+) => { |_| Repetition::AtLeastOnce } |
        punct!(?) => { |_| Repetition::AtMostOnce }
    ));
}

impl Synom for Separator {
    named!(parse -> Self, alt!(
        do_parse!(
            not!(syn!(Repetition)) >>
            punct: syn!(Punct) >>
            (Separator::Punct(punct))
        ) |
        call!(Ident::parse_any) => { Separator::Ident } |
        syn!(Literal) => { Separator::Literal }
    ));
}

impl Synom for Fragment {
    named!(parse -> Self, alt!(
        custom_keyword!(ident) => { |_| Fragment::Ident } |
        custom_keyword!(path) => { |_| Fragment::Path } |
        custom_keyword!(expr) => { |_| Fragment::Expr } |
        custom_keyword!(ty) => { |_| Fragment::Ty } |
        custom_keyword!(pat) => { |_| Fragment::Pat } |
        custom_keyword!(stmt) => { |_| Fragment::Stmt } |
        custom_keyword!(block) => { |_| Fragment::Block } |
        custom_keyword!(item) => { |_| Fragment::Item } |
        custom_keyword!(meta) => { |_| Fragment::Meta } |
        custom_keyword!(tt) => { |_| Fragment::Tt } |
        custom_keyword!(lifetime) => { |_| Fragment::Lifetime }
    ));
}

pub fn parse(src: &str) -> Result<MacroRules, syn::synom::ParseError> {
    syn::parse_str::<MacroRules>(src)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn should_parse() {
        // A more or less random collection of macro_rules!()-blocks which should parse
        // successfully.
        let fixture = &[
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
