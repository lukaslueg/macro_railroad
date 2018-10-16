//! Parses a raw `macro_rules!`-string.

use proc_macro2::{Delimiter, Ident, Literal, Punct, TokenStream, TokenTree};

use syn;
use syn::ext::IdentExt;
use syn::parse::{Error, Parse, ParseBuffer, ParseStream, Result};
use syn::token::{Brace, Bracket, Dollar, Paren};
use syn::Lifetime;

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
    Lifetime(Lifetime),
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

fn delimited(input: ParseStream) -> Result<(Delimiter, ParseBuffer)> {
    let content;
    let delimiter = if input.peek(Paren) {
        parenthesized!(content in input);
        Delimiter::Parenthesis
    } else if input.peek(Brace) {
        braced!(content in input);
        Delimiter::Brace
    } else if input.peek(Bracket) {
        bracketed!(content in input);
        Delimiter::Bracket
    } else {
        return Err(input.error("expected delimiter"));
    };
    Ok((delimiter, content))
}

impl Parse for MacroRules {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse `macro_rules! macro_name`.
        custom_keyword!(macro_rules);
        input.parse::<macro_rules>()?;
        input.parse::<Token![!]>()?;
        let name: Ident = input.parse()?;

        // Parse the delimited macro rules.
        let (delimiter, content) = delimited(&input)?;
        let rules = Rule::parse_many(&content)?;

        // Require trailing semicolon after parens or brackets.
        match delimiter {
            Delimiter::Parenthesis | Delimiter::Bracket => {
                input.parse::<Token![;]>()?;
            }
            Delimiter::Brace | Delimiter::None => {}
        }

        Ok(MacroRules { name, rules })
    }
}

impl Rule {
    fn parse_many(input: ParseStream) -> Result<Vec<Self>> {
        let rules = input.parse_terminated::<Rule, Token![;]>(Rule::parse)?;
        if rules.is_empty() {
            Err(input.error("expected at least one macro rule"))
        } else {
            Ok(rules.into_iter().collect())
        }
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse the input pattern.
        let content = delimited(&input)?.1;
        let matcher = Matcher::parse_many(&content)?;

        input.parse::<Token![=>]>()?;

        // Parse the expansion tokens.
        let content = delimited(&input)?.1;
        let expansion: TokenStream = content.parse()?;

        Ok(Rule { matcher, expansion })
    }
}

impl Matcher {
    fn parse_many(input: ParseStream) -> Result<Vec<Self>> {
        let mut matchers = Vec::new();
        while !input.is_empty() {
            matchers.push(input.parse()?);
        }
        Ok(matchers)
    }
}

impl Parse for Matcher {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (delimiter, content) = delimited(&input)?;
            let content = Matcher::parse_many(&content)?;
            Ok(Matcher::Group { delimiter, content })
        } else if input.parse::<Option<Dollar>>()?.is_some() {
            if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                let content = Matcher::parse_many(&content)?;
                let separator = Separator::parse_optional(input)?;
                let repetition: Repetition = input.parse()?;
                Ok(Matcher::Repeat {
                    content,
                    separator,
                    repetition,
                })
            } else {
                let name = Ident::parse_any(input)?;
                input.parse::<Token![:]>()?;
                let fragment: Fragment = input.parse()?;
                Ok(Matcher::Fragment { name, fragment })
            }
        } else if let Some(lifetime) = input.parse()? {
            Ok(Matcher::Lifetime(lifetime))
        } else {
            match input.parse()? {
                TokenTree::Ident(ident) => Ok(Matcher::Ident(ident)),
                TokenTree::Punct(punct) => Ok(Matcher::Punct(punct)),
                TokenTree::Literal(literal) => Ok(Matcher::Literal(literal)),
                TokenTree::Group(_) => unreachable!(),
            }
        }
    }
}

impl Separator {
    fn parse_optional(input: ParseStream) -> Result<Option<Self>> {
        if input.peek(Token![*]) || input.peek(Token![+]) || input.peek(Token![?]) {
            Ok(None)
        } else {
            input.parse().map(Some)
        }
    }
}

impl Parse for Separator {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(match input.parse()? {
            TokenTree::Ident(ident) => Separator::Ident(ident),
            // FIXME: multi-character punctuation
            TokenTree::Punct(punct) => Separator::Punct(punct),
            TokenTree::Literal(literal) => Separator::Literal(literal),
            TokenTree::Group(group) => {
                return Err(Error::new(group.span(), "unexpected token"));
            }
        })
    }
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<Option<Token![*]>>()?.is_some() {
            Ok(Repetition::Repeated)
        } else if input.parse::<Option<Token![+]>>()?.is_some() {
            Ok(Repetition::AtLeastOnce)
        } else if input.parse::<Option<Token![?]>>()?.is_some() {
            Ok(Repetition::AtMostOnce)
        } else {
            Err(input.error("expected `*` or `+` or `?`"))
        }
    }
}

impl Parse for Fragment {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "ident" => Ok(Fragment::Ident),
            "path" => Ok(Fragment::Path),
            "expr" => Ok(Fragment::Expr),
            "ty" => Ok(Fragment::Ty),
            "pat" => Ok(Fragment::Pat),
            "stmt" => Ok(Fragment::Stmt),
            "block" => Ok(Fragment::Block),
            "item" => Ok(Fragment::Item),
            "meta" => Ok(Fragment::Meta),
            "tt" => Ok(Fragment::Tt),
            "vis" => Ok(Fragment::Vis),
            "literal" => Ok(Fragment::Literal),
            "lifetime" => Ok(Fragment::Lifetime),
            _ => Err(Error::new(ident.span(), "unrecognized fragment specifier")),
        }
    }
}

pub fn parse(src: &str) -> Result<MacroRules> {
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
