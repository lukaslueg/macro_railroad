//! Parses a raw `macro_rules!`-string.

use proc_macro2::{Delimiter, Ident, Literal, Spacing, TokenStream, TokenTree};

use syn::Lifetime;
use syn::ext::IdentExt;
use syn::parse::{Error, Parse, ParseBuffer, ParseStream, Result};
use syn::token::{Brace, Bracket, Dollar, Paren};

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
    Punct(String),
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
    Punct(String),
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
    PatParam,
    Stmt,
    Block,
    Item,
    Meta,
    Tt,
    Vis,
    Literal,
    Lifetime,
}

fn delimited(input: ParseStream<'_>) -> Result<(Delimiter, ParseBuffer<'_>)> {
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
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        // Parse `macro_rules! macro_name`.
        custom_keyword!(macro_rules);
        input.parse::<macro_rules>()?;
        input.parse::<Token![!]>()?;
        let name: Ident = input.parse()?;

        // Parse the delimited macro rules.
        let (delimiter, content) = delimited(input)?;
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
    fn parse_many(input: ParseStream<'_>) -> Result<Vec<Self>> {
        let rules = input.parse_terminated(Rule::parse, Token![;])?;
        if rules.is_empty() {
            Err(input.error("expected at least one macro rule"))
        } else {
            Ok(rules.into_iter().collect())
        }
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        // Parse the input pattern.
        let content = delimited(input)?.1;
        let matcher = Matcher::parse_many(&content)?;

        input.parse::<Token![=>]>()?;

        // Parse the expansion tokens.
        let content = delimited(input)?.1;
        let expansion: TokenStream = content.parse()?;

        Ok(Rule { matcher, expansion })
    }
}

impl Matcher {
    fn parse_many(input: ParseStream<'_>) -> Result<Vec<Self>> {
        let mut matchers = Vec::new();
        while !input.is_empty() {
            matchers.push(input.parse()?);
        }
        Ok(matchers)
    }
}

impl Parse for Matcher {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (delimiter, content) = delimited(input)?;
            let content = Matcher::parse_many(&content)?;
            Ok(Matcher::Group { delimiter, content })
        } else if input.parse::<Option<Dollar>>()?.is_some() {
            if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                let content = Matcher::parse_many(&content)?;
                let separator = Separator::parse_optional(input)?;
                let repetition: Repetition = input.parse()?;
                if repetition == Repetition::AtMostOnce && separator.is_some() {
                    return Err(
                        input.error("the `?` macro repetition operator does not take a separator")
                    );
                }
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
        } else if Separator::peek_punct(input)?.is_some() {
            Ok(Matcher::Punct(Separator::parse_punct(input)?))
        } else {
            match input.parse()? {
                TokenTree::Ident(ident) => Ok(Matcher::Ident(ident)),
                TokenTree::Literal(literal) => Ok(Matcher::Literal(literal)),
                TokenTree::Punct(_) => unreachable!(),
                TokenTree::Group(_) => unreachable!(),
            }
        }
    }
}

impl Separator {
    fn is_repetition_operator(token: &str) -> bool {
        matches!(token, "*" | "+" | "?")
    }

    fn punct_token_len(run: &str) -> Option<usize> {
        [3, 2, 1].into_iter().find(|&len| {
            len <= run.len()
                && matches!(
                    &run[..len],
                    "<<="
                        | ">>="
                        | "..."
                        | "..="
                        | "<="
                        | "=="
                        | "!="
                        | ">="
                        | "&&"
                        | "||"
                        | "<<"
                        | ">>"
                        | "+="
                        | "-="
                        | "*="
                        | "/="
                        | "%="
                        | "^="
                        | "&="
                        | "|="
                        | ".."
                        | "::"
                        | "->"
                        | "<-"
                        | "=>"
                        | "="
                        | "<"
                        | ">"
                        | "!"
                        | "~"
                        | "+"
                        | "-"
                        | "*"
                        | "/"
                        | "%"
                        | "^"
                        | "&"
                        | "|"
                        | "@"
                        | "."
                        | ","
                        | ";"
                        | ":"
                        | "#"
                        | "$"
                        | "?"
                )
        })
    }

    fn peek_punct(input: ParseStream<'_>) -> Result<Option<String>> {
        if input.is_empty() {
            return Ok(None);
        }

        let fork = input.fork();
        let mut punct = match fork.parse()? {
            TokenTree::Punct(punct) => punct,
            _ => return Ok(None),
        };

        let mut run = punct.as_char().to_string();
        while punct.spacing() == Spacing::Joint {
            punct = match fork.parse() {
                Ok(TokenTree::Punct(next)) => next,
                Ok(_) | Err(_) => break,
            };
            run.push(punct.as_char());
        }

        Ok(Self::punct_token_len(&run).map(|len| run[..len].to_owned()))
    }

    fn parse_punct(input: ParseStream<'_>) -> Result<String> {
        let punct = Self::peek_punct(input)?.ok_or_else(|| input.error("expected punctuation"))?;
        for _ in 0..punct.len() {
            match input.parse()? {
                TokenTree::Punct(_) => {}
                _ => unreachable!(),
            }
        }
        Ok(punct)
    }

    fn parse_optional(input: ParseStream<'_>) -> Result<Option<Self>> {
        match Self::peek_punct(input)? {
            Some(punct) if Self::is_repetition_operator(&punct) => Ok(None),
            _ => input.parse().map(Some),
        }
    }
}

impl Parse for Separator {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(match Self::peek_punct(input)? {
            Some(_) => Separator::Punct(Self::parse_punct(input)?),
            None => match input.parse()? {
                TokenTree::Ident(ident) => Separator::Ident(ident),
                TokenTree::Literal(literal) => Separator::Literal(literal),
                TokenTree::Group(group) => {
                    return Err(Error::new(group.span(), "unexpected token"));
                }
                TokenTree::Punct(_) => unreachable!(),
            },
        })
    }
}

impl Parse for Repetition {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
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
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "ident" => Ok(Fragment::Ident),
            "path" => Ok(Fragment::Path),
            "expr" => Ok(Fragment::Expr),
            "ty" => Ok(Fragment::Ty),
            "pat" => Ok(Fragment::Pat),
            "pat_param" => Ok(Fragment::PatParam),
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

/// # Errors
/// If the input fails to parse as a `macro_rules!`.
pub fn parse(src: &str) -> Result<MacroRules> {
    syn::parse_str::<MacroRules>(src)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn trailing_semicolon_is_required() {
        // If the macro is delimited by parens or brackets, there has to be
        // a trailing semicolon. While this is not a strictly useful requirement,
        // we test this so the parser does not fall behind.
        let src = r#"macro_rules! a ( (a) => { $a } );"#;
        parse(src).unwrap();
        let src = r#"macro_rules! a ( (a) => { $a } )"#;
        parse(src).expect_err("Expected missing semicolon-error");
        let src = r#"macro_rules! a [ (a) => { $a } ]"#;
        parse(src).expect_err("Expected missing semicolon-error");
        let src = r#"macro_rules! a { (a) => { $a } }"#;
        parse(src).unwrap();
    }

    #[test]
    fn parse_pat_param() {
        let src = r#"macro_rules! classify_number {
    (inspect $p:pat_param in $e:expr => $body:expr) => { ... };
    (zero) => { ... };
    (positive $e:expr, max $max:expr) => { ... };
    (negative $e:expr, max $max:expr) => { ... };
    (describe $e:expr) => { ... };
}"#;
        parse(src).unwrap();
    }

    #[test]
    fn pat_param_fragment() {
        let src = r#"macro_rules! m { ($x:pat_param) => {} }"#;
        let parsed = parse(src).unwrap();
        assert!(matches!(
            parsed.rules[0].matcher[0],
            Matcher::Fragment {
                fragment: Fragment::PatParam,
                ..
            }
        ));
    }

    #[test]
    fn qmark_repeat_disallows_separator() {
        // Issue 21
        let src = r#"macro_rules! m { ($($tt:tt)-?) => {} }"#;
        let err = parse(src).expect_err("Should not have parsed");
        assert!(err.to_string().contains("does not take a separator"));
    }

    #[test]
    fn issue8_minimal_repro_parses_as_repeat_with_colon_separator() {
        let src = r#"macro_rules! a {
    ($($m:ident)::+) => {};
}"#;
        let parsed = parse(src).unwrap();
        assert!(matches!(
            parsed.rules[0].matcher[0],
            Matcher::Repeat {
                separator: Some(Separator::Punct(ref parsed_separator)),
                repetition: Repetition::AtLeastOnce,
                ..
            } if parsed_separator == "::"
        ));
    }

    #[test]
    fn multi_character_repetition_separators() {
        // Issue 8
        let fixtures = [
            ("::", r#"macro_rules! a { ($($m:ident)::+) => {}; }"#),
            ("=>", r#"macro_rules! a { ($($m:ident)=>+) => {}; }"#),
            ("+=", r#"macro_rules! a { ($($m:ident)+=+) => {}; }"#),
            ("*=", r#"macro_rules! a { ($($m:ident)*=+) => {}; }"#),
            (">>", r#"macro_rules! a { ($($m:ident)>>+) => {}; }"#),
            (">=", r#"macro_rules! a { ($($m:ident)>=+) => {}; }"#),
            ("..=", r#"macro_rules! a { ($($m:ident)..=+) => {}; }"#),
            ("...", r#"macro_rules! a { ($($m:ident)...+) => {}; }"#),
            ("&&", r#"macro_rules! a { ($($m:ident)&&+) => {}; }"#),
        ];
        for (separator, src) in fixtures {
            let parsed = parse(src).unwrap();
            assert!(matches!(
                parsed.rules[0].matcher[0],
                Matcher::Repeat {
                    separator: Some(Separator::Punct(ref parsed_separator)),
                    repetition: Repetition::AtLeastOnce,
                    ..
                } if parsed_separator == separator
            ));
        }
    }

    #[test]
    fn equivalent_macro_punctuation_sequences_parse_equally() {
        // Issue 4
        let src = r#"macro_rules! x {
    (=> >) => {};
    (=>>) => {};
}"#;
        let parsed = parse(src).unwrap();
        assert!(matches!(
            parsed.rules[0].matcher.as_slice(),
            [Matcher::Punct(first), Matcher::Punct(second)] if first == "=>" && second == ">"
        ));
        assert!(matches!(
            parsed.rules[1].matcher.as_slice(),
            [Matcher::Punct(first), Matcher::Punct(second)] if first == "=>" && second == ">"
        ));
    }

    #[test]
    fn original_issue4_spacing_case_parses_distinctly() {
        // Issue 4
        let src = r#"macro_rules! x {
    (= >) => {};
    (=>) => {};
}"#;
        let parsed = parse(src).unwrap();
        assert!(matches!(
            parsed.rules[0].matcher.as_slice(),
            [Matcher::Punct(first), Matcher::Punct(second)] if first == "=" && second == ">"
        ));
        assert!(matches!(
            parsed.rules[1].matcher.as_slice(),
            [Matcher::Punct(first)] if first == "=>"
        ));
    }

    #[test]
    fn distinct_macro_punctuation_sequences_parse_distinctly() {
        // Issue 4
        let src = r#"macro_rules! x {
    (= >>) => {};
    (=>>) => {};
}"#;
        let parsed = parse(src).unwrap();
        assert!(matches!(
            parsed.rules[0].matcher.as_slice(),
            [Matcher::Punct(first), Matcher::Punct(second)] if first == "=" && second == ">>"
        ));
        assert!(matches!(
            parsed.rules[1].matcher.as_slice(),
            [Matcher::Punct(first), Matcher::Punct(second)] if first == "=>" && second == ">"
        ));
    }

    #[test]
    fn keywords_as_fragment_names() {
        // Issue 5
        let src = r#"macro_rules! a { ($self:ident) => { ... }; }"#;
        parse(src).unwrap();
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
$pat_param:pat_param
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
            r#"macro_rules! a {
    ($($m:ident)::+) => { ... };
}"#,
        ][..];
        for src in fixture {
            parse(src).expect(src);
        }
    }
}
