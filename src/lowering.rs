//! Intermediate representation of a `MacroRules` and it's transformations.
//!
//! The representation in this module is more coarse than what the parser provides,
//! yet has still more information than a diagram-node.
use std::collections::{HashSet, HashMap};
use parser;
use proc_macro2;

/// A more coarse representation of `parser::MacroRules`.
#[derive(Clone, Debug, PartialEq)]
pub struct MacroRules {
    /// This macro's name.
    pub name: String,
    /// The set of rules in this macro. Starts out as a `Choice` (one for every pattern)
    pub rules: Matcher
}

impl MacroRules {
    /// Recursivly fold common prefixes and suffixes in all rules
    pub fn foldcommontails(&mut self) {
        self.accept(&mut FoldCommonTails);
    }

    /// Replace all rules starting with dunder-literals (e.g. "__impl") with a single comment.
    pub fn remove_internal(&mut self) {
        self.accept(&mut InternalMacroRemover)
    }

    /// Collect the unique set of non-terminal symbols.
    pub fn collect_nonterminals(&mut self) -> HashMap<parser::Fragment, HashSet<String>> {
        let mut ntc = NonTerminalCollector::default();
        self.accept(&mut ntc);
        ntc.bag
    }

    /// Walk all rules using the specified visitor.
    fn accept(&mut self, visitor: &mut impl MatcherVisitor) {
        visitor.visit(&mut self.rules)
    }
}

impl From<parser::MacroRules> for MacroRules {
    fn from(m: parser::MacroRules) -> MacroRules {
        MacroRules {
            name: m.name.to_string(),
            rules: Matcher::Choice(m.rules.into_iter().map(|r| r.into()).collect())
        }
    }
}

/// A more coarse superset of `parser::Matcher`.
///
/// Some variants of `parser::Matcher` share the same representation here.
/// Variants like `Empty` or `Comment` are not produced by `parser::Matcher`
/// but due to transformations applied in this module.
// TODO We probably want to implemented `Ord` by ourselves because we can use
// it to have a visually useful sorting of elements in `::Choice` that way.
// Also "macro-internal" should alway be placed last/first.
// The derived implementation gives us at least repeatable sorting.
#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum Matcher {
    Choice(Vec<Matcher>),
    Comment(String),
    Empty,
    Group(Box<Matcher>),
    Literal(String),
    Optional(Box<Matcher>),
    Sequence(Vec<Matcher>),
    Repeat {
        content: Vec<Matcher>,
        seperator: Option<String>,
        repetition: parser::Repetition,
    },
    NonTerminal {
        name: String,
        fragment: parser::Fragment,
    },
}

impl Matcher {
    fn delimiter_to_str(delimiter: proc_macro2::Delimiter) -> Option<(&'static str, &'static str)> {
        match delimiter {
            proc_macro2::Delimiter::None => None,
            proc_macro2::Delimiter::Brace => Some(("{", "}")),
            proc_macro2::Delimiter::Parenthesis => Some(("(", ")")),
            proc_macro2::Delimiter::Bracket => Some(("[", "]")),
        }
    }
}

impl From<parser::Rule> for Matcher {
    fn from(r: parser::Rule) -> Matcher {
        Matcher::Sequence(r.matcher.into_iter().map(|m| m.into()).collect())
    }
}

impl From<parser::Matcher> for Matcher {
    fn from(m: parser::Matcher) -> Matcher {
        match m {
            parser::Matcher::Punct(p) => Matcher::Literal(p.to_string()),
            parser::Matcher::Ident(i) => Matcher::Literal(i.to_string()),
            parser::Matcher::Literal(l) => Matcher::Literal(l.to_string()),
            parser::Matcher::Group { delimiter, content } => {
                // We actually keep Groups as the parser sees them, representing them
                // visually as boxed (literally "boxed", like a rectangle of pixels) sequence.
                // This has two effects, both of which probably desireable:
                //  * Groups are represented as a visually bound sequence of (nested) elements.
                //  * The optimizer does not peek into a group but sees it as a whole.
                //    This means we never tear apart a group while merging common tails.
                //    While one might consider this correct-ish, it may prevent a *lot*
                //    of folding if the macro is very large.
                //  Maybe add a knob? Maybe this is superficial?
                let mut v = content.into_iter().map(|m| m.into()).collect::<Vec<_>>();
                if let Some((b, e)) = Matcher::delimiter_to_str(delimiter) {
                    v.insert(0, Matcher::Literal(b.to_owned()));
                    v.push(Matcher::Literal(e.to_owned()));
                }
                if v.len() == 1 {
                    Matcher::Group(Box::new(v.remove(0)))
                } else {
                    Matcher::Group(Box::new(Matcher::Sequence(v)))
                }
            },
            parser::Matcher::Repeat { content, separator, repetition } =>
                Matcher::Repeat { content: content.into_iter().map(|m| m.into()).collect(),
                                  seperator: separator.map(|s| match s {
                                      parser::Separator::Punct(p) => p.to_string(),
                                      parser::Separator::Literal(l) => l.to_string(),
                                      parser::Separator::Ident(i) => i.to_string(),
                                  }),
                                  repetition
                                  },
            parser::Matcher::Fragment { name, fragment } =>
                Matcher::NonTerminal { name: name.to_string(),
                                       fragment }
        }
    }
}

/// A simple Visitor to walk `Matcher`.
pub trait MatcherVisitor {
    fn visit(&mut self, &mut Matcher);

    fn visit_children(&mut self, m: &mut Matcher) {
        match m {
            Matcher::Empty
                | Matcher::Comment(_)
                | Matcher::Literal(_)
                | Matcher::NonTerminal { .. } => {},
            Matcher::Sequence(v) => {
                for m in v.iter_mut() {
                    self.visit(m)
                }
            },
            Matcher::Group(m) => {
                self.visit(m)
            },
            Matcher::Choice(v) => {
                for m in v.iter_mut() {
                    self.visit(m)
                }
            },
            Matcher::Optional(m) => {
                self.visit(m)
            },
            Matcher::Repeat { ref mut content, .. } => {
                for m in content.iter_mut() {
                    self.visit(m);
                }
            },
        }
    }
}

/// Walks a `Matcher`-tree and collects all NonTerminals as their Name/Fragment-combinations
#[derive(Clone, Debug)]
pub struct NonTerminalCollector {
    pub bag: HashMap<parser::Fragment, HashSet<String>>
}

impl Default for NonTerminalCollector {
    fn default() -> Self {
        NonTerminalCollector { bag: HashMap::new() }
    }
}

impl MatcherVisitor for NonTerminalCollector {
    fn visit(&mut self, m: &mut Matcher) {
        match m {
            Matcher::NonTerminal { name, fragment } => {
                self.bag.entry(fragment.clone())
                    .or_insert_with(Default::default)
                    .insert(name.clone());
            },
            other => { self.visit_children(other) }
        }
    }
}

/// Replaces macro-variants which start with certain literals with a `macro-internal`-comment.
pub struct InternalMacroRemover;

impl InternalMacroRemover {
    fn first_literal<'a>(&self, m: &'a Matcher) -> Option<&'a str> {
        match m {
            Matcher::Choice(_) => None,
            Matcher::Comment(_) => None,
            Matcher::Empty => None,
            Matcher::Group(m) => self.first_literal(m),
            Matcher::Literal(s) => Some(s),
            Matcher::NonTerminal { .. } => None,
            Matcher::Optional(_) => None,
            Matcher::Repeat { content, .. } => content.first().and_then(|m| self.first_literal(m)),
            Matcher::Sequence(v) => v.first().and_then(|m| self.first_literal(m)),
        }
    }

    fn is_internal(&self, literal: &str) -> bool {
        // Maybe more ?! Maybe none at all? This is effectivly a #[doc(hidden)] by convention,
        // which may be bad...
        ["__", "@"][..].iter().any(|dunder| literal.starts_with(dunder))
    }
}

impl MatcherVisitor for InternalMacroRemover {
    fn visit(&mut self, m: &mut Matcher) {
        if let Matcher::Choice(rules) = m {
            if rules.iter().filter_map(|e| self.first_literal(e)).any(|l| self.is_internal(l)) {
                rules.retain(|e| self.first_literal(e).map_or(true, |l| !self.is_internal(l)));
                rules.push(Matcher::Comment("Macro-internal rules omitted".to_owned()));
            }
        }
        self.visit_children(m);
    }
}

/// Fold common prefixes and suffixes in a `Matcher::Choice`.
///
/// If multiple rules have a common prefix and/or suffix, these tails are
/// to be passed inevitably during invocation. We can therefor pull those out
/// of the `Matcher::Choice` and form a `Matcher::Sequence` using the common
/// prefix, the common suffix and the remaining `Choice`.
struct FoldCommonTails;

impl FoldCommonTails {
    // There is quite some complexity involved here because it's not obvious
    // which optimization is the most valuable one. This might be it.
    // What we do here is
    // 1. Determine those rules which share a common prefix (at least one common item)
    // 2. Determine those rules which share a common suffix
    // 3. Considering only the largest groups with at least one common item
    //    (would fold the most rules) determine the longest common prefix/suffix
    // 4. Select the (prefix, suffix)-tuple with largest number of items.
    // 5. The remaining parts might have common tails again, so recurse.
    // 5. Glue the common prefix, the folded core and the common suffix.
    // 6. Remove those rules from the original `Choice`, which are now represented by the above.
    // 7. Select the next best candidate and repeat until there are no more common tails.
    /// Determine the longest common prefix/suffix and the rules which share these.
    fn mostcommongroups<'a>(tails: HashMap<&Matcher, Vec<&'a Vec<Matcher>>>) -> Option<(Vec<&'a Vec<Matcher>>, &'a [Matcher], &'a [Matcher])>{
        let max_size = tails.values()
            .map(|g| g.len())
            .max()
            .unwrap_or_default();
        if max_size <= 1 {
            return None
        }

        tails.into_iter().filter_map(|(_k, v)| if v.len() == max_size { Some(v) } else { None }).map(|group| {
            // Figure out the longest prefix common to all
            let mut lcp = &group[0][0..group[0].iter().zip(group[1]).take_while(|(a, b)| a == b).count()];
            for s in group.iter().skip(2) {
                if lcp.is_empty() {
                    break;
                }
                lcp = &lcp[0..lcp.iter().zip(s.iter()).take_while(|(a, b)| a == b).count()];
            }
            // Figure out the longest suffix common to all
            // Take care never to overlap the prefix, which we favor implicitly
            let mut lcs = &group[0][group[0].len() - group[0].iter().skip(lcp.len()).rev().zip(group[1].iter().skip(lcp.len()).rev()).take_while(|(a, b)| a == b).count()..];
            for s in group.iter().skip(2) {
                if lcs.is_empty() {
                    break;
                }
                lcs = &lcs[lcs.len() - lcs.iter().rev().zip(s.iter().rev()).take_while(|(a, b)| a == b).count()..];
            }
            (group, lcp, lcs)
        }).max_by_key(|(_group, lcp, lcs)| lcp.len() + lcs.len())
    }

    /// Perform one step in folding a `Matcher::Choice`.
    fn mostcommontails_core(inp: &[Matcher]) -> Option<(Vec<usize>, Matcher)> {

        // Determine rules with at lest one common prefix/suffix
        let mut prefixes = HashMap::new();
        let mut suffixes = HashMap::new();
        for i in inp {
            if let Matcher::Sequence(i) = i {
                if let Some(n) = i.first() {
                    prefixes.entry(n).or_insert_with(Vec::new).push(i);
                }
                if let Some(n) = i.last() {
                    suffixes.entry(n).or_insert_with(Vec::new).push(i);
                }
            }
        }

        // If there is a common prefix but no common suffix at all, chose the prefix; vice versa
        // applies. If both are there, chose the one with the longest common tails, favoring the prefix
        let group = match (Self::mostcommongroups(prefixes), Self::mostcommongroups(suffixes)) {
            (None, None) => {
                // Neither the most common prefix or suffix yield any more groups. We are done
                return None;
            },
            (Some(p), None) => p,
            (None, Some(s)) => s,
            (Some(p), Some(s)) => if p.0.len() >= s.0.len() { p } else { s },
        };

        // The merged sequence is made from the common prefix, the uncommon core and the common suffix
        let mut new_seq = Vec::new();

        // The common prefix
        new_seq.extend_from_slice(group.1);

        // The core
        let mut core = group.0.iter().filter_map(|s| {
            let c = &s[group.1.len()..s.len() - group.2.len()];
            //println!("{} {} {}: {}", group.1.len(), s.len(), group.2.len(), s.len() - group.2.len() - group.1.len());
            if c.is_empty() {
                //println!("ad");
                // TODO skip this Element and construct an Optional(core)
                //Some(Matcher::Empty)
                None
            } else if c.len() == 1 && false {
                // TODO Remove the `false`, this causes a panic downstairs
                Some(c[0].clone())
            } else {
                Some(Matcher::Sequence(c.to_vec()))
            }
        }).collect::<Vec<_>>();
        // Recursivly fold this
        Self::mostcommontails(&mut core);
        // The exact order currently depends on the auto-derived implementation, this is only here to
        // have *some* repeatable ordering.
        core.sort();
        if group.0.iter().any(|s| s.len() - group.2.len() - group.1.len() == 0) {
            new_seq.push(Matcher::Optional(Box::new(Matcher::Choice(core))));
        } else {
            new_seq.push(Matcher::Choice(core));
        }

        // The common suffix
        new_seq.extend_from_slice(group.2);

        // We need to let go of the borrows into the groups, so lets collect their indexes
        // There is some better way to arrange things than that...
        let mut indices = group.0.into_iter()
            .filter_map(|s| {
                inp.iter().position(|n| {
                    match n {
                        Matcher::Sequence(t) => {
                            s == t
                        },
                        // TODO What to do here? After all the panic speaks the truth
                        _ => false,
                        //u => panic!("There should be only Sequences here, got {:?}", u),
                    }
                })})
            .collect::<Vec<usize>>();

        // guarantees that .swap_remove() can remove by index without re-arrangement
        indices.sort_by(|a, b| a.cmp(b).reverse());

        Some((indices, Matcher::Sequence(new_seq)))
    }

    fn mostcommontails(inp: &mut Vec<Matcher>) {
        // With every iteration, the best available candidate gets folded.
        // Repeat until there is nothing we can do at all.
        while let Some((indices, new_seq)) = Self::mostcommontails_core(&inp) {
            indices.into_iter().for_each(|idx| drop(inp.swap_remove(idx)));
            inp.push(new_seq);
        }
    }
}

impl MatcherVisitor for FoldCommonTails {
    fn visit(&mut self, m: &mut Matcher) {
        if let Matcher::Choice(rules) = m {
            Self::mostcommontails(rules)
        }
        self.visit_children(m);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! mr { ($r:expr) => { MacroRules { name: "Test".to_owned(), rules: Matcher::Choice($r) } } }
    macro_rules! opt { ($o:expr) => { Matcher::Optional(Box::new($o)) } }
    macro_rules! lit { ($t:expr) => { Matcher::Literal($t.to_owned()) } }
    macro_rules! seq { ($($r:expr),*) => { Matcher::Sequence(vec![$($r,)+]) } }
    macro_rules! lseq { ($($r:expr),*) => { seq!($(lit!($r)),+) } }
    macro_rules! cho { ($($r:expr),*) => { Matcher::Choice(vec![$($r,)+]) } }
    macro_rules! cmt { ($t:expr) => { Matcher::Comment($t.to_owned()) } }
    macro_rules! rpt { ($($c:expr),*) => { Matcher::Repeat { content: vec![$($c,)+],
                                                             seperator: None,
                                                             repetition: parser::Repetition::AtLeastOnce } } }
    macro_rules! nonterm {
        ($t:expr, $v:expr) => { Matcher::NonTerminal { name: $t.to_owned(), fragment: $v } };
        ($t:expr) => { nonterm!($t, parser::Fragment::Ident) }
    }


    macro_rules! test_fold {
        ($name:ident, $inp:expr, $exp:expr) => {
            #[test]
            fn $name() {
                let mut mr = mr!($inp);
                mr.foldcommontails();
                assert_eq!(mr.rules, $exp);
            }
        }
    }

    // TODO the seq!(lit!("A")) // "B" should not be a seq but just a lit
    test_fold!(fold_simple,
               vec![ lseq!("A", "X", "C"), lseq!("A", "Y", "C"), ],
               cho!( seq!(lit!("A"), cho!(seq!(lit!("X")), seq!(lit!("Y"))), lit!("C")) )
               );


    #[test]
    fn remove_interals() {
        let rules = vec![
            lit!("Not internal!"),
            lit!("__impl internal"),
            seq!(lit!("@internal")),
            seq!(cmt!("also not internal"), lit!("__or is it")),
            rpt!(lit!("__self")),
        ];
        let mut mr = mr!(rules);

        mr.remove_internal();
        println!("{:#?}", mr);

        assert_eq!(mr, mr!(vec![lit!("Not internal!"),
                                seq!(cmt!("also not internal"),
                                     lit!("__or is it")),
                                cmt!("Macro-internal rules omitted") ]));
    }

    #[test]
    fn collect_nonterminals() {
        let rules = vec![
            seq![nonterm!["Foo", parser::Fragment::Ty ]],
            opt!(cho!(nonterm!("Bar", parser::Fragment::Ty))),
            rpt!(nonterm!("Foobar"))
        ];
        let mut mr = mr!(rules);

        let nonterminals = mr.collect_nonterminals();
        println!("{:#?}", nonterminals);

        assert_eq!(nonterminals[&parser::Fragment::Ty], ["Foo".to_owned(), "Bar".to_owned()].into_iter().cloned().collect());
        assert_eq!(nonterminals[&parser::Fragment::Ident], ["Foobar".to_owned()].into_iter().cloned().collect());
    }
}
