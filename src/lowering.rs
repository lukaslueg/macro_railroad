//! Intermediate representation of a `MacroRules` and it's transformations.
//!
//! The representation in this module is more coarse than what the parser provides,
//! yet has still more information than a diagram-node.
use crate::parser;
use std::collections::{BTreeMap, HashMap, HashSet};

/// A more coarse representation of `parser::MacroRules`.
#[derive(Clone, Debug, PartialEq)]
pub struct MacroRules {
    /// This macro's name.
    pub name: String,
    /// The set of rules in this macro. Starts out as a `Choice` (one for every pattern)
    pub rules: Matcher,
}

impl MacroRules {
    /// Recursivly fold common prefixes and suffixes in all rules
    pub fn foldcommontails(&mut self) {
        self.accept_mut(&mut FoldCommonTails);
    }

    /// Replace all rules starting with dunder-literals (e.g. "__impl") with a single comment.
    pub fn remove_internal(&mut self) {
        self.accept_mut(&mut InternalMacroRemover)
    }

    /// Collect the unique set of non-terminal symbols.
    pub fn collect_nonterminals(&mut self) -> HashMap<parser::Fragment, HashSet<String>> {
        let mut ntc = NonTerminalCollector::default();
        self.accept(&mut ntc);
        ntc.bag
    }

    /// Unpack all `Matcher::Group` in the tree
    pub fn ungroup(&mut self) {
        self.accept_mut(&mut Ungrouper);
    }

    /// Transmogrify the tree by removing superfluous elements.
    ///
    /// For example, a `Choice([Sequence([Terminal("Foo"), Empty])])` becomes just
    /// `Terminal("Foo")`.
    pub fn normalize(&mut self) {
        self.accept_mut(&mut Normalizer);
    }

    /// Walk all rules using the specified visitor.
    fn accept_mut(&mut self, visitor: &mut impl TransformVisitor) {
        self.rules.accept_mut(visitor);
    }

    /// Walk all rules using the specified visitor.
    fn accept(&self, visitor: &mut impl InspectVisitor) {
        self.rules.accept(visitor);
    }

    /// Tests if any `Matcher` matches the given predicate.
    pub fn any<F: FnMut(&Matcher) -> bool>(&self, predicate: F) -> bool {
        self.rules.any(predicate)
    }

    /// Tests if the given `Matcher` is found in the graph.
    pub fn contains(&self, needle: &Matcher) -> bool {
        self.rules.contains(needle)
    }
}

impl From<parser::MacroRules> for MacroRules {
    fn from(m: parser::MacroRules) -> MacroRules {
        MacroRules {
            name: m.name.to_string(),
            rules: Matcher::Choice(m.rules.into_iter().map(|r| r.into()).collect()),
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
    InternalMacroHint,
    Group(Box<Matcher>),
    Literal(String),
    Optional(Box<Matcher>),
    Sequence(Vec<Matcher>),
    Repeat {
        content: Vec<Matcher>,
        seperator: Option<String>,
    },
    NonTerminal {
        name: String,
        fragment: parser::Fragment,
    },
}

impl Matcher {
    pub fn accept_mut(&mut self, visitor: &mut impl TransformVisitor) {
        visitor.visit(self);
    }

    pub fn accept(&self, visitor: &mut impl InspectVisitor) {
        visitor.visit(self);
    }

    pub fn any<F: FnMut(&Self) -> bool>(&self, predicate: F) -> bool {
        let mut s = SearchVisitor::new(predicate);
        s.visit(self);
        s.found
    }

    pub fn contains(&self, needle: &Matcher) -> bool {
        self.any(|m| m == needle)
    }

    fn delimiter_to_str(delimiter: proc_macro2::Delimiter) -> Option<(&'static str, &'static str)> {
        match delimiter {
            proc_macro2::Delimiter::None => None,
            proc_macro2::Delimiter::Brace => Some(("{", "}")),
            proc_macro2::Delimiter::Parenthesis => Some(("(", ")")),
            proc_macro2::Delimiter::Bracket => Some(("[", "]")),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Matcher::Empty => true,
            _ => false,
        }
    }

    pub fn is_sequence(&self) -> bool {
        match self {
            Matcher::Sequence(_) => true,
            _ => false,
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
            parser::Matcher::Lifetime(l) => Matcher::Literal(l.to_string()),
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
                //  One can use the Ungrouper-Walker to unpack Groups
                let mut v = content.into_iter().map(|m| m.into()).collect::<Vec<_>>();
                if let Some((b, e)) = Matcher::delimiter_to_str(delimiter) {
                    v.insert(0, Matcher::Literal(b.to_owned()));
                    v.push(Matcher::Literal(e.to_owned()));
                }
                Matcher::Group(Box::new(Matcher::Sequence(v)))
            }
            parser::Matcher::Repeat {
                content,
                separator,
                repetition,
            } => {
                let seperator = separator.map(|s| match s {
                    parser::Separator::Punct(p) => p.to_string(),
                    parser::Separator::Literal(l) => l.to_string(),
                    parser::Separator::Ident(i) => i.to_string(),
                });
                let content = content.into_iter().map(|m| m.into()).collect();
                match repetition {
                    parser::Repetition::AtMostOnce => {
                        // The ? repetition does not take a seperator, should
                        // be a parse-error!
                        debug_assert!(seperator.is_none());
                        Matcher::Optional(Box::new(Matcher::Sequence(content)))
                    }
                    parser::Repetition::AtLeastOnce => Matcher::Repeat { content, seperator },
                    parser::Repetition::Repeated => {
                        Matcher::Optional(Box::new(Matcher::Repeat { content, seperator }))
                    }
                }
            }
            parser::Matcher::Fragment { name, fragment } => Matcher::NonTerminal {
                name: name.to_string(),
                fragment,
            },
        }
    }
}

/// A  Visitor to walk `Matcher`.
pub trait InspectVisitor {
    fn visit(&mut self, m: &Matcher);

    fn visit_children(&mut self, m: &Matcher) {
        match m {
            Matcher::Empty
            | Matcher::InternalMacroHint
            | Matcher::Comment(_)
            | Matcher::Literal(_)
            | Matcher::NonTerminal { .. } => {}
            Matcher::Sequence(content)
            | Matcher::Choice(content)
            | Matcher::Repeat { content, .. } => content.iter().for_each(|e| self.visit(e)),
            Matcher::Group(m) | Matcher::Optional(m) => self.visit(m),
        }
    }
}

/// Tests if any `Matcher` matches a given predicate.
pub struct SearchVisitor<F> {
    predicate: F,
    found: bool,
}

impl<F> SearchVisitor<F> {
    fn new(predicate: F) -> Self {
        SearchVisitor {
            predicate,
            found: false,
        }
    }
}

impl<F: FnMut(&Matcher) -> bool> InspectVisitor for SearchVisitor<F> {
    fn visit(&mut self, m: &Matcher) {
        if (self.predicate)(m) {
            self.found = true;
        } else if !self.found {
            self.visit_children(m);
        }
    }
}

/// A Visitor to walk `Matcher` and mutate it.
pub trait TransformVisitor {
    fn visit(&mut self, m: &mut Matcher);

    fn visit_children(&mut self, m: &mut Matcher) {
        match m {
            Matcher::Empty
            | Matcher::InternalMacroHint
            | Matcher::Comment(_)
            | Matcher::Literal(_)
            | Matcher::NonTerminal { .. } => {}
            Matcher::Sequence(content)
            | Matcher::Choice(content)
            | Matcher::Repeat { content, .. } => content.iter_mut().for_each(|e| self.visit(e)),
            Matcher::Group(m) | Matcher::Optional(m) => self.visit(m),
        }
    }
}

/// Unpacks all Groups in a Matcher
pub struct Ungrouper;

impl TransformVisitor for Ungrouper {
    fn visit(&mut self, m: &mut Matcher) {
        self.visit_children(m);
        *m = match ::std::mem::replace(m, Matcher::Empty) {
            // A Group is replaced by the element it contains
            Matcher::Group(b) => *b,
            // If we unpacked a Group in a Sequence, we probably end up with
            // nested Sequences. Clean this up now, so folding only sees
            // flat Sequences.
            Matcher::Sequence(v) => {
                if v.iter().any(|e| e.is_sequence()) {
                    let mut new_v = Vec::with_capacity(v.len());
                    for e in v {
                        match e {
                            Matcher::Sequence(ee) => new_v.extend(ee),
                            other => new_v.push(other),
                        }
                    }
                    Matcher::Sequence(new_v)
                } else {
                    Matcher::Sequence(v)
                }
            }
            other => other,
        }
    }
}

/// Simplifies a Matcher-tree
pub struct Normalizer;

impl Normalizer {
    fn normalize(m: &mut Matcher) {
        let mut changed;
        // Pound on this element until no more transformations are performed
        loop {
            changed = false;
            // The Matcher::Empty is just temporary, it never actually materializes
            *m = match ::std::mem::replace(m, Matcher::Empty) {
                Matcher::Optional(mut b) => {
                    if let Matcher::Optional(c) = *b {
                        // A nested Optional can be unnested
                        changed |= true;
                        b = c;
                    }
                    Matcher::Optional(b)
                }
                Matcher::Choice(mut b) => {
                    if b.is_empty() {
                        // An empty Choice is just the empty element
                        changed |= true;
                        Matcher::Empty
                    } else if b.len() == 1 {
                        // A Choice of exactly one element is that element
                        changed |= true;
                        b.pop().unwrap()
                    } else {
                        // A Choice with an Empty in it is an Optional(Choice)
                        match b
                            .iter()
                            .enumerate()
                            .filter_map(|(idx, e)| if e.is_empty() { Some(idx) } else { None })
                            .next()
                        {
                            Some(idx) => {
                                changed |= true;
                                b.remove(idx);
                                let mut new_m = Matcher::Optional(Box::new(Matcher::Choice(b)));
                                Normalizer.visit(&mut new_m);
                                new_m
                            }
                            None => Matcher::Choice(b),
                        }
                    }
                }
                Matcher::Sequence(mut b) => {
                    if b.is_empty() {
                        // An empty sequence is the empty element
                        changed |= true;
                        Matcher::Empty
                    } else if b.len() == 1 {
                        // A Sequence of exactly one element is that element
                        changed |= true;
                        b.pop().unwrap()
                    } else if b.iter().any(|e| e.is_sequence()) {
                        // A Sequence within a Sequence can be unpacked
                        changed |= true;
                        let mut new_v = Vec::with_capacity(b.len());
                        for e in b {
                            match e {
                                Matcher::Sequence(ee) => new_v.extend(ee),
                                other => new_v.push(other),
                            }
                        }
                        Matcher::Sequence(new_v)
                    } else {
                        Matcher::Sequence(b)
                    }
                }
                other => other,
            };
            if !changed {
                break;
            };
        }
    }
}

impl TransformVisitor for Normalizer {
    fn visit(&mut self, m: &mut Matcher) {
        self.visit_children(m);
        Self::normalize(m);
    }
}

/// Walks a `Matcher`-tree and collects all NonTerminals as their Name/Fragment-combinations
#[derive(Clone, Debug)]
pub struct NonTerminalCollector {
    pub bag: HashMap<parser::Fragment, HashSet<String>>,
}

impl Default for NonTerminalCollector {
    fn default() -> Self {
        NonTerminalCollector {
            bag: HashMap::new(),
        }
    }
}

impl InspectVisitor for NonTerminalCollector {
    fn visit(&mut self, m: &Matcher) {
        match m {
            Matcher::NonTerminal { name, fragment } => {
                self.bag
                    .entry(fragment.clone())
                    .or_insert_with(Default::default)
                    .insert(name.clone());
            }
            other => self.visit_children(other),
        }
    }
}

/// Replaces macro-variants which start with certain literals with a `macro-internal`-comment.
pub struct InternalMacroRemover;

impl InternalMacroRemover {
    fn first_literal(m: &Matcher) -> Option<&str> {
        match m {
            Matcher::Choice(_)
            | Matcher::InternalMacroHint
            | Matcher::Comment(_)
            | Matcher::Empty
            | Matcher::NonTerminal { .. }
            | Matcher::Optional(_) => None,
            Matcher::Group(m) => Self::first_literal(m),
            Matcher::Literal(s) => Some(s),
            Matcher::Repeat { content, .. } | Matcher::Sequence(content) => {
                content.first().and_then(Self::first_literal)
            }
        }
    }

    fn is_internal(literal: &str) -> bool {
        // Maybe more ?! Maybe none at all? This is effectivly a #[doc(hidden)] by convention,
        // which may be bad...
        ["__", "@"][..]
            .iter()
            .any(|dunder| literal.starts_with(dunder))
    }
}

impl TransformVisitor for InternalMacroRemover {
    fn visit(&mut self, m: &mut Matcher) {
        if let Matcher::Choice(rules) = m {
            if rules
                .iter()
                .filter_map(Self::first_literal)
                .any(Self::is_internal)
            {
                rules.retain(|e| Self::first_literal(e).map_or(true, |l| !Self::is_internal(l)));
                rules.push(Matcher::InternalMacroHint);
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
    fn mostcommongroups<'a>(
        tails: BTreeMap<&Matcher, Vec<&'a Vec<Matcher>>>,
    ) -> Option<(Vec<&'a Vec<Matcher>>, &'a [Matcher], &'a [Matcher])> {
        let max_size = tails.values().map(|g| g.len()).max().unwrap_or_default();
        if max_size <= 1 {
            return None;
        }

        tails
            .into_iter()
            .filter_map(|(_k, v)| if v.len() == max_size { Some(v) } else { None })
            .map(|group| {
                // Figure out the longest prefix common to all
                let mut lcp = &group[0][0..group[0]
                    .iter()
                    .zip(group[1])
                    .take_while(|(a, b)| a == b)
                    .count()];
                for s in group.iter().skip(2) {
                    if lcp.is_empty() {
                        break;
                    }
                    lcp = &lcp[0..lcp.iter().zip(s.iter()).take_while(|(a, b)| a == b).count()];
                }
                // Figure out the longest suffix common to all
                // Take care never to overlap the prefix, which we favor implicitly
                let mut lcs = &group[0][group[0].len()
                    - group[0]
                        .iter()
                        .skip(lcp.len())
                        .rev()
                        .zip(group[1].iter().skip(lcp.len()).rev())
                        .take_while(|(a, b)| a == b)
                        .count()..];
                for s in group.iter().skip(2) {
                    if lcs.is_empty() {
                        break;
                    }
                    lcs = &lcs[lcs.len()
                        - lcs
                            .iter()
                            .rev()
                            .zip(s.iter().rev())
                            .take_while(|(a, b)| a == b)
                            .count()..];
                }
                (group, lcp, lcs)
            })
            .max_by_key(|(_group, lcp, lcs)| lcp.len() + lcs.len())
    }

    /// Perform one step in folding a `Matcher::Choice`.
    fn mostcommontails_core(inp: &[Matcher]) -> Option<(Vec<usize>, Matcher)> {
        // Determine rules with at lest one common prefix/suffix
        // We use a BTreeMap in order to have deterministic ordering
        // of otherwise equal elements
        let mut prefixes = BTreeMap::new();
        let mut suffixes = BTreeMap::new();
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
        let group = match (
            Self::mostcommongroups(prefixes),
            Self::mostcommongroups(suffixes),
        ) {
            (None, None) => {
                // Neither the most common prefix or suffix yield any more groups. We are done
                return None;
            }
            (Some(p), None) => p,
            (None, Some(s)) => s,
            (Some(p), Some(s)) => {
                if p.0.len() >= s.0.len() {
                    p
                } else {
                    s
                }
            }
        };

        // The merged sequence is made from the common prefix, the uncommon core and the common suffix
        let mut new_seq = Vec::new();

        // The common prefix
        new_seq.extend_from_slice(group.1);

        // The core
        let mut core = group
            .0
            .iter()
            .map(|s| {
                let c = &s[group.1.len()..s.len() - group.2.len()];
                Matcher::Sequence(c.to_vec())
            })
            .collect::<Vec<_>>();
        // Recursivly fold this
        Self::mostcommontails(&mut core);
        // The exact order currently depends on the auto-derived implementation, this is only here to
        // have *some* repeatable ordering.
        core.sort();
        new_seq.push(Matcher::Choice(core));

        // The common suffix
        new_seq.extend_from_slice(group.2);

        // We need to let go of the borrows into the groups, so lets collect their indexes
        // There is some better way to arrange things than that...
        let mut indices = group
            .0
            .into_iter()
            .filter_map(|s| {
                inp.iter().position(|n| match n {
                    Matcher::Sequence(t) => s == t,
                    _ => false,
                })
            })
            .collect::<Vec<usize>>();

        // guarantees that .swap_remove() can remove by index without re-arrangement
        indices.sort_by(|a, b| a.cmp(b).reverse());

        let mut v = Matcher::Sequence(new_seq);
        // Ensure that we come up with a canonical representation (e.g. no Choice(Sequence(Foobar))
        // vs. Sequence(Choice(Foobar))), so that folding the next-best candidate won't have
        // to deal with nested structures it can't look into. This basically covers up the fact
        // that there is no decent impl of Eq for Matcher
        Normalizer.visit(&mut v);
        Some((indices, v))
    }

    fn mostcommontails(inp: &mut Vec<Matcher>) {
        // With every iteration, the best available candidate gets folded.
        // Repeat until there is nothing we can do at all.
        while let Some((indices, new_seq)) = Self::mostcommontails_core(&inp) {
            indices
                .into_iter()
                .for_each(|idx| drop(inp.swap_remove(idx)));
            inp.push(new_seq);
        }
    }
}

impl TransformVisitor for FoldCommonTails {
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

    macro_rules! rmr {
        ($r:expr) => {
            MacroRules {
                name: "Test".to_owned(),
                rules: $r,
            }
        };
    }
    macro_rules! mr {
        ($r:expr) => {
            rmr!(Matcher::Choice($r))
        };
    }
    macro_rules! opt {
        ($o:expr) => {
            Matcher::Optional(Box::new($o))
        };
    }
    macro_rules! lit {
        ($t:expr) => {
            Matcher::Literal($t.to_owned())
        };
    }
    macro_rules! epty {
        () => {
            Matcher::Empty
        };
    }
    macro_rules! grp {
        ($r:expr) => {
            Matcher::Group(Box::new($r))
        };
    }
    macro_rules! seq { ($($r:expr),*) => { Matcher::Sequence(vec![$($r,)+]) } }
    macro_rules! lseq { ($($r:expr),*) => { seq!($(lit!($r)),+) } }
    macro_rules! cho { ($($r:expr),*) => { Matcher::Choice(vec![$($r,)+]) } }
    macro_rules! cmt {
        ($t:expr) => {
            Matcher::Comment($t.to_owned())
        };
    }
    macro_rules! rpt { ($($c:expr),*) => { Matcher::Repeat { content: vec![$($c,)+],
                                                             seperator: None } } }
    macro_rules! nonterm {
        ($t:expr, $v:expr) => {
            Matcher::NonTerminal {
                name: $t.to_owned(),
                fragment: $v,
            }
        };
        ($t:expr) => {
            nonterm!($t, parser::Fragment::Ident)
        };
    }

    macro_rules! test_fold {
        ($name:ident, $inp:expr, $exp:expr) => {
            #[test]
            fn $name() {
                let mut mr = mr!($inp);
                mr.foldcommontails();
                assert_eq!(mr.rules, $exp);
            }
        };
    }

    test_fold!(
        fold_simple,
        vec![lseq!("A", "X", "C"), lseq!("A", "Y", "C"),],
        cho!(seq!(lit!("A"), cho!(lit!("X"), lit!("Y")), lit!("C")))
    );

    #[test]
    fn normalize_simple() {
        let mut mr = mr!(vec!(seq!(cho!(lseq!("A", "B"), epty!()), lit!("C"))));
        mr.normalize();
        assert_eq!(mr, rmr!(seq!(opt!(lseq!("A", "B")), lit!("C"))));
    }

    #[test]
    fn fold_nested_options() {
        // Issue 22
        let mut mr = rmr!(seq!(opt!(opt!(lit!("A"))), lit!("B")));
        mr.normalize();
        assert_eq!(mr, rmr!(seq!(opt!(lit!("A")), lit!("B"))));

        let mut mr = rmr!(seq!(opt!(opt!(lit!("A"))), opt!(opt!(opt!(lit!("B"))))));
        mr.normalize();
        assert_eq!(mr, rmr!(seq!(opt!(lit!("A")), opt!(lit!("B")))));
    }

    #[test]
    fn ungroup_simple() {
        let mut mr = mr!(vec!(
            lit!("A"),
            seq!(lit!("A"), grp!(seq!(lit!("B"), lseq!("C"))), lit!("D"))
        ));
        mr.ungroup();
        assert_eq!(mr, mr!(vec!(lit!("A"), lseq!("A", "B", "C", "D"))));
    }

    #[test]
    fn remove_internals() {
        let rules = vec![
            lit!("Not internal!"),
            lit!("__impl internal"),
            seq!(lit!("@internal")),
            seq!(cmt!("also not internal"), lit!("__or is it")),
            rpt!(lit!("__self")),
        ];
        let mut mr = mr!(rules);

        mr.remove_internal();

        assert_eq!(
            mr,
            mr!(vec![
                lit!("Not internal!"),
                seq!(cmt!("also not internal"), lit!("__or is it")),
                Matcher::InternalMacroHint,
            ])
        );
    }

    #[test]
    fn collect_nonterminals() {
        let rules = vec![
            seq![nonterm!["Foo", parser::Fragment::Ty]],
            opt!(cho!(nonterm!("Bar", parser::Fragment::Ty))),
            rpt!(nonterm!("Foobar")),
        ];
        let mut mr = mr!(rules);

        let nonterminals = mr.collect_nonterminals();
        println!("{:#?}", nonterminals);

        assert_eq!(
            nonterminals[&parser::Fragment::Ty],
            ["Foo".to_owned(), "Bar".to_owned()]
                .into_iter()
                .cloned()
                .collect()
        );
        assert_eq!(
            nonterminals[&parser::Fragment::Ident],
            ["Foobar".to_owned()].into_iter().cloned().collect()
        );
    }
}
