//! Transform a `lowering::MacroRules`-tree into a `railroad::Diagram`

use std::collections;

use railroad;
use lowering;
use parser;

/// The default CSS used for macro-diagrams
pub const CSS: &str = r#"
    svg.railroad .fragment_ty > rect {
        fill: red;
    }
    svg.railroad .fragment_ident > rect {
        fill: orange;
    }

    svg.railroad .fragment_path > rect {
        fill: green;
    }

    svg.railroad .fragment_expr > rect {
        fill: yellow;
    }

    svg.railroad .fragment_pat > rect {
        fill: deepskyblue;
    }

    svg.railroad .fragment_stmt > rect {
        fill: aqua;
    }

    svg.railroad .fragment_block > rect {
        fill: aquamarine;
    }

    svg.railroad .fragment_item > rect {
        fill: thistle;
    }

    svg.railroad .fragment_meta > rect {
        fill: violet;
    }

    svg.railroad .fragment_tt > rect {
        fill: turquoise;
    }

    svg.railroad .fragment_vis > rect {
        fill: gold;
    }

    svg.railroad .fragment_literal > rect {
        fill: salmon;
    }

    svg.railroad .fragment_lifetime > rect {
        fill: teal;
    }

    svg.railroad .legend {
        transform: scale(0.85);
        transform-origin: bottom left;
        /*transform-box: fill-box;*/
    }

    svg.railroad .legend > rect {
        stroke: black;
        stroke-width: 0.5px;
        stroke-dasharray: 2px;
        fill: rgba(90, 90, 90, .1)
    }

    svg.railroad .legend .comment:first-child {
        text-decoration: underline;
    }
"#;

// TODO The transform causes problems with transform-box...

/// A simple box with no connectors (unlike LabeledBox).
///
/// Used for legend below the main diagram.
#[derive(Debug)]
struct Container<T: railroad::RailroadNode> {
    inner: T,
    padding: i64,
    attributes: collections::HashMap<String, String>
}

impl<T: railroad::RailroadNode> Container<T> {
    fn new(inner: T, padding: i64) -> Self {
        Container { inner, padding, attributes: collections::HashMap::new() }
    }

    pub fn attr(&mut self, key: String) -> collections::hash_map::Entry<String, String> {
        self.attributes.entry(key)
    }
}

impl<T: railroad::RailroadNode> railroad::RailroadNode for Container<T> {
    fn entry_height(&self) -> i64 { self.inner.entry_height() + self.padding }
    fn height(&self) -> i64 { self.inner.height() + self.padding*2 }
    fn width(&self) -> i64 { self.inner.width() + self.padding*2 }
    fn draw(&self, x: i64, y: i64) -> railroad::svg::Element {
        railroad::svg::Element::new("g")
            .add(railroad::svg::Element::new("rect")
                 .set("x", x)
                 .set("y", y)
                 .set("height", self.height())
                 .set("width", self.width()))
            .add(self.inner.draw(x + self.padding, y + self.padding))
            .set_all(self.attributes.iter())
            .debug("Container", x, y, self)
    }
}

/// Given a `MacroRules`, generate the legend containing all non-terminal symbols
///
/// Returns `None` if there are no non-terminal symbols at all.
fn create_legend(tree: &mut lowering::MacroRules) -> Option<impl railroad::RailroadNode> {
    // Collect nonterminals and apply *some* sorting to have repeatable ordering
    let mut bag = tree.collect_nonterminals().into_iter().collect::<Vec<_>>();
    if bag.is_empty() {
        return None;
    }
    bag.sort_unstable_by(|(frag1, _), (frag2, _)| frag1.cmp(frag2));

    let mut lines = railroad::VerticalGrid::new(vec![]);
    lines.push(Box::new(railroad::Comment::new("Non-terminal patterns used by this macro:".to_owned())));

    for (fragment, names) in bag {
        let mut items = railroad::HorizontalGrid::new(vec![]);
        let explanation = match (&fragment, names.len() > 1) {
            (parser::Fragment::Block, false) => "is a block.",
            (parser::Fragment::Block, true) => "are blocks.",
            (parser::Fragment::Expr, false) => "is an expressions.",
            (parser::Fragment::Expr, true) => "are expressions.",
            (parser::Fragment::Ident, false) => "is an identifier.",
            (parser::Fragment::Ident, true) => "are identifiers.",
            (parser::Fragment::Item, false) => "is an item.",
            (parser::Fragment::Item, true) => "are items.",
            (parser::Fragment::Lifetime, false) => "is a lifetime.",
            (parser::Fragment::Lifetime, true) => "are lifetimes.",
            (parser::Fragment::Meta, false) => "is a meta-item.",
            (parser::Fragment::Meta, true) => "are meta-items.",
            (parser::Fragment::Pat, false) => "is a pattern.",
            (parser::Fragment::Pat, true) => "are patterns.",
            (parser::Fragment::Path, false) => "is a path.",
            (parser::Fragment::Path, true) => "are paths.",
            (parser::Fragment::Stmt, false) => "is a statement.",
            (parser::Fragment::Stmt, true) => "are statements.",
            (parser::Fragment::Tt, false) => "is a token tree.",
            (parser::Fragment::Tt, true) => "are token trees.",
            (parser::Fragment::Ty, false) => "is a type.",
            (parser::Fragment::Ty, true) => "are types.",
            (parser::Fragment::Vis, false) => "is a visibility annotation.",
            (parser::Fragment::Vis, true) => "are visibility annotations.",
            (parser::Fragment::Literal, false) => "is a literal constant.",
            (parser::Fragment::Literal, true) => "are literal constants.",
        };
        let mut names = names.into_iter().collect::<Vec<_>>();
        names.sort_unstable();
        for name in names {
            let m = lowering::Matcher::NonTerminal { name, fragment: fragment.clone() };
            items.push(into_primitive(m));
        }
        items.push(Box::new(railroad::Comment::new(explanation.to_owned())));
        lines.push(Box::new(items));
    }
    let mut legend = Container::new(lines, 6);
    legend.attr("class".to_owned())
        .or_insert_with(Default::default)
        .push_str(" legend");
    Some(legend)
}

pub fn into_diagram(mut mr: lowering::MacroRules, legend: bool) -> railroad::Diagram<Box<railroad::RailroadNode>> {

    let legend = if legend {
        create_legend(&mut mr)
    } else {
        None
    };

    let seq = railroad::Sequence::new(vec![
        Box::new(railroad::SimpleStart),
        Box::new(into_primitive(mr.rules)),
        Box::new(railroad::SimpleEnd)
    ]);

    match legend {
        None => railroad::Diagram::new(Box::new(seq)),
        Some(l) => {
            let combined = railroad::VerticalGrid::new(vec![Box::new(seq), Box::new(l)]);
            railroad::Diagram::new(Box::new(combined))
        }
    }
}

/// Shorthand to add a `<style>` containing the default CSS to a diagram-element.
///
/// Should be called with the main diagram-element.
pub fn add_default_css<T: railroad::RailroadNode>(dia: &mut railroad::Diagram<T>) {
    dia.add_element(railroad::svg::Element::new("style")
                        .set("type", "text/css")
                        .text(CSS));
}

fn fragment_to_class(f: &parser::Fragment) -> &'static str {
    match f {
        parser::Fragment::Ty => " fragment_ty",
        parser::Fragment::Ident => " fragment_ident",
        parser::Fragment::Path => " fragment_path",
        parser::Fragment::Expr => " fragment_expr",
        parser::Fragment::Pat => " fragment_pat",
        parser::Fragment::Stmt => " fragment_stmt",
        parser::Fragment::Block => " fragment_block",
        parser::Fragment::Item => " fragment_item",
        parser::Fragment::Meta => " fragment_meta",
        parser::Fragment::Tt => " fragment_tt",
        parser::Fragment::Vis => " fragment_vis",
        parser::Fragment::Literal => " fragment_literal",
        parser::Fragment::Lifetime => " fragment_lifetime"
    }
}

/// Convert a Matcher into a diagram-element.
fn into_primitive(m: lowering::Matcher) -> Box<railroad::RailroadNode> {
    match m {
        lowering::Matcher::Group(m) => Box::new(railroad::LabeledBox::without_label(into_primitive(*m))),
        lowering::Matcher::Empty => Box::new(railroad::Empty),
        lowering::Matcher::Comment(s) => Box::new(railroad::Comment::new(s)),
        lowering::Matcher::Optional(o) => Box::new(railroad::Optional::new(into_primitive(*o))),
        lowering::Matcher::Choice(s) => {
            Box::new(railroad::Choice::new(s.into_iter().map(into_primitive).collect()))
        },
        lowering::Matcher::Sequence(s) => {
            Box::new(railroad::Sequence::new(s.into_iter().map(into_primitive).collect()))
        },
        lowering::Matcher::Literal(s) => Box::new(railroad::Terminal::new(s)),
        lowering::Matcher::Repeat { content, seperator, repetition } => {
            let seperator: Box<railroad::RailroadNode> = match seperator {
                Some(s) => Box::new(railroad::Terminal::new(s)),
                None => Box::new(railroad::Empty)
            };
            let main = Box::new(railroad::Sequence::new(content.into_iter().map(into_primitive).collect()));
            match repetition {
                parser::Repetition::AtMostOnce => {
                    Box::new(railroad::Optional::new(main))
                },
                parser::Repetition::AtLeastOnce => {
                    Box::new(railroad::Repeat::new(main, seperator))
                },
                parser::Repetition::Repeated => {
                    let r = Box::new(railroad::Repeat::new(main, seperator));
                    Box::new(railroad::Optional::new(r))
                }
            }
        },
        lowering::Matcher::NonTerminal { name, fragment } => {
            let mut nonterm = railroad::NonTerminal::new(name);
            nonterm.attr("class".to_owned())
                .or_insert_with(Default::default)
                .push_str(fragment_to_class(&fragment));
            Box::new(nonterm)
        }
    }
}
