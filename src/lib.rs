// MIT License
//
// Copyright (c) 2018 Lukas Lueg (lukas.lueg@gmail.com)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
//! A library to generate syntax diagrams for Rust macros.
//!
//! ```
//! let src = r#"macro_rules! vec {
//!           	     ( $ elem : expr ; $ n : expr ) => { ... };
//!                  ( $ ( $ x : expr ) , * ) => { ... };
//!                  ( $ ( $ x : expr , ) * ) => { ... };
//!              }
//! "#;
//! let dia = macro_railroad::to_diagram(&src).expect("Failed to parse");
//! assert!(dia.starts_with("<svg"));
//! ```
//!
//! Diagrams are generated directly from code:
//!
//! 1. Parsing the `macro_rules!()`-block.
//! 0. Converting the parser-tree into an intermediate representation and applying
//! transformations as desired.
//! 0. Converting the intermediate representation into a `railroad::Diagram`.
//! Adding CSS to control the graphical representation.
//! 0. Outputting the final `SVG`.
//!
//! ```
//! let src = r#"macro_rules! vec {
//!           	     ( $ elem : expr ; $ n : expr ) => { ... };
//!                  ( $ ( $ x : expr ) , * ) => { ... };
//!                  ( $ ( $ x : expr , ) * ) => { ... };
//!              }
//! "#;
//!
//! let parsed = macro_railroad::parser::parse(&src).expect("Failed to parse");
//!
//! let mut ir: macro_railroad::lowering::MacroRules = parsed.into();
//! // Remove `__impl`--rules, which we consider macro-internal here.
//! ir.remove_internal();
//! // Fold common parts in the macro-syntax, making the diagram easier to understand.
//! ir.foldcommontails();
//! // Remove superfluous elements left from parsing and transforming.
//! ir.normalize();
//!
//! // Create a diagram, add a legend
//! let mut dia = macro_railroad::diagram::into_diagram(ir, true);
//! dia.add_default_css();
//!
//! assert!(dia.to_string().starts_with("<svg"));
//! ```
#[macro_use]
extern crate syn;
extern crate proc_macro2;
extern crate railroad;

pub mod parser;
pub mod lowering;
pub mod diagram;

/// Create a syntax diagram as an SVG from the given macro_rules!-source.
pub fn to_diagram(src: &str) -> Result<String, syn::synom::ParseError> {

    // Parser-tree, basically as rustc sees it
    let macro_rules = parser::parse(&src)?;

    // Create simplified tree
    let mut tree = lowering::MacroRules::from(macro_rules);

    // Optimize tree
    tree.remove_internal();
    tree.foldcommontails();
    tree.normalize();

    // create a diagram with a legend
    let dia = diagram::into_diagram(tree, true);

    Ok(dia.to_string())
}
