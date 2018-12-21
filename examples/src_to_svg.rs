extern crate macro_railroad;

use std::fs;
use std::io;

fn main() -> Result<(), io::Error> {
    let src = r#"macro_rules! method {
    ($name:ident<$a:ty>( $i:ty ) -> $o:ty, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$i:ty,$o:ty,$e:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$i:ty,$o:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$o:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty>( $i:ty ) -> $o:ty, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$i:ty,$o:ty,$e:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$i:ty,$o:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$o:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty>, $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty>( $i:ty ) -> $o:ty, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$i:ty,$o:ty,$e:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$i:ty,$o:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty,$o:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$a:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty>( $i:ty ) -> $o:ty, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$i:ty,$o:ty,$e:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$i:ty,$o:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty,$o:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$a:ty>, mut $self_:ident, $submac:ident!( $($args:tt)* )) => { ... };
}"#;

    // Parse the source
    let macro_rules = macro_railroad::parser::parse(&src).unwrap();

    // Convert to intermediate representation
    let mut tree = macro_railroad::lowering::MacroRules::from(macro_rules);
    // Fold common sections
    tree.foldcommontails();
    // Remove cruft left during transformation
    tree.normalize();

    // Generate a diagram with a legend, add the `railroad`- and `macro_railroad`-CSS
    let mut dia = macro_railroad::diagram::into_diagram(tree, true);
    dia.add_default_css();
    macro_railroad::diagram::add_default_css(&mut dia);

    // Write the SVG
    let fname = "examples/nom4_method.svg";
    let mut outp = fs::File::create(&fname)?;
    dia.write(&mut outp)?;

    println!("Written to `{}`", fname);
    Ok(())
}
