use htmlescape;
use macro_railroad;
use railroad;
use std::fs;
use std::io;
use std::io::Write;

pub const CSS: &str = r#"
svg.railroad {
    border: 1px solid;
}

div.examples {
    margin: 20px;
    padding: 0px 10px;
}

div.example {
    border: 1px solid;
    border-style: dashed;
    border-radius: 5px;
    padding: 15px;
    margin: 25px;
    height: fit-content;
}

div.dia {
    max-width: 100%;
    max-height: 100%;
}

div.dia.unoptimized {
    display: block;
}
div.dia.optimized {
    display: none;
}
input[type=checkbox]:checked ~ div.dia.unoptimized {
    display: none;
}
input[type=checkbox]:checked ~ div.dia.optimized {
    display: block;
}

pre.rust {
	background-color: darkslategray;
	color: moccasin;
	padding: 10px;
}"#;

fn to_diagram(
    src: &str,
) -> (
    String,
    railroad::Diagram<Box<dyn railroad::Node>>,
    railroad::Diagram<Box<dyn railroad::Node>>,
) {
    let macro_rules = macro_railroad::parser::parse(&src).expect(src);
    let tree = macro_railroad::lowering::MacroRules::from(macro_rules);

    let mut optimized_tree = tree.clone();
    optimized_tree.remove_internal();
    //optimized_tree.ungroup();
    optimized_tree.foldcommontails();
    optimized_tree.normalize();
    let name = tree.name.clone();

    let dia = macro_railroad::diagram::into_diagram(tree, true);
    let dia_opt = macro_railroad::diagram::into_diagram(optimized_tree, true);
    (name, dia, dia_opt)
}

pub fn to_example_page(
    title: &str,
    examples: &[&str],
    outp_filename: &str,
) -> Result<(), io::Error> {
    println!("Generating `{}`, written to `{}`.", title, outp_filename);
    let mut outp = fs::File::create(outp_filename)?;

    outp.write_all(b"<html><head>")?;
    outp.write_all(b"<style type=\"text/css\">")?;
    outp.write_all(railroad::DEFAULT_CSS.as_bytes())?;
    outp.write_all(macro_railroad::diagram::CSS.as_bytes())?;
    outp.write_all(CSS.as_bytes())?;
    outp.write_all(b"</style>")?;
    outp.write_all(b"</head><body>")?;

    write!(outp, "<h1>{}</h1>", htmlescape::encode_minimal(title))?;
    outp.write_all(b"<div>")?;
    let mut names = Vec::with_capacity(examples.len());
    outp.write_all(b"<div class=\"examples\">")?;
    for src in examples {
        let (name, dia, dia_opt) = to_diagram(&src);
        println!(" * `{}`", name);
        write!(
            outp,
            "<div class=\"example\" id=\"{}\">",
            htmlescape::encode_attribute(&name)
        )?;
        write!(
            outp,
            "<a href=\"#{}\">",
            htmlescape::encode_attribute(&name)
        )?;
        write!(
            outp,
            "<h3>Macro <i>`{}`</i></h3></a>",
            htmlescape::encode_minimal(&name)
        )?;
        names.push(name);
        write!(
            outp,
            "<pre class=\"rust\">{}</pre><br>",
            htmlescape::encode_minimal(src)
        )?;
        let dia_svg = dia.to_string();
        let dia_opt_svg = dia_opt.to_string();
        if dia_svg != dia_opt_svg {
            write!(outp, "<input class=\"opt_ckbox\" type=\"checkbox\" checked><label>Optimized layout</label>")?;
            write!(
                outp,
                "<div style=\"width: {}; height:auto\" class=\"dia unoptimized\">{}</div>",
                (&dia as &dyn railroad::Node).width(),
                dia_svg
            )?;
            write!(
                outp,
                "<div style=\"width: {}; height: auto\" class=\"dia optimized\">{}</div>",
                (&dia_opt as &dyn railroad::Node).width(),
                dia_opt_svg
            )?;
        } else {
            write!(
                outp,
                "<div style=\"width: {}; height: auto\" class=\"dia\">{}</div>",
                (&dia_opt as &dyn railroad::Node).width(),
                dia_opt_svg
            )?;
        }
        outp.write_all(b"</div>")?;
    }
    outp.write_all(b"</div>")?;
    outp.write_all(b"</body></html>")?;
    Ok(())
}

fn main() -> Result<(), io::Error> {
    let stdlib_examples = &[
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
    ][..];
    to_example_page(
        "Examples from the standard library",
        stdlib_examples,
        "examples/stdlib_examples.html",
    )?;

    let nom_examples = &[
        r#"macro_rules! add_return_error {
    ($i:expr, $code:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $code:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! alt {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $($rest:tt)* ) => { ... };
    (__impl $i:expr, $e:path, $($rest:tt)* ) => { ... };
    (__impl $i:expr, $e:path | $($rest:tt)*) => { ... };
    (__impl $i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => { ... };
    (__impl $i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)*) => { ... };
    (__impl $i:expr, $e:path => { $gen:expr } | $($rest:tt)*) => { ... };
    (__impl $i:expr, __end) => { ... };
    ($i:expr, $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! alt_complete {
    ($i:expr, $e:path | $($rest:tt)*) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)+) => { ... };
    ($i:expr, $e:path => { $gen:expr } | $($rest:tt)*) => { ... };
    ($i:expr, $e:path => { $gen:expr }) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr }) => { ... };
    ($i:expr, $e:path) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)*)) => { ... };
}"#,
        r#"macro_rules! apply {
    ($i:expr, $fun:expr, $($args:expr),* ) => { ... };
}"#,
        r#"macro_rules! apply_m {
    ($i:expr, $self_:ident.$method:ident, $($args:expr),* ) => { ... };
}"#,
        r#"macro_rules! bits {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! bytes {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! call {
    ($i:expr, $fun:expr) => { ... };
    ($i:expr, $fun:expr, $($args:expr),* ) => { ... };
}"#,
        r#"macro_rules! call_m {
    ($i:expr, $self_:ident.$method:ident) => { ... };
    ($i:expr, $self_:ident.$method:ident, $($args:expr),* ) => { ... };
}"#,
        r#"macro_rules! char {
    ($i:expr, $c: expr) => { ... };
}"#,
        r#"macro_rules! closure {
    ($ty:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! complete {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! cond {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $cond:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! cond_reduce {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $cond:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! cond_with_error {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $cond:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! count {
    ($i:expr, $submac:ident!( $($args:tt)* ), $count: expr) => { ... };
    ($i:expr, $f:expr, $count: expr) => { ... };
}"#,
        r#"macro_rules! count_fixed {
    ($i:expr, $typ:ty, $submac:ident!( $($args:tt)* ), $count: expr) => { ... };
    ($i:expr, $typ: ty, $f:expr, $count: expr) => { ... };
}"#,
        r#"macro_rules! dbg {
    ($i: expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:ident) => { ... };
}"#,
        r#"macro_rules! dbg_dmp {
    ($i: expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:ident) => { ... };
}"#,
        r#"macro_rules! delimited {
    ($i:expr, $submac:ident!( $($args:tt)* ), $($rest:tt)+) => { ... };
    ($i:expr, $f:expr, $($rest:tt)+) => { ... };
}"#,
        r#"macro_rules! do_parse {
    (__impl $i:expr, ( $($rest:expr),* )) => { ... };
    (__impl $i:expr, $field:ident : $submac:ident!( $($args:tt)* ) ) => { ... };
    (__impl $i:expr, $submac:ident!( $($args:tt)* ) ) => { ... };
    (__impl $i:expr, $field:ident : $submac:ident!( $($args:tt)* ) ~ $($rest:tt)* ) => { ... };
    (__impl $i:expr, $submac:ident!( $($args:tt)* ) ~ $($rest:tt)* ) => { ... };
    (__impl $i:expr, $field:ident : $e:ident ~ $($rest:tt)*) => { ... };
    (__impl $i:expr, $e:ident ~ $($rest:tt)*) => { ... };
    (__impl $i:expr, $e:ident >> $($rest:tt)*) => { ... };
    (__impl $i:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => { ... };
    (__impl $i:expr, $field:ident : $e:ident >> $($rest:tt)*) => { ... };
    (__impl $i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => { ... };
    (__impl $i:expr, $e:ident >> ( $($rest:tt)* )) => { ... };
    (__impl $i:expr, $submac:ident!( $($args:tt)* ) >> ( $($rest:tt)* )) => { ... };
    (__impl $i:expr, $field:ident : $e:ident >> ( $($rest:tt)* )) => { ... };
    (__impl $i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> ( $($rest:tt)* )) => { ... };
    (__finalize $i:expr, ( $o: expr )) => { ... };
    (__finalize $i:expr, ( $($rest:tt)* )) => { ... };
    ($i:expr, $($rest:tt)*) => { ... };
    ($submac:ident!( $($args:tt)* ) >> $($rest:tt)* ) => { ... };
    ($e:ident! >> $($rest:tt)* ) => { ... };
}"#,
        r#"macro_rules! eat_separator {
    ($i:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! eof {
    ($i:expr,) => { ... };
}"#,
        r#"macro_rules! error_node_position {
    ($input:expr, $code:expr, $next:expr) => { ... };
}"#,
        r#"macro_rules! error_position {
    ($input:expr, $code:expr) => { ... };
}"#,
        r#"macro_rules! escaped {
    (__impl $i: expr, $normal:ident!(  $($args:tt)* ), $control_char: expr, $escapable:ident!(  $($args2:tt)* )) => { ... };
    (__impl_1 $i:expr, $submac1:ident!( $($args:tt)* ), $control_char: expr, $submac2:ident!( $($args2:tt)*) ) => { ... };
    (__impl_1 $i:expr, $submac1:ident!( $($args:tt)* ), $control_char: expr, $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $control_char: expr, $($rest:tt)+) => { ... };
    ($i:expr, $f:expr, $control_char: expr, $($rest:tt)+) => { ... };
}"#,
        r#"macro_rules! escaped_transform {
    (__impl $i: expr, $normal:ident!(  $($args:tt)* ), $control_char: expr, $transform:ident!(  $($args2:tt)* )) => { ... };
    (__impl_1 $i:expr, $submac1:ident!( $($args:tt)* ), $control_char: expr, $submac2:ident!( $($args2:tt)*) ) => { ... };
    (__impl_1 $i:expr, $submac1:ident!( $($args:tt)* ), $control_char: expr, $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $control_char: expr, $($rest:tt)+) => { ... };
    ($i:expr, $f:expr, $control_char: expr, $($rest:tt)+) => { ... };
}"#,
        r#"macro_rules! exact {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! expr_opt {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! expr_res {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! fix_error {
    ($i:expr, $t:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $t:ty, $f:expr) => { ... };
}"#,
        r#"macro_rules! flat_map {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
}"#,
        r#"macro_rules! fold_many0 {
    ($i:expr, $submac:ident!( $($args:tt)* ), $init:expr, $f:expr) => { ... };
    ($i:expr, $f:expr, $init:expr, $fold_f:expr) => { ... };
}"#,
        r#"macro_rules! fold_many1 {
    ($i:expr, $submac:ident!( $($args:tt)* ), $init:expr, $f:expr) => { ... };
    ($i:expr, $f:expr, $init:expr, $fold_f:expr) => { ... };
}"#,
        r#"macro_rules! fold_many_m_n {
    ($i:expr, $m:expr, $n: expr, $submac:ident!( $($args:tt)* ), $init:expr, $f:expr) => { ... };
    ($i:expr, $m:expr, $n: expr, $f:expr, $init:expr, $fold_f:expr) => { ... };
}"#,
        r#"macro_rules! i128 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! i16 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! i32 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! i64 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! is_a {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! is_a_s {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! is_not {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! is_not_s {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! length_bytes {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! length_count {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! length_data {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! length_value {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! many0 {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! many1 {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! many_m_n {
    ($i:expr, $m:expr, $n: expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $m:expr, $n: expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! many_till {
    (__impl $i:expr, $submac1:ident!( $($args1:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac1:ident!( $($args1:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac1:ident!( $($args1:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $f:expr, $g: expr) => { ... };
}"#,
        r#"macro_rules! map {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! map_opt {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! map_res {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! map_res_err {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! method {
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
}"#,
        r#"macro_rules! named {
    (#$($args:tt)*) => { ... };
    ($name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident, $submac:ident!( $($args:tt)* )) => { ... };
    (pub(crate) $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    (pub(crate) $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub(crate) $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub(crate) $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    (pub(crate) $name:ident, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! named_args {
    (pub $func_name:ident ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    (pub $func_name:ident < 'a > ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    (pub(crate) $func_name:ident ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    (pub(crate) $func_name:ident < 'a > ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    ($func_name:ident ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    ($func_name:ident < 'a > ( $( $arg:ident : $typ:ty ),* ) < $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    (pub $func_name:ident ( $( $arg:ident : $typ:ty ),* ) < $input_type:ty, $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    ($func_name:ident ( $( $arg:ident : $typ:ty ),* ) < $input_type:ty, $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    (pub $func_name:ident < 'a > ( $( $arg:ident : $typ:ty ),* ) < $input_type:ty, $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
    ($func_name:ident < 'a > ( $( $arg:ident : $typ:ty ),* ) < $input_type:ty, $return_type:ty > , $submac:ident!( $($args:tt)* ) ) => { ... };
}"#,
        r#"macro_rules! named_attr {
    ($(#[$attr:meta])*, $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, $name:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub $name:ident, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub(crate) $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub(crate) $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub(crate) $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub(crate) $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => { ... };
    ($(#[$attr:meta])*, pub(crate) $name:ident, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! none_of {
    ($i:expr, $inp: expr) => { ... };
}"#,
        r#"macro_rules! not {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! one_of {
    ($i:expr, $inp: expr) => { ... };
}"#,
        r#"macro_rules! opt {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! opt_res {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! pair {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! parse_to {
    ($i:expr, $t:ty ) => { ... };
}"#,
        r#"macro_rules! peek {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! permutation {
    ($i:expr, $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! preceded {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! recognize {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! return_error {
    ($i:expr, $code:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $code:expr, $f:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! sep {
    ($i:expr,  $separator:path, tuple ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, pair ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, delimited ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, separated_pair ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, preceded ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, terminated ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, do_parse ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, permutation ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, alt ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, alt_complete ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, switch ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, separated_list ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, many0 ! ($($rest:tt)*) ) => { ... };
    ($i:expr,  $separator:path, many1 ! ($($rest:tt)*) ) => { ... };
    ($i:expr, $separator:path, return_error!( $($args:tt)* )) => { ... };
    ($i:expr, $separator:path, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $separator:path, $f:expr) => { ... };
}"#,
        r#"macro_rules! separated_list {
    ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! separated_list_complete {
    ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! separated_nonempty_list {
    ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! separated_nonempty_list_complete {
    ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! separated_pair {
    ($i:expr, $submac:ident!( $($args:tt)* ), $($rest:tt)+) => { ... };
    ($i:expr, $f:expr, $($rest:tt)+) => { ... };
}"#,
        r#"macro_rules! switch {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $( $($p:pat)|+ => $subrule:ident!( $($args2:tt)* ))|* ) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)*), $($rest:tt)*) => { ... };
    ($i:expr, $e:path, $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! tag {
    ($i:expr, $tag: expr) => { ... };
}"#,
        r#"macro_rules! tag_bits {
    ($i:expr, $t:ty, $count:expr, $p: pat) => { ... };
}"#,
        r#"macro_rules! tag_no_case {
    ($i:expr, $tag: expr) => { ... };
}"#,
        r#"macro_rules! tag_no_case_s {
    ($i:expr, $tag: expr) => { ... };
}"#,
        r#"macro_rules! tag_s {
    ($i:expr, $tag: expr) => { ... };
}"#,
        r#"macro_rules! take {
    ($i:expr, $count:expr) => { ... };
}"#,
        r#"macro_rules! take_bits {
    ($i:expr, $t:ty, $count:expr) => { ... };
}"#,
        r#"macro_rules! take_s {
    ($i:expr, $count:expr) => { ... };
}"#,
        r#"macro_rules! take_str {
    ( $i:expr, $size:expr ) => { ... };
}"#,
        r#"macro_rules! take_till {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_till1 {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_till1_s {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_till_s {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_until {
    ($i:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_until1 {
    ($i:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_and_consume {
    ($i:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_and_consume1 {
    ($i:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_and_consume_s {
    ($input:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_either {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_either1 {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_either_and_consume {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_either_and_consume1 {
    ($input:expr, $arr:expr) => { ... };
}"#,
        r#"macro_rules! take_until_s {
    ($input:expr, $substr:expr) => { ... };
}"#,
        r#"macro_rules! take_while {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_while1 {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_while1_s {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_while_m_n {
    ($input:expr, $m:expr, $n:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $m:expr, $n: expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! take_while_s {
    ($input:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($input:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! tap {
    ($i:expr, $name:ident : $submac:ident!( $($args:tt)* ) => $e:expr) => { ... };
    ($i:expr, $name: ident: $f:expr => $e:expr) => { ... };
}"#,
        r#"macro_rules! terminated {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! try_parse {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! tuple {
    ($i:expr, $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! u128 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! u16 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! u32 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! u64 {
    ($i:expr, $e:expr) => { ... };
}"#,
        r#"macro_rules! value {
    ($i:expr, $res:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $res:expr, $f:expr) => { ... };
    ($i:expr, $res:expr) => { ... };
}"#,
        r#"macro_rules! verify {
    (__impl $i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! wrap_sep {
    ($i:expr, $separator:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $separator:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! ws {
    ($i:expr, $($args:tt)*) => { ... };
}"#,
    ][..];
    to_example_page(
        "Examples from nom-4.1.1",
        nom_examples,
        "examples/nom_examples.html",
    )?;

    let syn_examples = &[
        r#"macro_rules! Token {
    (+) => { ... };
    (+=) => { ... };
    (&) => { ... };
    (&&) => { ... };
    (&=) => { ... };
    (@) => { ... };
    (!) => { ... };
    (^) => { ... };
    (^=) => { ... };
    (:) => { ... };
    (::) => { ... };
    (,) => { ... };
    (/) => { ... };
    (/=) => { ... };
    (.) => { ... };
    (..) => { ... };
    (...) => { ... };
    (..=) => { ... };
    (=) => { ... };
    (==) => { ... };
    (>=) => { ... };
    (>) => { ... };
    (<=) => { ... };
    (<) => { ... };
    (*=) => { ... };
    (!=) => { ... };
    (|) => { ... };
    (|=) => { ... };
    (||) => { ... };
    (#) => { ... };
    (?) => { ... };
    (->) => { ... };
    (<-) => { ... };
    (%) => { ... };
    (%=) => { ... };
    (=>) => { ... };
    (;) => { ... };
    (<<) => { ... };
    (<<=) => { ... };
    (>>) => { ... };
    (>>=) => { ... };
    (*) => { ... };
    (-) => { ... };
    (-=) => { ... };
    (_) => { ... };
    (as) => { ... };
    (auto) => { ... };
    (box) => { ... };
    (break) => { ... };
    (Self) => { ... };
    (catch) => { ... };
    (const) => { ... };
    (continue) => { ... };
    (crate) => { ... };
    (default) => { ... };
    (do) => { ... };
    (dyn) => { ... };
    (else) => { ... };
    (enum) => { ... };
    (extern) => { ... };
    (fn) => { ... };
    (for) => { ... };
    (if) => { ... };
    (impl) => { ... };
    (in) => { ... };
    (let) => { ... };
    (loop) => { ... };
    (macro) => { ... };
    (match) => { ... };
    (mod) => { ... };
    (move) => { ... };
    (mut) => { ... };
    (pub) => { ... };
    (ref) => { ... };
    (return) => { ... };
    (self) => { ... };
    (static) => { ... };
    (struct) => { ... };
    (super) => { ... };
    (trait) => { ... };
    (type) => { ... };
    (union) => { ... };
    (unsafe) => { ... };
    (use) => { ... };
    (where) => { ... };
    (while) => { ... };
    (yield) => { ... };
}"#,
        r#"macro_rules! alt {
    ($i:expr, $e:ident | $($rest:tt)*) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)+) => { ... };
    ($i:expr, $e:ident => { $gen:expr } | $($rest:tt)*) => { ... };
    ($i:expr, $e:ident => { $gen:expr }) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr }) => { ... };
    ($i:expr, $e:ident) => { ... };
    ($i:expr, $subrule:ident!( $($args:tt)*)) => { ... };
}"#,
        r#"macro_rules! braces {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! brackets {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! call {
    ($i:expr, $fun:expr $(, $args:expr)*) => { ... };
}"#,
        r#"macro_rules! cond {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $cond:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! cond_reduce {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $cond:expr) => { ... };
    ($i:expr, $cond:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! custom_keyword {
    ($i:expr, $keyword:ident) => { ... };
}"#,
        r#"macro_rules! do_parse {
    ($i:expr, ( $($rest:expr),* )) => { ... };
    ($i:expr, $e:ident >> $($rest:tt)*) => { ... };
    ($i:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => { ... };
    ($i:expr, $field:ident : $e:ident >> $($rest:tt)*) => { ... };
    ($i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => { ... };
    ($i:expr, mut $field:ident : $e:ident >> $($rest:tt)*) => { ... };
    ($i:expr, mut $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! epsilon {
    ($i:expr,) => { ... };
}"#,
        r#"macro_rules! input_end {
    ($i:expr,) => { ... };
}"#,
        r#"macro_rules! keyword {
    ($i:expr, as) => { ... };
    ($i:expr, auto) => { ... };
    ($i:expr, box) => { ... };
    ($i:expr, break) => { ... };
    ($i:expr, Self) => { ... };
    ($i:expr, catch) => { ... };
    ($i:expr, const) => { ... };
    ($i:expr, continue) => { ... };
    ($i:expr, crate) => { ... };
    ($i:expr, default) => { ... };
    ($i:expr, do) => { ... };
    ($i:expr, dyn) => { ... };
    ($i:expr, else) => { ... };
    ($i:expr, enum) => { ... };
    ($i:expr, extern) => { ... };
    ($i:expr, fn) => { ... };
    ($i:expr, for) => { ... };
    ($i:expr, if) => { ... };
    ($i:expr, impl) => { ... };
    ($i:expr, in) => { ... };
    ($i:expr, let) => { ... };
    ($i:expr, loop) => { ... };
    ($i:expr, macro) => { ... };
    ($i:expr, match) => { ... };
    ($i:expr, mod) => { ... };
    ($i:expr, move) => { ... };
    ($i:expr, mut) => { ... };
    ($i:expr, pub) => { ... };
    ($i:expr, ref) => { ... };
    ($i:expr, return) => { ... };
    ($i:expr, self) => { ... };
    ($i:expr, static) => { ... };
    ($i:expr, struct) => { ... };
    ($i:expr, super) => { ... };
    ($i:expr, trait) => { ... };
    ($i:expr, type) => { ... };
    ($i:expr, union) => { ... };
    ($i:expr, unsafe) => { ... };
    ($i:expr, use) => { ... };
    ($i:expr, where) => { ... };
    ($i:expr, while) => { ... };
    ($i:expr, yield) => { ... };
}"#,
        r#"macro_rules! many0 {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! map {
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => { ... };
    ($i:expr, $f:expr, $g:expr) => { ... };
}"#,
        r#"macro_rules! named {
    ($name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    ($name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
    (pub $name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! not {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
}"#,
        r#"macro_rules! option {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! parens {
    ($i:expr, $submac:ident!( $($args:tt)* )) => { ... };
    ($i:expr, $f:expr) => { ... };
}"#,
        r#"macro_rules! parse_quote {
    ($($tt:tt)*) => { ... };
}"#,
        r#"macro_rules! punct {
    ($i:expr, +) => { ... };
    ($i:expr, +=) => { ... };
    ($i:expr, &) => { ... };
    ($i:expr, &&) => { ... };
    ($i:expr, &=) => { ... };
    ($i:expr, @) => { ... };
    ($i:expr, !) => { ... };
    ($i:expr, ^) => { ... };
    ($i:expr, ^=) => { ... };
    ($i:expr, :) => { ... };
    ($i:expr, ::) => { ... };
    ($i:expr, ,) => { ... };
    ($i:expr, /) => { ... };
    ($i:expr, /=) => { ... };
    ($i:expr, .) => { ... };
    ($i:expr, ..) => { ... };
    ($i:expr, ...) => { ... };
    ($i:expr, ..=) => { ... };
    ($i:expr, =) => { ... };
    ($i:expr, ==) => { ... };
    ($i:expr, >=) => { ... };
    ($i:expr, >) => { ... };
    ($i:expr, <=) => { ... };
    ($i:expr, <) => { ... };
    ($i:expr, *=) => { ... };
    ($i:expr, !=) => { ... };
    ($i:expr, |) => { ... };
    ($i:expr, |=) => { ... };
    ($i:expr, ||) => { ... };
    ($i:expr, #) => { ... };
    ($i:expr, ?) => { ... };
    ($i:expr, ->) => { ... };
    ($i:expr, <-) => { ... };
    ($i:expr, %) => { ... };
    ($i:expr, %=) => { ... };
    ($i:expr, =>) => { ... };
    ($i:expr, ;) => { ... };
    ($i:expr, <<) => { ... };
    ($i:expr, <<=) => { ... };
    ($i:expr, >>) => { ... };
    ($i:expr, >>=) => { ... };
    ($i:expr, *) => { ... };
    ($i:expr, -) => { ... };
    ($i:expr, -=) => { ... };
    ($i:expr, _) => { ... };
}"#,
        r#"macro_rules! reject {
    ($i:expr,) => { ... };
}"#,
        r#"macro_rules! syn {
    ($i:expr, $t:ty) => { ... };
}"#,
        r#"macro_rules! tuple {
    ($i:expr, $($rest:tt)+) => { ... };
}"#,
        r#"macro_rules! value {
    ($i:expr, $res:expr) => { ... };
}"#,
    ][..];
    to_example_page(
        "Examples from syn-0.14",
        syn_examples,
        "examples/syn_examples.html",
    )?;

    let various_examples = &[
        r#"macro_rules! bitflags {
    (
        $(#[$outer:meta])*
        pub struct $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                const $Flag:ident = $value:expr;
            )+
        }
    ) => { ... };
    (
        $(#[$outer:meta])*
        struct $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                const $Flag:ident = $value:expr;
            )+
        }
    ) => { ... };
    (
        $(#[$outer:meta])*
        pub ($($vis:tt)+) struct $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                const $Flag:ident = $value:expr;
            )+
        }
    ) => { ... };
}"#,
        r#"macro_rules! lazy_static {
    ($(#[$attr:meta])* static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    ($(#[$attr:meta])* pub static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    ($(#[$attr:meta])* pub ($($vis:tt)+) static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    () => { ... };
}"#,
        r#"macro_rules! quickcheck {
    (@as_items $($i:item)*) => { ... };
    {
        $(
            $(#[$m:meta])*
            fn $fn_name:ident($($arg_name:ident : $arg_ty:ty),*) -> $ret:ty {
                $($code:tt)*
            }
        )*
    } => { ... };
}"#,
        r#"macro_rules! handlebars_helper {
    ($struct_name:ident: |$($name:ident: $tpe:tt),*| $body:expr ) => { ... };
    (@as_json_value $x:ident, object) => { ... };
    (@as_json_value $x:ident, array) => { ... };
    (@as_json_value $x:ident, str) => { ... };
    (@as_json_value $x:ident, i64) => { ... };
    (@as_json_value $x:ident, u64) => { ... };
    (@as_json_value $x:ident, f64) => { ... };
    (@as_json_value $x:ident, bool) => { ... };
    (@as_json_value $x:ident, null) => { ... };
}"#,
        r#"macro_rules! dlopen_external_library {
    (__struct, $structname: ident,
        $(statics: $($sname: ident: $stype: ty),+,)|*
        $(functions: $(fn $fname: ident($($farg: ty),*) -> $fret:ty),+,)|*
        $(varargs: $(fn $vname: ident($($vargs: ty),+) -> $vret: ty),+,)|*
    ) => { ... };
    (__impl, $structname: ident,
        $(statics: $($sname: ident: $stype: ty),+,)|*
        $(functions: $(fn $fname: ident($($farg: ty),*) -> $fret:ty),+,)|*
        $(varargs: $(fn $vname: ident($($vargs: ty),+) -> $vret: ty),+,)|*
    ) => { ... };
    ($structname: ident,
        $(statics: $($sname: ident: $stype: ty),+,)|*
        $(functions: $(fn $fname: ident($($farg: ty),*) -> $fret:ty),+,)|*
        $(varargs: $(fn $vname: ident($($vargs: ty),+) -> $vret: ty),+,)|*
    ) => { ... };
}"#,
        r#"macro_rules! gfx_defines {
    ($(#[$attr:meta])* vertex $name:ident {
            $( $(#[$field_attr:meta])* $field:ident : $ty:ty = $e:expr, )+
    }) => { ... };
    ($(#[$attr:meta])* constant $name:ident {
            $( $(#[$field_attr:meta])* $field:ident : $ty:ty = $e:expr, )+
    }) => { ... };
    (pipeline $name:ident {
            $( $field:ident : $ty:ty = $e:expr, )+
    }) => { ... };
    ($(#[$attr:meta])* vertex $name:ident {
            $( $(#[$field_attr:meta])* $field:ident : $ty:ty = $e:expr, )+
    } $($tail:tt)+) => { ... };
    ($(#[$attr:meta])* constant $name:ident {
            $( $(#[$field_attr:meta])* $field:ident : $ty:ty = $e:expr, )+
    } $($tail:tt)+) => { ... };
    ($keyword:ident $name:ident {
            $( $field:ident : $ty:ty = $e:expr, )+
    } $($tail:tt)+) => { ... };
}"#,
        r#"macro_rules! clap_app {
    (@app ($builder:expr)) => { ... };
    (@app ($builder:expr) (@arg ($name:expr): $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@arg $name:ident: $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@setting $setting:ident) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@attributes $($attr:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@group $name:ident => $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@group $name:ident !$ident:ident => $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@group $name:ident +$ident:ident => $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) (@subcommand $name:ident => $($tail:tt)*) $($tt:tt)*) => { ... };
    (@app ($builder:expr) ($ident:ident: $($v:expr),*) $($tt:tt)*) => { ... };
    (@group ($builder:expr, $group:expr)) => { ... };
    (@group ($builder:expr, $group:expr) (@attributes $($attr:tt)*) $($tt:tt)*) => { ... };
    (@group ($builder:expr, $group:expr) (@arg $name:ident: $($tail:tt)*) $($tt:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt) => { ... };
    (@arg ($arg:expr) $modes:tt --($long:expr) $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt --$long:ident $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt -$short:ident $($tail:tt)*) => { ... };
    (@arg ($arg:expr) (-) <$var:ident> $($tail:tt)*) => { ... };
    (@arg ($arg:expr) (+) <$var:ident> $($tail:tt)*) => { ... };
    (@arg ($arg:expr) (-) [$var:ident] $($tail:tt)*) => { ... };
    (@arg ($arg:expr) (+) [$var:ident] $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt ... $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt #{$n:expr, $m:expr} $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt * $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt !$ident:ident $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt +$ident:ident $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt {$fn_:expr} $($tail:tt)*) => { ... };
    (@as_expr $expr:expr) => { ... };
    (@arg ($arg:expr) $modes:tt $desc:tt) => { ... };
    (@arg ($arg:expr) $modes:tt $ident:ident[$($target:ident)*] $($tail:tt)*) => { ... };
    (@arg ($arg:expr) $modes:tt $ident:ident($($expr:expr)*) $($tail:tt)*) => { ... };
    (@subcommand $name:ident => $($tail:tt)*) => { ... };
    (($name:expr) => $($tail:tt)*) => { ... };
    ($name:ident => $($tail:tt)*) => { ... };
}"#,
        r#"macro_rules! convert_args {
    (keys=$kf:expr, $macro_name:ident !($($k:expr),* $(,)*)) => { ... };
    (keys=$kf:expr, values=$vf:expr, $macro_name:ident !($($k:expr),* $(,)*)) => { ... };
    (keys=$kf:expr, values=$vf:expr, $macro_name:ident !( $($k:expr => $v:expr),* $(,)*)) => { ... };
    (keys=$kf:expr, $macro_name:ident !($($rest:tt)*)) => { ... };
    (values=$vf:expr, $macro_name:ident !($($rest:tt)*)) => { ... };
    ($macro_name:ident ! $($rest:tt)*) => { ... };
}"#,
        r#"macro_rules! cfg_if {
    ($(
        if #[cfg($($meta:meta),*)] { $($it:item)* }
    ) else * else {
        $($it2:item)*
    }) => { ... };
    (
        if #[cfg($($i_met:meta),*)] { $($i_it:item)* }
        $(
            else if #[cfg($($e_met:meta),*)] { $($e_it:item)* }
        )*
    ) => { ... };
}"#,
        r#"macro_rules! enum_from_primitive {
    (
        $( #[$enum_attr:meta] )*
        enum $name:ident {
            $( $( #[$variant_attr:meta] )* $variant:ident ),+ $( = $discriminator:expr, $( $( #[$variant_two_attr:meta] )* $variant_two:ident ),+ )*
        }
    ) => {
        $( #[$enum_attr] )*
        enum $name {
            $( $( #[$variant_attr] )* $variant ),+ $( = $discriminator, $( $( #[$variant_two_attr] )* $variant_two ),+ )*
        }
        enum_from_primitive_impl! { $name, $( $variant )+ $( $( $variant_two )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        enum $name:ident {
            $( $( $( #[$variant_attr:meta] )* $variant:ident ),+ = $discriminator:expr ),*
        }
    ) => {
        $( #[$enum_attr] )*
        enum $name {
            $( $( $( #[$variant_attr] )* $variant ),+ = $discriminator ),*
        }
        enum_from_primitive_impl! { $name, $( $( $variant )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        enum $name:ident {
            $( $( #[$variant_attr:meta] )* $variant:ident ),+ $( = $discriminator:expr, $( $( #[$variant_two_attr:meta] )* $variant_two:ident ),+ )*,
        }
    ) => {
        $( #[$enum_attr] )*
        enum $name {
            $( $( #[$variant_attr] )* $variant ),+ $( = $discriminator, $( $( #[$variant_two_attr] )* $variant_two ),+ )*,
        }
        enum_from_primitive_impl! { $name, $( $variant )+ $( $( $variant_two )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        enum $name:ident {
            $( $( $( #[$variant_attr:meta] )* $variant:ident ),+ = $discriminator:expr ),+,
        }
    ) => {
        $( #[$enum_attr] )*
        enum $name {
            $( $( $( #[$variant_attr] )* $variant ),+ = $discriminator ),+,
        }
        enum_from_primitive_impl! { $name, $( $( $variant )+ )+ }
    };

    (
        $( #[$enum_attr:meta] )*
        pub enum $name:ident {
            $( $( #[$variant_attr:meta] )* $variant:ident ),+ $( = $discriminator:expr, $( $( #[$variant_two_attr:meta] )* $variant_two:ident ),+ )*
        }
    ) => {
        $( #[$enum_attr] )*
        pub enum $name {
            $( $( #[$variant_attr] )* $variant ),+ $( = $discriminator, $( $( #[$variant_two_attr] )* $variant_two ),+ )*
        }
        enum_from_primitive_impl! { $name, $( $variant )+ $( $( $variant_two )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        pub enum $name:ident {
            $( $( $( #[$variant_attr:meta] )* $variant:ident ),+ = $discriminator:expr ),*
        }
    ) => {
        $( #[$enum_attr] )*
        pub enum $name {
            $( $( $( #[$variant_attr] )* $variant ),+ = $discriminator ),*
        }
        enum_from_primitive_impl! { $name, $( $( $variant )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        pub enum $name:ident {
            $( $( #[$variant_attr:meta] )* $variant:ident ),+ $( = $discriminator:expr, $( $( #[$variant_two_attr:meta] )* $variant_two:ident ),+ )*,
        }
    ) => {
        $( #[$enum_attr] )*
        pub enum $name {
            $( $( #[$variant_attr] )* $variant ),+ $( = $discriminator, $( $( #[$variant_two_attr] )* $variant_two ),+ )*,
        }
        enum_from_primitive_impl! { $name, $( $variant )+ $( $( $variant_two )+ )* }
    };

    (
        $( #[$enum_attr:meta] )*
        pub enum $name:ident {
            $( $( $( #[$variant_attr:meta] )* $variant:ident ),+ = $discriminator:expr ),+,
        }
    ) => {
        $( #[$enum_attr] )*
        pub enum $name {
            $( $( $( #[$variant_attr] )* $variant ),+ = $discriminator ),+,
        }
        enum_from_primitive_impl! { $name, $( $( $variant )+ )+ }
    };
}"#,
        r#"macro_rules! quick_error {
    (   $(#[$meta:meta])*
        pub enum $name:ident { $($chunks:tt)* }
    ) => { ... };
    (   $(#[$meta:meta])*
        enum $name:ident { $($chunks:tt)* }
    ) => { ... };
    (   $(#[$meta:meta])*
        pub enum $name:ident wraps $enum_name:ident { $($chunks:tt)* }
    ) => { ... };
    (   $(#[$meta:meta])*
        pub enum $name:ident wraps pub $enum_name:ident { $($chunks:tt)* }
    ) => { ... };
    (   $(#[$meta:meta])*
        enum $name:ident wraps $enum_name:ident { $($chunks:tt)* }
    ) => { ... };
    (   $(#[$meta:meta])*
        enum $name:ident wraps pub $enum_name:ident { $($chunks:tt)* }
    ) => { ... };
    (
        WRAPPER $internal:ident [ $($strdef:tt)* ] $strname:ident
        $(#[$meta:meta])*
    ) => { ... };
    (SORT [enum $name:ident $( #[$meta:meta] )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [ ]
        queue [ ]
    ) => { ... };
    (SORT [pub enum $name:ident $( #[$meta:meta] )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [ ]
        queue [ ]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*]
        queue [ #[$qmeta:meta] $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*]
        queue [ $qitem:ident $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
            => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ #[$qmeta:meta] $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )* => $bitem:ident: UNIT [ ] ]
        queue [($( $qvar:ident: $qtyp:ty ),+) $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )* => $bitem:ident: UNIT [ ] ]
        queue [{ $( $qvar:ident: $qtyp:ty ),+} $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )* => $bitem:ident: UNIT [ ] ]
        queue [{$( $qvar:ident: $qtyp:ty ),+ ,} $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
                 => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ {$( $qfuncs:tt )*} $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
                 => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ $qitem:ident $( $tail:tt )*]
    ) => { ... };
    (SORT [$( $def:tt )*]
        items [$($( #[$imeta:meta] )*
                  => $iitem:ident: $imode:tt [$( $ivar:ident: $ityp:ty ),*]
                                {$( $ifuncs:tt )*} )* ]
        buf [$( #[$bmeta:meta] )*
            => $bitem:ident: $bmode:tt [$( $bvar:ident: $btyp:ty ),*] ]
        queue [ ]
    ) => { ... };
    (ENUM_DEFINITION [pub enum $name:ident $( #[$meta:meta] )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [ ]
    ) => { ... };
    (ENUM_DEFINITION [enum $name:ident $( #[$meta:meta] )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [ ]
    ) => { ... };
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: UNIT [ ] $( $queue:tt )*]
    ) => { ... };
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: TUPLE [$( $qvar:ident: $qtyp:ty ),+] $( $queue:tt )*]
    ) => { ... };
    (ENUM_DEFINITION [$( $def:tt )*]
        body [$($( #[$imeta:meta] )*
            => $iitem:ident ($(($( $ttyp:ty ),+))*) {$({$( $svar:ident: $styp:ty ),*})*} )* ]
        queue [$( #[$qmeta:meta] )*
            => $qitem:ident: STRUCT [$( $qvar:ident: $qtyp:ty ),*] $( $queue:tt )*]
    ) => { ... };
    (IMPLEMENTATIONS
        $name:ident {$(
            $item:ident: $imode:tt [$(#[$imeta:meta])*] [$( $var:ident: $typ:ty ),*] {$( $funcs:tt )*}
        )*}
    ) => { ... };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($self_:tt) -> ($( $exprs:tt )*) $( $tail:tt )*}
    ) => { ... };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($pattern:expr) $( $tail:tt )*}
    ) => { ... };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { display($pattern:expr, $( $exprs:tt )*) $( $tail:tt )*}
    ) => { ... };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { $t:tt $( $tail:tt )*}
    ) => { ... };
    (FIND_DISPLAY_IMPL $name:ident $item:ident: $imode:tt
        { }
    ) => { ... };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { description($expr:expr) $( $tail:tt )*}
    ) => { ... };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { $t:tt $( $tail:tt )*}
    ) => { ... };
    (FIND_DESCRIPTION_IMPL $item:ident: $imode:tt $me:ident $fmt:ident
        [$( $var:ident ),*]
        { }
    ) => { ... };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { cause($expr:expr) $( $tail:tt )*}
    ) => { ... };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { $t:tt $( $tail:tt )*}
    ) => { ... };
    (FIND_CAUSE_IMPL $item:ident: $imode:tt
        [$( $var:ident ),*]
        { }
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { from() $( $tail:tt )*}
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: UNIT
        [ ]
        { from($ftyp:ty) $( $tail:tt )*}
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: TUPLE
        [$( $var:ident: $typ:ty ),*]
        { from($fvar:ident: $ftyp:ty) -> ($( $texpr:expr ),*) $( $tail:tt )*}
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
        { from($fvar:ident: $ftyp:ty) -> {$( $tvar:ident: $texpr:expr ),*} $( $tail:tt )*}
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { $t:tt $( $tail:tt )*}
    ) => { ... };
    (FIND_FROM_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { }
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: TUPLE
        [$( $var:ident: $typ:ty ),*]
        { context($cvar:ident: AsRef<$ctyp:ty>, $fvar:ident: $ftyp:ty)
            -> ($( $texpr:expr ),*) $( $tail:tt )* }
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: TUPLE
        [$( $var:ident: $typ:ty ),*]
        { context($cvar:ident: $ctyp:ty, $fvar:ident: $ftyp:ty)
            -> ($( $texpr:expr ),*) $( $tail:tt )* }
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
        { context($cvar:ident: AsRef<$ctyp:ty>, $fvar:ident: $ftyp:ty)
            -> {$( $tvar:ident: $texpr:expr ),*} $( $tail:tt )* }
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
        { context($cvar:ident: $ctyp:ty, $fvar:ident: $ftyp:ty)
            -> {$( $tvar:ident: $texpr:expr ),*} $( $tail:tt )* }
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { $t:tt $( $tail:tt )*}
    ) => { ... };
    (FIND_CONTEXT_IMPL $name:ident $item:ident: $imode:tt
        [$( $var:ident: $typ:ty ),*]
        { }
    ) => { ... };
    (ITEM_BODY $(#[$imeta:meta])* $item:ident: UNIT
    ) => { ... };
    (ITEM_BODY $(#[$imeta:meta])* $item:ident: TUPLE
        [$( $typ:ty ),*]
    ) => { ... };
    (ITEM_BODY $(#[$imeta:meta])* $item:ident: STRUCT
        [$( $var:ident: $typ:ty ),*]
    ) => { ... };
    (ITEM_PATTERN $name:ident $item:ident: UNIT []
    ) => { ... };
    (ITEM_PATTERN $name:ident $item:ident: TUPLE
        [$( ref $var:ident ),*]
    ) => { ... };
    (ITEM_PATTERN $name:ident $item:ident: STRUCT
        [$( ref $var:ident ),*]
    ) => { ... };
    (ERROR_CHECK $imode:tt display($self_:tt) -> ($( $exprs:tt )*) $( $tail:tt )*) => { ... };
    (ERROR_CHECK $imode:tt display($pattern: expr) $( $tail:tt )*) => { ... };
    (ERROR_CHECK $imode:tt display($pattern: expr, $( $exprs:tt )*) $( $tail:tt )*) => { ... };
    (ERROR_CHECK $imode:tt description($expr:expr) $( $tail:tt )*) => { ... };
    (ERROR_CHECK $imode:tt cause($expr:expr) $($tail:tt)*) => { ... };
    (ERROR_CHECK $imode:tt from() $($tail:tt)*) => { ... };
    (ERROR_CHECK $imode:tt from($ftyp:ty) $($tail:tt)*) => { ... };
    (ERROR_CHECK TUPLE from($fvar:ident: $ftyp:ty) -> ($( $e:expr ),*) $( $tail:tt )*) => { ... };
    (ERROR_CHECK STRUCT from($fvar:ident: $ftyp:ty) -> {$( $v:ident: $e:expr ),*} $( $tail:tt )*) => { ... };
    (ERROR_CHECK TUPLE context($cvar:ident: $ctyp:ty, $fvar:ident: $ftyp:ty)
        -> ($( $e:expr ),*) $( $tail:tt )*) => { ... };
    (ERROR_CHECK STRUCT context($cvar:ident: $ctyp:ty, $fvar:ident: $ftyp:ty)
        -> {$( $v:ident: $e:expr ),*} $( $tail:tt )*) => { ... };
    (ERROR_CHECK $imode:tt ) => { ... };
    (IDENT $ident:ident) => { ... };
}"#,
    ][..];
    to_example_page(
        "Examples from various crates, from the coy to the insane",
        various_examples,
        "examples/various_examples.html",
    )?;
    println!("Done");

    Ok(())
}
