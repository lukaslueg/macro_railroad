
[![Build Status](https://travis-ci.org/lukaslueg/macro_railroad.svg?branch=master)](https://travis-ci.org/lukaslueg/macro_railroad)

**[Live demo](https://lukaslueg.github.io/macro_railroad_wasm_demo/)** ([code](https://github.com/lukaslueg/macro_railroad_wasm))

A library to generate syntax ("railroad") diagrams for Rust's `macro_rules!()`.

Diagrams are generated as Scalable Vector Graphics, with layout-details controlled by customizable CSS.

Given the input

```rust
macro_rules! handlebars_helper {
    ($struct_name:ident: |$($name:ident: $tpe:tt),*| $body:expr ) => { ... };
    (@as_json_value $x:ident, object) => { ... };
    (@as_json_value $x:ident, array) => { ... };
    (@as_json_value $x:ident, str) => { ... };
    (@as_json_value $x:ident, i64) => { ... };
    (@as_json_value $x:ident, u64) => { ... };
    (@as_json_value $x:ident, f64) => { ... };
    (@as_json_value $x:ident, bool) => { ... };
    (@as_json_value $x:ident, null) => { ... };
}
```

... the library generates an SVG which renders (using default CSS) as

![Image output](https://raw.githubusercontent.com/lukaslueg/macro_railroad/master/examples/handlebars_helper.jpg)


The library is currently mostly a tech-demo. Pull requests are most welcome.

To generate the examples shown here, run

```
cargo run --example various
```

which will output some html-files to `/examples`; Feel free to hack on the demos in `/examples/various.rs`.


### Examples


* **[Live demo](https://lukaslueg.github.io/macro_railroad_wasm_demo/)** ([code](https://github.com/lukaslueg/macro_railroad_wasm))
* The macros from `nom-4.0.0` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/nom_examples.html)
* Some of the macros from the standard library [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/stdlib_examples.html) 
* The macros from `syn-0.14` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/syn_examples.html)
* Some macros from various crates, including `bitflags`, `quickcheck` and `cfg-if`, [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/various_examples.html)