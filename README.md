
[![Build Status](https://travis-ci.org/lukaslueg/macro_railroad.svg?branch=master)](https://travis-ci.org/lukaslueg/macro_railroad)

**[Live demo](https://lukaslueg.github.io/macro_railroad_wasm_demo/)** ([code](https://github.com/lukaslueg/macro_railroad_wasm))

**[A browser add-on for Firefox and Chrome](https://github.com/lukaslueg/macro_railroad_ext)**

A library to generate syntax ("railroad") diagrams for Rust's `macro_rules!()`.

Diagrams are generated as Scalable Vector Graphics, with layout-details controlled by customizable CSS.

As an example, given the definition of [nom's `method`](https://docs.rs/nom/4.0.0/nom/macro.method.html)

```rust
macro_rules! method {
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
}
```

... the library generates an SVG which renders (using default CSS) as

![Syntax diagram for nom::method](https://github.com/lukaslueg/macro_railroad/raw/master/examples/nom4_method.svg?sanitize=true)

The library is currently mostly a tech-demo. Pull requests are most welcome.

To generate the examples shown here, run

```
cargo run --example various
```

which will output some html-files to `/examples`; Feel free to hack on the demos in `/examples/various.rs`.


### Examples


* **[Live demo](https://lukaslueg.github.io/macro_railroad_wasm_demo/)** ([code](https://github.com/lukaslueg/macro_railroad_wasm))
* The macros from `nom-4.1.1` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/nom_examples.html)
* Some of the macros from the standard library [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/stdlib_examples.html) 
* The macros from `syn-0.14` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/syn_examples.html)
* Some macros from various crates, including `bitflags`, `quickcheck` and `cfg-if`, [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/various_examples.html)
