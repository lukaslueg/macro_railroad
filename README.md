A library to generate syntax diagrams for Rust macros.


```rust
let src = r#"macro_rules! vec {
    ( $ elem : expr ; $ n : expr ) => { ... };
    ( $ ( $ x : expr ) , * ) => { ... };
    ( $ ( $ x : expr , ) * ) => { ... };
}"#;

let dia = macro_railroad::to_diagram(&src).expect("Failed to parse");
assert!(dia.starts_with("<svg"));
```

Examples:

* The macros from `nom-4.0.0` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/nom_examples.html)
* Some of the macros from the standard library [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/stdlib_examples.html) 
* The macros from `syn-0.14` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/syn_examples.html)
* Some macros from various crates, including `bitflags`, `quickcheck` and `cfg-if` [as diagrams](https://htmlpreview.github.io/?https://github.com/lukaslueg/macro_railroad/blob/master/examples/various_examples.html)