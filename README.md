A library to generate syntax diagrams for Rust macros.


```rust
let src = r#"macro_rules! vec {
    ( $ elem : expr ; $ n : expr ) => { ... };
    ( $ ( $ x : expr ) , * ) => { ... };
    ( $ ( $ x : expr , ) * ) => { ... };
}"#;

let dia = macro_railroad::to_diagram(&src).expect("Failed to parse");
assert!(dia.starts_with("<svg"));