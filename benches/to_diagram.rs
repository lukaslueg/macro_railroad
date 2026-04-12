use criterion::{Criterion, criterion_group, criterion_main};

// Simple: 3 arms, basic fragments
const VEC: &str = r#"macro_rules! vec {
    ( $ elem : expr ; $ n : expr ) => { ... };
    ( $ ( $ x : expr ) , * ) => { ... };
    ( $ ( $ x : expr , ) * ) => { ... };
}"#;

// Medium: 4 arms, meta/ty/expr fragments, common prefix folding
const LAZY_STATIC: &str = r#"macro_rules! lazy_static {
    ($(#[$attr:meta])* static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    ($(#[$attr:meta])* pub static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    ($(#[$attr:meta])* pub ($($vis:tt)+) static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => { ... };
    () => { ... };
}"#;

// Medium: 3 arms, deeply nested repetitions
const BITFLAGS: &str = r#"macro_rules! bitflags {
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
}"#;

// Complex: ~20 arms, internal `__impl`/`__finalize` arms
const DO_PARSE: &str = r#"macro_rules! do_parse {
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
}"#;

// Large: ~35 arms, heavy internal-arm folding
const CLAP_APP: &str = r#"macro_rules! clap_app {
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
}"#;

// Very large: ~90 arms, keyword + punctuation, heavy folding
const SYN_TOKEN: &str = r#"macro_rules! Token {
    (abstract) => { ... };
    (as) => { ... };
    (async) => { ... };
    (auto) => { ... };
    (become) => { ... };
    (box) => { ... };
    (break) => { ... };
    (const) => { ... };
    (continue) => { ... };
    (crate) => { ... };
    (default) => { ... };
    (do) => { ... };
    (dyn) => { ... };
    (else) => { ... };
    (enum) => { ... };
    (existential) => { ... };
    (extern) => { ... };
    (final) => { ... };
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
    (override) => { ... };
    (priv) => { ... };
    (pub) => { ... };
    (ref) => { ... };
    (return) => { ... };
    (Self) => { ... };
    (self) => { ... };
    (static) => { ... };
    (struct) => { ... };
    (super) => { ... };
    (trait) => { ... };
    (try) => { ... };
    (type) => { ... };
    (typeof) => { ... };
    (union) => { ... };
    (unsafe) => { ... };
    (unsized) => { ... };
    (use) => { ... };
    (virtual) => { ... };
    (where) => { ... };
    (while) => { ... };
    (yield) => { ... };
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
    (~) => { ... };
    (_) => { ... };
}"#;

fn bench_to_diagram(c: &mut Criterion) {
    let cases: &[(&str, &str)] = &[
        ("vec", VEC),
        ("lazy_static", LAZY_STATIC),
        ("bitflags", BITFLAGS),
        ("do_parse", DO_PARSE),
        ("clap_app", CLAP_APP),
        ("syn_token", SYN_TOKEN),
    ];

    let mut group = c.benchmark_group("to_diagram");
    for (name, src) in cases {
        group.bench_function(*name, |b| {
            b.iter(|| macro_railroad::to_diagram(std::hint::black_box(src)).unwrap())
        });
    }
    group.finish();
}

criterion_group!(benches, bench_to_diagram);
criterion_main!(benches);
