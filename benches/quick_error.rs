#![recursion_limit = "512"]
#![feature(test)]

#[macro_use]
extern crate quote;

use syn;
extern crate test;

use macro_railroad::parser::MacroRules;
use test::Bencher;

#[bench]
fn bench(b: &mut Bencher) {
    let quick_error = quote! {
        macro_rules! quick_error {
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
        }
    };
    b.iter(|| syn::parse2::<MacroRules>(quick_error.clone()).unwrap());
}
