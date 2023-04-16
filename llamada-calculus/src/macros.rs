#[macro_export]
macro_rules! expr(
    { $ctx: expr, $final: ident, $( $name: ident = $ex: expr ),* $(,)? } => {
        {
            use $crate::Term::*;
            $( let $name = $ctx.add($ex); )*
            $final
        }
    }
);
#[macro_export]
macro_rules! new_expr(
    { $ty: ty, $final: ident, $first: ident = $first_ex: expr, $( $name: ident = $ex: expr ),* $(,)? } => {
        {
            use $crate::Term::*;
            let mut e = <$ty>::new($first_ex, Empty {});
            let $first = e.get_last_id();
            let f = expr!(e, $final, $( $name = $ex, )*);
            e.set_root(f);
            e
        }
    }
);
