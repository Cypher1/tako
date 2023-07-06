#[macro_export]
macro_rules! expr(
    { $ctx: expr, $final: ident, $( $name: ident = $ex: expr ),* $(,)? } => {
        {
            #[allow(unused_imports)]
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
            #[allow(unused_imports)]
            use $crate::Term::*;
            use $crate::expr;
            let mut e = <$ty>::new($first_ex, Empty {});
            let $first = e.get_last_id();
            let f = expr!(e, $final, $( $name = $ex, )*);
            *e.root_mut() = f;
            e
        }
    }
);

#[macro_export]
macro_rules! derive_expr_from(
    { $inner: ident } => {
        fn get<'a>(&'a self, id: &'a Self::Index) -> &Term<Self::Value, Self::Index> {
            self.$inner.get(id)
        }
        fn get_mut<'a>(&'a mut self, id: &'a mut Self::Index) -> &mut Term<Self::Value, Self::Index> {
            self.$inner.get_mut(id)
        }
        fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &Self::Meta {
            self.$inner.get_meta(id)
        }
        fn get_last_id(&self) -> Self::Index {
            self.$inner.get_last_id()
        }
        fn root(&self) -> &Self::Index {
            self.$inner.root()
        }
        fn root_mut(&mut self) -> &mut Self::Index {
            self.$inner.root_mut()
        }
        fn add_with_meta(&mut self, term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self::Index {
            self.$inner.add_with_meta(term, meta)
        }
        fn print_meta(&self) -> bool {
            self.$inner.print_meta()
        }
        fn set_print_meta(&mut self, print_meta: bool) {
            self.$inner.set_print_meta(print_meta);
        }
    }
);
