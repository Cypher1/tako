#[macro_export]
macro_rules! as_context(
    { $t: ty, $ctx: ty, $name: ident } => {
        #[derive(Debug)]
        pub struct $name<'a> {
            value: $t,
            ctx: &'a $ctx,
        }

        impl $name<'_> {
            pub fn context(&self) -> &$ctx {
              &self.ctx
            }

            pub fn child(&self, child: $t) -> Self {
                $name { value: child, ctx: self.ctx }
            }

            fn in_context<'a>(ctx: &'a $ctx, value: $t) -> $name<'a> {
                $name { value, ctx }
            }
        }

        impl std::ops::Deref for $name<'_> {
            type Target = $t;

            fn deref(&self) -> &$t {
                &self.value
            }
        }
     };
);
