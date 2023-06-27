pub mod compact_to_church;
pub mod count_depth;
pub mod count_nodes;
pub mod transform_meta;

use crate::{Expr, Term};
pub use transform_meta::{map_nodes, map_to_nodes, number_nodes};

pub trait Visitor<E, Over: Expr> {
    type Type;

    fn start_value(&mut self) -> Self::Type;

    fn on_ext(
        &mut self,
        _ctx: &Over,
        _ext: &Over::Value,
        _meta: &Over::Meta,
    ) -> Result<Self::Type, E> {
        Ok(self.start_value())
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize, _meta: &Over::Meta) -> Result<Self::Type, E> {
        Ok(self.start_value())
    }
    fn on_abs(
        &mut self,
        ctx: &Over,
        arg_meta: &Option<Over::Index>,
        inner: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<Self::Type, E> {
        if let Some(arg_meta) = arg_meta {
            self.on_id(ctx, arg_meta)?;
        }
        self.on_id(ctx, inner)?;
        Ok(self.start_value())
    }
    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<Self::Type, E> {
        self.on_id(ctx, inner)?;
        self.on_id(ctx, arg)?;
        Ok(self.start_value())
    }
    fn on_id(&mut self, ctx: &Over, id: &Over::Index) -> Result<Self::Type, E> {
        let meta = ctx.get_meta(id);
        self.on_term(ctx, ctx.get(id), meta)
    }
    fn on_term(
        &mut self,
        ctx: &Over,
        term: &Term<Over::Value, Over::Index>,
        meta: &Over::Meta,
    ) -> Result<Self::Type, E> {
        match term {
            Term::Ext(ext) => self.on_ext(ctx, ext, meta),
            Term::Var(n) => self.on_var(ctx, *n, meta),
            Term::Abs(arg, inner) => self.on_abs(ctx, arg, inner, meta),
            Term::App(inner, arg) => self.on_app(ctx, inner, arg, meta),
        }
    }
}
