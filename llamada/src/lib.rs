#[cfg(test)]
#[macro_use]
pub mod tests;

pub mod base_types;
mod expr_result;
pub mod reprs;
pub mod type_checking;
pub mod visitors;
#[macro_use]
pub mod macros;
pub mod union_find;
use crate::expr_result::EvalInfo;
use base_types::{Empty, Never};
pub use expr_result::ExprResult;
pub use reprs::dense::{DenseRepr, LambdaCalc};
pub use reprs::llamada::Llamada;
pub use visitors::Visitor;

pub struct BoundExpr<'a, Expr: ?Sized> {
    pub expr: &'a Expr,
    pub bind_depth: usize,
}

impl<Expr> Clone for BoundExpr<'_, Expr> {
    fn clone(&self) -> Self {
        Self {
            expr: self.expr,
            bind_depth: self.bind_depth,
        }
    }
}

pub struct WithContext<'a, Ctx: ?Sized, T> {
    // TODO(cleanup): Update to use better_std::as_context
    pub ctx: BoundExpr<'a, Ctx>,
    pub val: &'a T,
}

impl<'a, Ctx, T> WithContext<'a, Ctx, T> {
    pub fn new(ctx: BoundExpr<'a, Ctx>, val: &'a T) -> Self {
        WithContext { ctx, val }
    }

    pub fn child<U>(&self, val: &'a U) -> WithContext<'a, Ctx, U> {
        WithContext::new(self.ctx.clone(), val)
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum Term<T, Id> {
    Var(usize),
    App(Id, Id),
    Abs(Option<Id>, Id), // TODO: Add simply typed 'type'.
    Ext(T),
}

impl<T, Id> Term<T, Id> {
    pub fn abs(id: Id) -> Self {
        Self::Abs(None, id)
    }
}

pub fn ext<T, U: Into<T>, Id>(t: U) -> Term<T, Id> {
    Term::Ext(t.into())
}

pub trait Evaluable {
    fn info(&self) -> Option<EvalInfo> {
        None
    }
}

impl Evaluable for Never {}
impl Evaluable for Empty {}

pub trait Expr: Sized {
    type Index: Clone + Eq + PartialEq + std::fmt::Debug;
    type Value: Evaluable + Clone;
    type Meta: Clone + Default + std::fmt::Display;

    fn new(term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self;

    fn get_last_id(&self) -> Self::Index;
    fn get<'a>(&'a self, id: &'a Self::Index) -> &'a Term<Self::Value, Self::Index>;
    fn get_mut<'a>(&'a mut self, id: &'a mut Self::Index)
        -> &'a mut Term<Self::Value, Self::Index>;
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &'a Self::Meta;
    fn root(&self) -> &Self::Index;
    fn root_mut(&mut self) -> &mut Self::Index;

    fn add_with_meta(
        &mut self,
        term: Term<Self::Value, Self::Index>,
        meta: Self::Meta,
    ) -> Self::Index;

    fn add(&mut self, term: Term<Self::Value, Self::Index>) -> Self::Index {
        self.add_with_meta(term, Self::Meta::default())
    }

    fn shift(
        &mut self,
        id: &Self::Index,
        depth: usize,
        delta: i64,
    ) -> ExprResult<Self::Index, EvalInfo> {
        match self.get(id).clone() {
            Term::Ext(_) => ExprResult::unchanged(id.clone()),
            Term::Var(d) => {
                // eprintln!("\n shift {d}, {depth}");
                if d > depth {
                    // Rebound inside the nodes.
                    ExprResult::unchanged(id.clone())
                } else {
                    ExprResult::new(self.add(Term::Var((d as i64 + delta) as usize)), true, None)
                    // References something outside.
                }
            }
            Term::Abs(_arg_meta, inner) => {
                let inner = self.shift(&inner, depth, delta);
                inner.if_changed(
                    |inner| self.add(Term::Abs(_arg_meta, inner)),
                    |_| id.clone(),
                )
            }
            Term::App(inner, arg) => {
                let inner = self.shift(&inner, depth, delta);
                let arg = self.shift(&arg, depth, delta);
                inner.apply(arg).if_changed(
                    |(inner, arg)| self.add(Term::App(inner, arg)),
                    |_| id.clone(),
                )
            }
        }
    }
    fn subst(
        &mut self,
        id: &Self::Index,
        val: &Self::Index,
        depth: usize,
    ) -> ExprResult<Self::Index, EvalInfo> {
        match self.get(id).clone() {
            Term::Ext(_) => ExprResult::unchanged(id.clone()),
            Term::Var(d) => {
                // eprintln!("\n subst {d}, {depth}");
                if d == depth {
                    // Create a shifted version!
                    let id = self.shift(val, 0, depth as i64);
                    id.changed()
                } else {
                    ExprResult::unchanged(id.clone())
                }
            }
            Term::Abs(arg_meta, inner) => {
                let inner = self.subst(&inner, val, depth + 1);
                inner.if_changed(|inner| self.add(Term::Abs(arg_meta, inner)), |_| id.clone())
            }
            Term::App(inner, arg) => {
                let inner = self.subst(&inner, val, depth);
                let arg = self.subst(&arg, val, depth);
                inner.apply(arg).if_changed(
                    |(inner, arg)| self.add(Term::App(inner, arg)),
                    |_| id.clone(),
                )
            }
        }
    }

    fn reduce_at(&mut self, id: Self::Index, depth: usize) -> ExprResult<Self::Index, EvalInfo> {
        let mut id = self.reduce_at_impl(id, depth);
        let had_changed = id.changed;
        while had_changed && id.changed {
            id = self.reduce_at_impl(id.id, depth);
        }
        id.changed = had_changed;
        id
    }

    fn reduce_at_impl(
        &mut self,
        id: Self::Index,
        depth: usize,
    ) -> ExprResult<Self::Index, EvalInfo> {
        // eprintln!("{}reducing {}", "  ".repeat(depth), self.as_context(&id));
        let curr = self.get(&id).clone();
        match curr {
            Term::Ext(ext) => ExprResult::unchanged(id).with_ext_info(ext.info()),
            Term::Var(_) => ExprResult::unchanged(id),
            Term::Abs(arg_meta, inner) => self
                .reduce_at(inner, depth + 1)
                .if_changed(|inner| self.add(Term::Abs(arg_meta, inner)), |_| id),
            Term::App(inner, arg) => {
                let arg = self.reduce_at(arg, depth + 1);
                let inner = self.reduce_at(inner, depth + 1);
                if let Term::Abs(_arg_meta, inner) = self.get(&inner.id).clone() {
                    // TODO: Check `arg` against `arg_meta`.
                    // Beta reduction.
                    let inner = self.subst(&inner, &arg.id, 1);
                    return inner.changed();
                }
                //if let Term::Ext(val) = self.get(inner) {
                // TOdO
                //}
                let inner = inner.apply(arg);
                let inner =
                    inner.if_changed(|(inner, arg)| self.add(Term::App(inner, arg)), |_| id);
                self.reduce_ext_apps(inner)
            }
        }
    }
    fn reduce(&mut self)
    where
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone,
    {
        // TODO: Beta and Eta reduction.
        let root = self.root().clone();
        let root = self.reduce_at(root, 0);
        // assign new root:
        *self.root_mut() = root.id;
    }

    fn reduce_ext_apps(
        &mut self,
        value: ExprResult<Self::Index, EvalInfo>,
    ) -> ExprResult<Self::Index, EvalInfo> {
        value
    }

    fn to_church(&mut self, i: u32) -> Self::Index {
        let startv = self.add(Term::Var(1));
        let mut curr = startv;
        if i > 0 {
            let f = self.add(Term::Var(2));
            for _ in 0..i {
                curr = self.add(Term::App(f.clone(), curr));
            }
        }
        curr = self.add(Term::abs(curr));
        curr = self.add(Term::abs(curr));
        curr
    }

    fn as_church(&self, id: &Self::Index) -> Option<u32> {
        // (\f. (\x. (f ... (f x)...) ))
        let inner = match self.get(id) {
            Term::Abs(_arg_meta, inner) => inner,
            _ => return None,
        };
        let mut inner = match self.get(inner) {
            Term::Abs(_arg_meta, inner) => inner,
            _ => return None,
        };
        let mut i = 0;
        loop {
            match self.get(inner) {
                Term::App(a, b) => match self.get(a) {
                    Term::Var(2) => {
                        i += 1;
                        inner = b;
                    }
                    _ => return None,
                },
                Term::Var(x) if *x == 1 => {
                    return Some(i);
                }
                _ => return None,
            }
        }
    }

    fn print_meta(&self) -> bool;
    fn set_print_meta(&mut self, print_meta: bool);

    fn fmt_root(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where
        Self::Value: std::fmt::Display,
    {
        write!(f, "{}", &self.as_context(self.root()))
    }

    fn traverse<T, E, V: Visitor<E, Self, Type = T>>(&self, visitor: &mut V) -> Result<T, E> {
        let root = self.root();
        let meta = &self.get_meta(root);
        let root = &self.get(root);
        visitor.on_term(self, root, meta)
    }

    fn as_context<'a>(&'a self, val: &'a Self::Index) -> WithContext<'a, Self, Self::Index> {
        WithContext::new(
            BoundExpr {
                expr: self,
                bind_depth: 0,
            },
            val,
        )
    }
}

struct NameIndex {
    depth: usize,
}

impl std::fmt::Display for NameIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let len = self.depth;
        let chr = ((len % 26) + ('a' as usize)) as u8 as char;
        let name_ind = len / 26;
        write!(f, "{chr}")?;
        if name_ind > 0 {
            write!(f, "{:?}", name_ind - 1)?;
        }
        Ok(())
    }
}

impl<Ctx> std::fmt::Display for WithContext<'_, Ctx, Ctx::Index>
where
    Ctx: Expr,
    Ctx::Value: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let term: &Term<Ctx::Value, Ctx::Index> = self.ctx.expr.get(self.val);
        match term {
            Term::Ext(val) => write!(f, "{val}")?,
            Term::Var(var_id) => {
                // TODO(cleanup): Name lookup and generation is deterministic based on the number of bindings.
                // Replace with a simple index counter.
                if let Some(ind) = self.ctx.bind_depth.checked_sub(*var_id) {
                    let name = format!("{}", NameIndex { depth: ind });
                    write!(f, "{name}")?;
                } else {
                    write!(f, "#{var_id:?}")?;
                }
            }
            Term::App(x, y) => {
                write!(f, "({} {})", &self.child(x), &self.child(y))?;
            }
            Term::Abs(arg_meta, ind) => {
                // TODO(cleanup): Name generation is deterministic based on the number of bindings.
                // Replace with a simple index counter.
                write!(
                    f,
                    "({name}",
                    name = NameIndex {
                        depth: self.ctx.bind_depth
                    }
                )?;
                if let Some(arg_meta) = arg_meta {
                    write!(f, ": {}", &self.child(arg_meta))?;
                }
                write!(
                    f,
                    " => {})",
                    &WithContext::new(
                        BoundExpr {
                            expr: self.ctx.expr,
                            bind_depth: self.ctx.bind_depth + 1
                        },
                        ind
                    ),
                )?;
            }
        }
        if self.ctx.expr.print_meta() {
            let meta = self.ctx.expr.get_meta(self.val);
            write!(f, ": {meta}")?;
        }
        Ok(())
    }
}
