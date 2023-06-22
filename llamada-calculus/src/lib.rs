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
pub mod with_context;
use crate::expr_result::EvalInfo;
use base_types::{Empty, Never};
pub use expr_result::ExprResult;
pub use reprs::dense::{DenseRepr, LambdaCalc};
pub use visitors::Visitor;
pub use with_context::WithContext;

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

    fn as_context<'a, U>(&'a self, val: &'a U) -> WithContext<'a, Self, U> {
        WithContext::new(self, val, vec!["?".to_string()])
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

    fn fmt_index_term(
        ctx: &WithContext<'_, Self, Self::Index>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Self: Sized,
        Self::Value: std::fmt::Display,
    {
        let this = ctx.ctx;
        let id = ctx.val;
        let term: &Term<Self::Value, Self::Index> = this.get(id);
        match term {
            Term::Ext(val) => write!(f, "{val}")?,
            Term::Var(var_id) => {
                let ind = ctx.names.len().checked_sub(*var_id);
                if let Some(ind) = ind {
                    if let Some(name) = ctx.names.get(ind) {
                        return write!(f, "{name}");
                    }
                }
                write!(f, "#{var_id:?}")?;
            }
            Term::App(x, y) => {
                write!(f, "(")?;
                Self::fmt_index(&ctx.child(x, vec![]), f)?;
                write!(f, " ")?;
                Self::fmt_index(&ctx.child(y, vec![]), f)?;
                write!(f, ")")?;
            }
            Term::Abs(arg_meta, ind) => {
                let len = ctx.names.len() - 1;
                let chr = ((len % 26) + ('a' as usize)) as u8 as char;
                let name_ind = len / 26;
                let name = format!(
                    "{chr}{}",
                    if name_ind > 0 {
                        format!("{}", name_ind - 1)
                    } else {
                        String::new()
                    }
                );
                write!(f, "(\\{name}")?;
                if let Some(arg_meta) = arg_meta {
                    write!(f, ": ")?;
                    Self::fmt_index(&ctx.child(arg_meta, vec![]), f)?;
                }
                write!(f, ". ")?;
                Self::fmt_index(&ctx.child(ind, vec![name]), f)?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }

    fn fmt_index(
        ctx: &WithContext<'_, Self, Self::Index>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result
    where
        Self::Value: std::fmt::Display,
    {
        Self::fmt_index_term(ctx, f)?;
        let this = ctx.ctx;
        let id = ctx.val;
        if this.print_meta() {
            let meta = this.get_meta(id);
            write!(f, ": {meta}")?;
        }
        Ok(())
    }

    fn print_meta(&self) -> bool;
    fn set_print_meta(&mut self, print_meta: bool);

    fn fmt_root(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    where
        Self::Value: std::fmt::Display,
    {
        Self::fmt_index(&self.as_context(self.root()), f)
    }

    fn traverse<T, E, V: Visitor<E, Self, Type = T>>(&self, visitor: &mut V) -> Result<T, E> {
        let root = self.root();
        let meta = &self.get_meta(root);
        let root = &self.get(root);
        visitor.on_term(self, root, meta)
    }
}

impl<'a, Ctx: Expr> std::fmt::Display for WithContext<'a, Ctx, Ctx::Index>
where
    Ctx::Value: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ctx::fmt_index(self, f)
    }
}
