#[cfg(test)]
#[macro_use]
pub mod tests;

pub mod compact_numerals;
pub mod dense;
pub mod ref_counted;
pub mod sparse;
pub mod types;
pub mod with_context;
pub use dense::{DenseRepr, LambdaCalc};
pub use with_context::WithContext;

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum Term<T, Id> {
    Var(usize),
    App(Id, Id),
    Abs(Id),
    Ext(T),
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct EvalInfo {
    arity: usize,
    arg_count: usize,
}

impl EvalInfo {
    fn new(arity: usize) -> Self {
        Self {
            arity,
            arg_count: 0,
        }
    }
    fn complete(&self) -> bool {
        self.arg_count == self.arity
    }
    fn apply(left: Option<EvalInfo>, right: Option<EvalInfo>) -> Option<EvalInfo> {
        match (left, right) {
            (Some(left), Some(right)) => {
                if !right.complete() {
                    // Not ready to apply yet...
                    return None;
                }
                if left.complete() {
                    // Not able to apply...
                    return None;
                }
                Some(EvalInfo {
                    arity: left.arity,
                    arg_count: left.arg_count + 1,
                })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct ExprResult<T> {
    id: T,
    changed: bool,
    ext_info: Option<EvalInfo>,
}

impl<T> ExprResult<T> {
    pub fn apply<U>(self, other: ExprResult<U>) -> ExprResult<(T, U)> {
        ExprResult {
            id: (self.id, other.id),
            changed: self.changed || other.changed,
            ext_info: EvalInfo::apply(self.ext_info, other.ext_info),
        }
    }
    pub fn unchanged(id: T) -> Self {
        Self::new(id, false, None)
    }
    pub fn changed(self) -> Self {
        Self {
            changed: true,
            ..self
        }
    }
    pub fn with_ext_info(self, ext_info: Option<EvalInfo>) -> Self {
        Self { ext_info, ..self }
    }
    pub fn new(id: T, changed: bool, ext_info: Option<EvalInfo>) -> Self {
        Self {
            id,
            changed,
            ext_info,
        }
    }
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> ExprResult<R> {
        ExprResult {
            id: f(self.id),
            changed: self.changed,
            ext_info: self.ext_info,
        }
    }
    pub fn if_changed<R>(
        self,
        then: impl FnOnce(T) -> R,
        els: impl FnOnce(T) -> R,
    ) -> ExprResult<R> {
        ExprResult {
            id: if self.changed {
                then(self.id)
            } else {
                els(self.id)
            },
            changed: self.changed,
            ext_info: self.ext_info,
        }
    }
}

pub trait Expr: Sized {
    type Index: Clone + Eq + PartialEq + std::fmt::Debug;
    type Extension: Clone + std::fmt::Display + std::fmt::Debug;
    type Meta: std::fmt::Display;

    fn new(term: Term<Self::Extension, Self::Index>, meta: Self::Meta) -> Self;

    fn get_last_id(&self) -> Self::Index;
    fn get<'a>(&'a self, id: &'a Self::Index) -> &'a Term<Self::Extension, Self::Index>;
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &'a Self::Meta;
    fn root(&self) -> &Self::Index;
    fn root_mut(&mut self) -> &mut Self::Index;

    fn add(&mut self, term: Term<Self::Extension, Self::Index>) -> Self::Index;

    fn shift(&mut self, id: &Self::Index, depth: usize, delta: i64) -> ExprResult<Self::Index> {
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
            Term::Abs(inner) => {
                let inner = self.shift(&inner, depth, delta);
                inner.if_changed(|inner| self.add(Term::Abs(inner)), |_| id.clone())
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
    ) -> ExprResult<Self::Index> {
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
            Term::Abs(inner) => {
                let inner = self.subst(&inner, val, depth + 1);
                inner.if_changed(|inner| self.add(Term::Abs(inner)), |_| id.clone())
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

    fn reduce_at(&mut self, id: Self::Index, depth: usize) -> ExprResult<Self::Index> {
        let mut id = self.reduce_at_impl(id, depth);
        let had_changed = id.changed;
        while had_changed && id.changed {
            id = self.reduce_at_impl(id.id, depth);
        }
        id.changed = had_changed;
        id
    }

    fn ext_info(&self, _ext: Self::Extension) -> Option<EvalInfo> {
        None
    }
    fn reduce_at_impl(&mut self, id: Self::Index, depth: usize) -> ExprResult<Self::Index> {
        // eprintln!("{}reducing {}", "  ".repeat(depth), self.as_context(&id));
        let curr = self.get(&id).clone();
        match curr {
            Term::Ext(ext) => ExprResult::unchanged(id).with_ext_info(self.ext_info(ext)),
            Term::Var(_) => ExprResult::unchanged(id),
            Term::Abs(inner) => self
                .reduce_at(inner, depth + 1)
                .if_changed(|inner| self.add(Term::Abs(inner)), |_| id),
            Term::App(inner, arg) => {
                let arg = self.reduce_at(arg, depth + 1);
                let inner = self.reduce_at(inner, depth + 1);
                if let Term::Abs(inner) = self.get(&inner.id).clone() {
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
                if let Some(ext_info) = &inner.ext_info {
                    if ext_info.complete() {
                        let inner = self.get(&inner.id).clone();
                        let res = self.reduce_ext_apps(inner);
                        return ExprResult::unchanged(self.add(res)).changed();
                    }
                }
                inner
            }
        }
    }
    fn reduce(&mut self)
    where
        Term<Self::Extension, Self::Index>: std::fmt::Debug + Clone,
    {
        // TODO: Beta and Eta reduction.
        let root = self.root().clone();
        let root = self.reduce_at(root, 0);
        // assign new root:
        *self.root_mut() = root.id;
    }

    fn reduce_ext_apps(
        &mut self,
        _value: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index>;

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
        curr = self.add(Term::Abs(curr));
        curr = self.add(Term::Abs(curr));
        curr
    }

    fn as_church(&self, id: &Self::Index) -> Option<u32> {
        // (\f. (\x. (f ... (f x)...) ))
        let inner = match self.get(id) {
            Term::Abs(inner) => inner,
            _ => return None,
        };
        let mut inner = match self.get(inner) {
            Term::Abs(inner) => inner,
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
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result
    where
        Self: Sized,
    {
        let this = ctx.ctx;
        let id = ctx.val;
        let term: &Term<Self::Extension, Self::Index> = this.get(id);
        match term {
            Term::Ext(val) => write!(f, "{:?}", val)?,
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
            Term::Abs(ind) => {
                let len = ctx.names.len() - 1;
                let chr = ((len % 26) + ('a' as usize)) as u8 as char;
                let name_ind = len / 26;
                let name = format!(
                    "{chr}{}",
                    if name_ind > 0 {
                        format!("{}", name_ind - 1)
                    } else {
                        "".to_string()
                    }
                );
                write!(f, "(\\{name}. ")?;
                Self::fmt_index(&ctx.child(ind, vec![name]), f)?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }

    fn fmt_index(
        ctx: &WithContext<'_, Self, Self::Index>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result
    where
        Self: Sized,
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

    fn fmt_root(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Self::fmt_index(&self.as_context(self.root()), f)
    }
}

impl<'a, Ctx: Expr> std::fmt::Display for WithContext<'a, Ctx, Ctx::Index> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ctx::fmt_index(self, f)
    }
}
