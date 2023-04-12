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

    fn shift(&mut self, id: &Self::Index, depth: usize, delta: i64) -> (Self::Index, bool) {
        let term = match self.get(id).clone() {
            Term::Ext(_) => {
                return (id.clone(), false);
            }
            Term::Var(d) => {
                // eprintln!("\n shift {d}, {depth}");
                if d > depth {
                    // Rebound inside the nodes.
                    return (id.clone(), false);
                }
                Term::Var((d as i64 + delta) as usize) // References something outside.
            }
            Term::Abs(inner) => {
                let (inner, new_inner) = self.shift(&inner, depth, delta);
                if !new_inner {
                    return (id.clone(), false);
                }
                Term::Abs(inner)
            }
            Term::App(inner, arg) => {
                let (inner, new_inner) = self.shift(&inner, depth, delta);
                let (arg, new_arg) = self.shift(&arg, depth, delta);
                if !new_inner && !new_arg {
                    return (id.clone(), false);
                }
                Term::App(inner, arg)
            }
        };
        (self.add(term), true)
    }
    fn subst(&mut self, id: &Self::Index, val: &Self::Index, depth: usize) -> (Self::Index, bool) {
        let term = match self.get(id).clone() {
            Term::Ext(_) => {
                return (id.clone(), false);
            }
            Term::Var(d) => {
                // eprintln!("\n subst {d}, {depth}");
                if d == depth {
                    // Create a shifted version!
                    let (id, _changed) = self.shift(val, 0, depth as i64);
                    return (id, true);
                }
                return (id.clone(), false);
            }
            Term::Abs(inner) => {
                let (inner, new_inner) = self.subst(&inner, val, depth + 1);
                if !new_inner {
                    return (id.clone(), false);
                }
                Term::Abs(inner)
            }
            Term::App(inner, arg) => {
                let (inner, new_inner) = self.subst(&inner, val, depth);
                let (arg, new_arg) = self.subst(&arg, val, depth);
                if !new_inner && !new_arg {
                    return (id.clone(), false);
                }
                Term::App(inner, arg)
            }
        };
        (self.add(term), true)
    }

    fn reduce_at(&mut self, id: Self::Index, depth: usize) -> (Self::Index, bool) {
        let (mut id, had_changed) = self.reduce_at_impl(id, depth);
        let mut changed = true;
        while had_changed && changed {
            (id, changed) = self.reduce_at_impl(id, depth);
        }
        (id, had_changed)
    }

    fn reduce_at_impl(&mut self, id: Self::Index, depth: usize) -> (Self::Index, bool) {
        // eprintln!("{}reducing {}", "  ".repeat(depth), self.as_context(&id));
        let curr = self.get(&id).clone();
        match curr {
            Term::Ext(_) => {}
            Term::Var(_) => {}
            Term::Abs(inner) => {
                let (inner, new_inner) = self.reduce_at(inner, depth + 1);
                if new_inner {
                    return (self.add(Term::Abs(inner)), true);
                }
            }
            Term::App(inner, arg) => {
                let (arg, new_arg) = self.reduce_at(arg, depth + 1);
                let (inner, new_inner) = self.reduce_at(inner, depth + 1);
                if let Term::Abs(inner) = self.get(&inner).clone() {
                    // eprint!("{}applying {} to {}", "  ".repeat(depth), self.as_context(&arg), self.as_context(&inner));
                    // Beta reduction.
                    // let (inner, _changed) = self.shift(&inner, 0, -1);
                    let (inner, _changed) = self.subst(&inner, &arg, 1);
                    // eprintln!(" -> {}", self.as_context(&inner));
                    return (inner, true);
                }
                //if let Term::Ext(val) = self.get(inner) {
                // TOdO
                //}
                if new_inner || new_arg {
                    return (self.add(Term::App(inner, arg)), true);
                }
            }
        }
        (id, false)
    }
    fn reduce(&mut self)
    where
        Term<Self::Extension, Self::Index>: std::fmt::Debug + Clone,
    {
        // TODO: Beta and Eta reduction.
        let root = self.root().clone();
        let (root, _changed) = self.reduce_at(root, 0);
        // assign new root:
        *self.root_mut() = root;
    }

    fn apply_to_value(
        &mut self,
        value: Self::Extension,
        _arg: Term<Self::Extension, Self::Index>,
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
