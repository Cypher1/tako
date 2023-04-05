#[cfg(test)]
#[macro_use]
pub mod tests;

pub mod dense;
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
    Val(T),
}

pub trait Expr: Sized {
    type Index: Clone + Eq + PartialEq + std::fmt::Debug;
    type Value: Clone + std::fmt::Display + std::fmt::Debug;
    type Meta: std::fmt::Display;

    fn new(term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self;

    fn get<'a>(&'a self, id: &'a Self::Index) -> &'a Term<Self::Value, Self::Index>;
    fn get_mut<'a>(&'a mut self, id: &'a mut Self::Index) -> &'a mut Term<Self::Value, Self::Index>;
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &'a Self::Meta;
    fn get_meta_mut<'a>(&'a mut self, id: &'a mut Self::Index) -> &'a mut Self::Meta;

    fn root(&self) -> &Self::Index;
    fn root_mut(&mut self) -> &mut Self::Index;
    fn new_meta(&mut self) -> Self::Meta;

    fn add(&mut self, term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self::Index;

    fn shift(&mut self, id: &Self::Index, depth: usize, delta: i64) -> Self::Index {
        let term = match self.get(&id).clone() {
            Term::Val(_) => {
                return id.clone();
            }
            Term::Var(d) if d < depth => {
                Term::Var((d as i64 +delta)as usize)
            }
            Term::Var(_) => {
                return id.clone();
            }
            Term::Abs(inner) => {
                let new_inner = self.shift(&inner, depth+1, delta);
                if new_inner != inner {
                    Term::Abs(new_inner)
                } else {
                    return id.clone();
                }
            }
            Term::App(inner, arg) => {
                let new_inner = self.shift(&inner, depth, delta);
                let new_arg = self.shift(&arg, depth, delta);
                if new_inner != inner || new_arg != arg {
                    Term::App(new_inner, new_arg)
                } else {
                    return id.clone();
                }
            }
        };
        let meta = self.new_meta();
        self.add(term, meta)
    }
    fn subst(&mut self, id: &Self::Index, val: &Self::Index, depth: usize) -> Self::Index {
        // TODO: What...
        let term = match self.get(&id).clone() {
            Term::Val(_) => {
                return id.clone();
            }
            Term::Var(d) if d == depth => {
                // Create a shifted version!
                return self.shift(val, 0, depth as i64);
            }
            Term::Var(_) => {
                return id.clone();
            }
            Term::Abs(inner) => {
                let new_inner = self.subst(&inner, val, depth+1);
                if new_inner == inner {
                    return id.clone();
                }
                Term::Abs(new_inner)
            }
            Term::App(inner, arg) => {
                let new_inner = self.subst(&inner, val, depth);
                let new_arg = self.subst(&arg, val, depth);
                if new_inner == inner && new_arg == arg {
                    return id.clone();
                }
                Term::App(new_inner, new_arg)
            }
        };
        let meta = self.new_meta();
        self.add(term, meta)
    }

    fn reduce_at<'a>(&'a mut self, id: &mut Self::Index)
    where
        // for <'b> &'b WithContext<'a, Self, Self::Index>: std::fmt::Display,
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone
    {
        let mut curr = self.get(id).clone();
        eprintln!("{id:?} {:?}", curr);
        match &mut curr {
            Term::Val(_) => {},
            Term::Var(_) => {},
            Term::Abs(inner) => {
                self.reduce_at(inner);
            }
            Term::App(ref mut inner, ref mut arg) => {
                self.reduce_at(arg);
                self.reduce_at(inner);
                let inner = self.get(&inner);
                eprintln!("App({:?}) -> {:?}", arg, &inner);
                if let Term::Abs(mut inner) = inner.clone() {
                    eprintln!("applying {} to {}", self.as_context(&inner), self.as_context(arg));
                    // Beta reduction.
                    let new_inner = self.shift(&mut inner, 0, -1);
                    eprintln!("Updated inner {:?} -> {:?}", &inner, new_inner);
                    let new_inner = self.subst(&new_inner, &arg, 0);
                    eprintln!("Subst inner {:?} -> {:?}", &inner, new_inner);
                    *id = new_inner;
                }
                //if let Term::Val(val) = self.get(inner) {
                // TOdO
                //}
            }
        }
        // Assign back!
        eprintln!("{id:?} {:?}", curr);
        *self.get_mut(id) = curr;
    }
    fn reduce(&mut self)
    where
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone,
    {
        // TODO: Beta and Eta reduction.
        let mut root = self.root().clone();
        self.reduce_at(&mut root);
        // assign new root:
        *self.root_mut() = root;
    }

    fn apply_to_value(&mut self, value: Self::Value, _arg: Term<Self::Value, Self::Index>) -> Term<Self::Value, Self::Index>;

    fn as_context<'a, U>(&'a self, val: &'a U) -> WithContext<'a, Self, U> {
        WithContext::new(self, val, vec!["<>".to_string()])
    }

    fn fmt_index_term<'a>(
        ctx: &WithContext<'a, Self, Self::Index>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result where Self: Sized {
        let this = ctx.ctx;
        let id = ctx.val;
        let term: &Term<Self::Value, Self::Index> = this.get(id);
        match term {
            Term::Val(val) => write!(f, "{:?}", val)?,
            Term::Var(var_id) => {
                let ind = ctx.names.len().checked_sub(*var_id);
                if let Some(ind) = ind {
                    if let Some(name) = ctx.names.get(ind) {
                        return write!(f, "{name}");
                    }
                }
                write!(f, "unbound_variable#{var_id:?}")?;
            }
            Term::App(x, y) => {
                Self::fmt_index(&ctx.child(x, vec![]), f)?;
                write!(f, " ")?;
                Self::fmt_index(&ctx.child(y, vec![]), f)?;
            }
            Term::Abs(ind) => {
                let len = ctx.names.len()-1;
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

    fn fmt_index<'a>(
        ctx: &WithContext<'a, Self, Self::Index>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result where Self: Sized {
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

    fn fmt_root(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Self::fmt_index(&self.as_context(self.root()), f)
    }
}

impl<'a, Ctx: Expr> std::fmt::Display
    for WithContext<'a, Ctx, Ctx::Index> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Ctx::fmt_index(self, f)
    }
}
