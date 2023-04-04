pub mod dense;
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

pub trait Expr {
    type Index: Clone + Eq + PartialEq + std::fmt::Debug;
    type Value: Clone + std::fmt::Display + std::fmt::Debug;
    type Meta: std::fmt::Display;

    fn get(&self, id: &Self::Index) -> &Term<Self::Value, Self::Index>;
    fn get_mut(&mut self, id: &mut Self::Index) -> &mut Term<Self::Value, Self::Index>;
    fn get_meta(&self, id: &Self::Index) -> &Self::Meta;
    fn get_meta_mut(&mut self, id: &mut Self::Index) -> &mut Self::Meta;

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
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone,
    {
        let mut curr = self.get(id).clone();
        eprintln!("{:?}", curr);
        match &mut curr {
            Term::Val(_) => {},
            Term::Var(_) => {},
            Term::Abs(inner) => {
                self.reduce_at(inner);
            }
            Term::App(inner, arg) => {
                self.reduce_at(arg);
                self.reduce_at(inner);
                let inner = self.get(inner).clone();
                eprintln!("App({:?}) -> {:?}", arg, &inner);
                if let Term::Abs(inner) = inner {
                    // eprintln!("applying {} to {}", self.as_context(&inner), self.as_context(&arg));
                    // Beta reduction.
                    let new_inner = self.shift(&inner, 0, -1);
                    eprintln!("Updated inner {:?} -> {:?}", &inner, new_inner);
                    let new_inner = self.subst(&new_inner, arg, 0);
                    eprintln!("Subst inner {:?} -> {:?}", &inner, new_inner);
                    *id = new_inner;
                }
                //if let Term::Val(val) = self.get(inner) {
                // TOdO
                //}
            }
        }
        // Assign back!
        eprintln!("{:?}", curr);
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

    fn as_context<'a, U>(&'a self, val: &'a U) -> WithContext<'a, Self, U> where Self: Sized {
        WithContext::new(&self, val, vec!["<>".to_string()])
    }

    fn fmt_index<'a>(
        ctx: WithContext<'a, Self, Self::Index>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result where Self: Sized {
        let id = ctx.val;
        let term = ctx.ctx.get(id);
        write!(f, "{term}", term = ctx.child(term, vec![]))?;
        if ctx.ctx.print_meta() {
            let meta = ctx.ctx.get_meta(id);
            write!(f, ": {meta}")?;
        }
        Ok(())
    }

    fn print_meta(&self) -> bool;
}

impl<'a, Ctx: Expr> std::fmt::Display
    for WithContext<'a, Ctx, Term<Ctx::Value, Ctx::Index>> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.val {
            Term::Val(val) => write!(f, "{:?}", val),
            Term::Var(var_id) => {
                let ind = self.names.len().checked_sub(*var_id);
                if let Some(ind) = ind {
                    if let Some(name) = self.names.get(ind) {
                        return write!(f, "{name}");
                    }
                }
                write!(f, "unbound_variable#{var_id:?}")
            }
            Term::App(x, y) => {
                Ctx::fmt_index(self.child(x, vec![]), f)?;
                write!(f, " ")?;
                Ctx::fmt_index(self.child(y, vec![]), f)
            }
            Term::Abs(ind) => {
                let len = self.names.len()-1;
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
                Ctx::fmt_index(self.child(ind, vec![name]), f)?;
                write!(f, ")")
            }
        }
    }
}
