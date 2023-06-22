use crate::base_types::Never;

pub trait ValueInfo: Sized {
    fn complete(&self) -> bool;
    fn apply(inner: Option<Self>, arg: Option<Self>) -> Option<Self>;
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct EvalInfo {
    pub arity: usize,
    arg_count: usize,
}

impl EvalInfo {
    pub const fn new(arity: usize) -> Self {
        Self {
            arity,
            arg_count: 0,
        }
    }
}

impl ValueInfo for EvalInfo {
    fn complete(&self) -> bool {
        self.arg_count == self.arity
    }
    fn apply(left: Option<Self>, right: Option<Self>) -> Option<Self> {
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
                Some(Self {
                    arity: left.arity,
                    arg_count: left.arg_count + 1,
                })
            }
            _ => None,
        }
    }
}

impl ValueInfo for Never {
    fn complete(&self) -> bool {
        false
    }
    fn apply(_left: Option<Self>, _right: Option<Self>) -> Option<Self> {
        None
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct ExprResult<T, I> {
    pub id: T,
    pub changed: bool,
    pub ext_info: Option<I>,
}

impl<T, I: ValueInfo + Sized> ExprResult<T, I> {
    pub fn apply<U>(self, other: ExprResult<U, I>) -> ExprResult<(T, U), I> {
        ExprResult {
            id: (self.id, other.id),
            changed: self.changed || other.changed,
            ext_info: I::apply(self.ext_info, other.ext_info),
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
    pub fn with_ext_info(self, ext_info: Option<I>) -> Self {
        Self { ext_info, ..self }
    }
    pub fn new(id: T, changed: bool, ext_info: Option<I>) -> Self {
        Self {
            id,
            changed,
            ext_info,
        }
    }
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> ExprResult<R, I> {
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
    ) -> ExprResult<R, I> {
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
