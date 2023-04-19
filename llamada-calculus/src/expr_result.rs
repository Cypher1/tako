#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct EvalInfo {
    pub arity: usize,
    arg_count: usize,
}

impl EvalInfo {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            arg_count: 0,
        }
    }
    pub fn complete(&self) -> bool {
        self.arg_count == self.arity
    }
    pub fn apply(left: Option<EvalInfo>, right: Option<EvalInfo>) -> Option<EvalInfo> {
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
    pub id: T,
    pub changed: bool,
    pub ext_info: Option<EvalInfo>,
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
