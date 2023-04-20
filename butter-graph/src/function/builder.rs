use super::*;

#[derive(Default, Debug)]
pub struct FunctionBuilder {
    function: Function,
}

impl FunctionBuilder {
    pub fn add_expr(&mut self, e: FunctionOp) -> Index {
        self.function.lambdas.push(e);
        (self.function.lambdas.len() as Index) - 1 // Index of the new op.
    }
    pub fn add_value(&mut self, v: Value) -> Index {
        self.add_expr(Value(v))
    }
    pub fn add_op(&mut self, op: Op) -> Index {
        self.add_value(Value::Op(op))
    }
    pub fn add_ref(&mut self, id: NodeId) -> Index {
        self.add_expr(Reference(id))
    }
    //pub fn add_abs(&mut self, id: Index) -> Index {
    //self.add_value(Value::Placeholder); // Holds the binding...
    //self.add_expr(Abstraction(id))
    //}
    pub fn add_call(&mut self, op: Op, args: Vec<FunctionOp>) -> Index {
        let function = self.add_op(op);
        let num_args = args.len() as u32;
        for arg in args {
            self.add_expr(arg);
        }
        self.add_expr(MakeVec(num_args));
        self.add_expr(Apply(function))
    }

    pub fn add_function(&mut self, function: Index) -> Index {
        self.add_expr(Apply(function))
    }

    pub fn finalize(self) -> Function {
        // TODO: Check correctness?
        // TODO: Finalize?
        self.function
    }
}
