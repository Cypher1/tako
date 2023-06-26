mod error;
mod function;
mod graph;
mod node;
mod value;

#[cfg(test)]
mod use_cases;
use error::GraphErr;
use function::{FunctionOp, Op};
use graph::{Graph, Graphic};
use value::*;

/*
use graph::NodeId;

#[derive(Debug)]
struct GraphView {
    graph: Arc<Mutex<Graph>>,
    active_set: Vec<NodeId>,
    requested_set: Vec<NodeId>, // A dependant is being requested...
}
*/

fn print_g<G: Graphic>(g: &G) -> Result<(), GraphErr> {
    g.with_graph(|g| {
        println!("{g}");
        Ok(())
    })
}

fn main_impl() -> Result<(), GraphErr> {
    let g = Graph::new_shared();

    print_g(&g)?;
    let mut f = Graph::overlay(&g);

    let a = f.add(Value::I64(3));
    let c = f.add_fn(|b| {
        // let op = b.add_op(Op::Sum);
        // b.add_ref(a);
        // b.add_value(Value::I64(3));
        // b.add_function(op, 2);
        b.add_call(Op::IntSum, vec![a.into(), Value::I64(3).into()]);
    });
    let ent = f.add_fn(|b| {
        b.add_call(
            Op::MakeMap,
            vec!["a".into(), a.into(), "c".into(), c.into()],
        );
    });
    f.add_fn(|b| {
        b.add_call(Op::Accessor, vec![ent.into(), "c".into()]);
    });
    let keys = f.add_fn(|b| {
        b.add_call(Op::Keys, vec![ent.into()]);
    });
    let values = f.add_fn(|b| {
        b.add_call(Op::Values, vec![ent.into()]);
    });
    f.add_fn(|b| {
        b.add_call(Op::IntSum, vec![values.into()]);
    });
    let w1 = f.add_fn(|b| {
        b.add_call(Op::StrConcat, vec![keys.into()]);
    });
    f.add_fn(|b| {
        b.add_call(Op::Eq, vec!["a".into(), "b".into()]);
    });
    let eq = f.add_fn(|b| {
        let op = b.add_op(Op::Eq);
        b.add_ref(values);
        b.add_function(op);
    });
    let w2 = f.add_fn(|b| {
        let op = b.add_op(Op::StrConcat);
        b.add_call(Op::VecConcat, vec![values.into(), keys.into()]);
        b.add_function(op);
    });
    let w3 = f.add_fn(|b| {
        let cond = b.add_call(Op::Not, vec![eq.into()]);
        b.add_call(
            Op::If,
            vec![FunctionOp::Index(cond), values.into(), keys.into()],
        );
        b.add_call(Op::If, vec![eq.into(), values.into(), keys.into()]);
    });

    f.watch(w1);
    f.watch(w2);
    f.watch(w3);

    print_g(&f)?;
    f.set(a, Value::I64(4).into())?;
    f.commit().expect("Commit f");
    print_g(&g)?;

    Ok(())
}

fn main() {
    match main_impl() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("Error: {e:?}"); // TODO: impl Display
            std::process::exit(1);
        }
    }
}
