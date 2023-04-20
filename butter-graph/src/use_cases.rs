use crate::error::GraphErr;
use crate::function::{Op, Function, FunctionOp};
use crate::graph::{Graph, Graphic};
use crate::value::*;

fn print_g<G: Graphic>(g: &G) -> Result<(), GraphErr> {
    g.with_graph(|g| {
        println!("{g}");
        Ok(())
    })
}

#[test]
fn org_chart() -> Result<(), GraphErr> {
    let mut g = Graph::new();

    let jay = g.add(Value::Map(dict!{
        "name" => "Jay Pratt",
        "employee_id" => 3,
    }));

    let alice = g.add(Value::Map(dict!{
        "name" => "Alice Johnson",
        "employee_id" => 2,
    }));

    let myk = g.add(Value::Map(dict!{
        "name" => "Michael Martin",
        "employee_id" => 1,
    }));

    let shane = g.add(Value::Map(dict!{
        "name" => "Shane Stephens",
        "employee_id" => 0,
    }));

    let employees = g.add_fn(|b| {
        b.add_call(Op::VecConcat, vec![shane.into(), alice.into(), myk.into(), jay.into()]);
    });

    let jay_reports_to_shane = g.add(Value::Map(dict!{
        "employee_id" => 3,
        "reports_to" => 0,
    }));

    let alice_reports_to_shane = g.add(Value::Map(dict!{
        "employee_id" => 2,
        "reports_to" => 0,
    }));

    let myk_reports_to_shane = g.add(Value::Map(dict!{
        "employee_id" => 1,
        "reports_to" => 0,
    }));

    let reports_to = g.add_fn(|b| {
        b.add_call(Op::VecConcat, vec![alice_reports_to_shane.into(), myk_reports_to_shane.into(), jay_reports_to_shane.into()]);
    });

    let employee_names = g.add_fn(|b| {
        b.add_call(Op::Over, vec![
            Value::Function(Function::build(|b| {
                b.add_call(Op::Accessor, vec![FunctionOp::Binding(0), "name".into()]);
            })).into(),
            employees.into()
        ]);
    });

    let employee_ids = g.add_fn(|b| {
        b.add_call(Op::Over, vec![
            Value::Function(Function::build(|b| {
                b.add_call(Op::Accessor, vec![FunctionOp::Binding(0), "employee_id".into()]);
            })).into(),
            employees.into()
        ]);
    });

    let key_by_employee_ids = g.add_fn(|b| {
        b.add_call(Op::MakeTable, vec![
            Value::Vec(vec!["employee_id".into(), "name".into()]).into(),
            employees.into()
        ]);
    });

    let by_employee_ids = g.add_fn(|b| {
        b.add_call(Op::SortedBy, vec![
            Value::Function(Function::build(|b| {
                b.add_call(Op::Accessor, vec![FunctionOp::Binding(0), "employee_id".into()]);
            })).into(),
            employees.into()
        ]);
    });

    let by_employee_ids_table = g.add_fn(|b| {
        b.add_call(Op::MakeTable, vec![
            Value::Vec(vec!["employee_id".into(), "name".into()]).into(),
            by_employee_ids.into()
        ]);
    });

    g.watch(employees);
    g.watch(employee_names);
    g.watch(employee_ids);
    g.watch(by_employee_ids_table);
    g.watch(reports_to);
    g.watch(key_by_employee_ids);
    print_g(&g)?;

    assert!(false);
    Ok(())
}
