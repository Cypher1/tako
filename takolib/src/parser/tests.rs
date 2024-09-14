use super::semantics::Literal;
use super::*;
use std::path::PathBuf;
use tokens::lex;

fn test_file1() -> PathBuf {
    "test.tk".into()
}

fn setup(s: &str) -> Result<Ast, TError> {
    crate::ensure_initialized();
    let tokens = lex(s)?;
    parse(&test_file1(), s, &tokens)
}

#[test]
fn parse_literal() -> Result<(), TError> {
    let ast = setup("123")?;
    // dbg!(&ast);
    let Ast {
        roots: _, literals, ..
    } = ast;

    assert_eq!(
        literals,
        vec![(
            NodeId::from_raw(0),
            Literal::Numeric, // ("123".to_string()),
        )]
        .into(),
        "Should have parsed a number"
    );

    Ok(())
}

#[test]
fn parse_add_literals() -> Result<(), TError> {
    let ast = setup("1+2")?;
    // dbg!(&ast);

    assert_eq!(
        ast.literals,
        vec![
            (
                NodeId::from_raw(0),
                Literal::Numeric, // ("1".to_string()),
            ),
            (
                NodeId::from_raw(1),
                Literal::Numeric, // ("2".to_string()),
            )
        ]
        .into(),
        "Should have parsed a number"
    );
    assert_eq!(ast.ops.len(), 1);
    Ok(())
}

#[test]
fn parse_negatives() -> Result<(), TError> {
    let ast = setup("-1*-2")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.literals.len(), 2);
    assert_eq!(ast.ops.len(), 3);
    Ok(())
}

#[test]
fn parse_add_add_literals() -> Result<(), TError> {
    let ast = setup("1+2+3")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.literals.len(), 3);
    assert_eq!(ast.ops.len(), 2);
    Ok(())
}

#[test]
fn parse_add_mul_literals() -> Result<(), TError> {
    let ast = setup("1+2*3")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.literals.len(), 3);
    assert_eq!(ast.ops.len(), 2);
    Ok(())
}

#[test]
fn parse_mul_add_literals() -> Result<(), TError> {
    let ast = setup("1*2+3")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.ops.len(), 2);
    assert_eq!(ast.literals.len(), 3);
    Ok(())
}

#[test]
fn parse_mul_add_literals_with_parens() -> Result<(), TError> {
    let ast = setup("(1+2)*3")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.ops.len(), 2);
    assert_eq!(ast.literals.len(), 3);
    Ok(())
}

#[test]
fn parse_atom() -> Result<(), TError> {
    let ast = setup("$x")?;
    // dbg!(&ast);

    assert_eq!(ast.atoms.len(), 1);
    Ok(())
}

#[test]
fn parse_identifier() -> Result<(), TError> {
    let ast = setup("x")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 1);
    Ok(())
}

#[test]
fn parse_call() -> Result<(), TError> {
    let ast = setup("x()")?;
    // dbg!(&ast);

    assert_eq!(ast.calls.len(), 1);
    assert_eq!(ast.identifiers.len(), 1);
    Ok(())
}

#[test]
fn parse_call_unfinished() -> Result<(), TError> {
    let err = setup("x(");
    dbg!(&err);
    assert!(err.is_err());
    let err = err.unwrap_err();
    assert_eq!(format!("{err}"), "Unexpected eof");
    Ok(())
}

#[test]
fn parse_call_with_argument() -> Result<(), TError> {
    let ast = setup("x(12)")?;
    // dbg!(&ast);

    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.calls.len(), 1);
    Ok(())
}

#[test]
fn parse_lambda() -> Result<(), TError> {
    let ast = setup("λx -> x")?;
    // dbg!(&ast);

    assert_eq!(ast.ops.len(), 1);
    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_lambda_keyword() -> Result<(), TError> {
    let ast = setup("lambda x -> x")?;
    // dbg!(&ast);

    assert_eq!(ast.ops.len(), 1);
    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_definition() -> Result<(), TError> {
    let ast = setup("x=1")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 0);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parsed_assignment_with_type_annotation_identifier() -> Result<(), TError> {
    let ast = setup("x:Int=1")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    assert_eq!(ast.atoms.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    Ok(())
}

#[test]
fn parsed_assignment_with_type_annotation_atom() -> Result<(), TError> {
    let ast = setup("x:$Int=1")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 0);
    assert_eq!(ast.atoms.len(), 1);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parsed_assignment_with_type_annotation_expression() -> Result<(), TError> {
    let ast = setup("x:1=1")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 0);
    assert_eq!(ast.atoms.len(), 0);
    assert_eq!(ast.literals.len(), 2);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parsed_assignment_with_typed_argument() -> Result<(), TError> {
    let ast = setup("x(y: Int): Int=1")?;
    eprintln!("{}", ast.pretty());
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 3);
    assert_eq!(ast.atoms.len(), 0);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.definitions.len(), 2);
    Ok(())
}

#[test]
fn parsed_assignment_with_implicit_args() -> Result<(), TError> {
    let ast = setup("id(forall T: Type, y: T): T=y")?;
    eprintln!("{}", &ast.pretty());

    assert_eq!(ast.identifiers.len(), 5);
    assert_eq!(ast.atoms.len(), 0);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.definitions.len(), 3);
    let (_id, def) = &ast.definitions[0];
    dbg!(def);
    assert_eq!(def.arguments.as_ref().map(|it| it.len()), None);
    let (_id, def) = &ast.definitions[1];
    dbg!(def);
    assert_eq!(def.arguments.as_ref().map(|it| it.len()), None);
    let (_id, def) = &ast.definitions[2];
    dbg!(def);
    assert_eq!(def.arguments.as_ref().map(|it| it.len()), Some(2));
    Ok(())
}

#[test]
fn parse_add_assign() -> Result<(), TError> {
    let ast = setup("x+=1")?;
    // dbg!(&ast);

    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.ops.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_tuple() -> Result<(), TError> {
    let ast = setup("[1,2]")?;
    assert_eq!(ast.identifiers.len(), 0);
    assert_eq!(ast.literals.len(), 2);
    assert_eq!(ast.ops.len(), 1);
    assert_eq!(ast.definitions.len(), 0);
    Ok(())
}

#[test]
fn parse_ambiguous_and_with_or() -> Result<(), TError> {
    let err = setup("a||b&&c");
    dbg!(&err);
    assert!(err.is_err());
    let err = err.unwrap_err();
    assert_eq!(format!("{err}"), "This expression could be read two ways, use parens to clarify whether || or && should be performed first");
    Ok(())
}

#[test]
fn parse_bare_lambda() -> Result<(), TError> {
    let _ast = setup("x->x")?;
    Ok(())
}

#[test]
fn parse_var_and_use_as_lambdas() -> Result<(), TError> {
    let _ast = setup("(x->x)(x=2)")?;
    Ok(())
}

#[test]
fn parse_var_from_expr_and_use_as_lambdas() -> Result<(), TError> {
    let _ast = setup("(x->x)(x=3+2)")?;
    Ok(())
}

#[test]
fn parse_nested_vars_as_lambdas() -> Result<(), TError> {
    let _ast = setup("(x->x)(x=((y->(2*y))(y=3)))")?;
    Ok(())
}

#[test]
fn parse_operator_precedence_allowed_comparison_of_expressions1() {
    let _ast = setup("a * b + c == foo & a").expect("Disallowed syntax");
}

#[test]
fn parse_operator_precedence_allowed_comparison_of_expressions2() {
    let _ast = setup("array + 32 < ~a | b").expect("Disallowed syntax");
}

// #[test]
// fn parse_operator_precedence_allowed_comparison_of_expressions_and_container_access() {
//     let _ast = setup("array[a] + 32 < ~a | b").expect("Disallowed syntax");
// }

#[test]
fn parse_operator_precedence_allowed_logic_operators() {
    let _ast = setup("a && (!b || c)").expect("Disallowed syntax");
}

#[test]
// #[should_panic] // TODO(errors): Implement!
fn parse_operator_precedence_disallowed1() {
    setup("a & b + c").expect("Disallowed syntax");
}

#[test]
// #[should_panic] // TODO(errors): Implement!
fn parse_operator_precedence_disallowed2() {
    setup("a << b + 1").expect("Disallowed syntax");
}

#[test]
fn parse_atom_atom_in_file() {
    setup("$a $b").expect("Top level allowed syntax");
}

#[test]
#[should_panic] // TODO(errors): Implement!
fn parse_atom_atom_in_expr() {
    setup("($a $b)").expect("Disallowed syntax");
}

#[test]
fn parse_empty_file() {
    setup("").expect("Should succeed on empty file");
}

#[test]
fn parse_values_in_fn_args() -> Result<(), TError> {
    let ast = setup("signum(x)=if(x<0, -1, 1)")?;
    // dbg!(ast);
    assert_eq!(ast.calls.len(), 1);
    assert_eq!(ast.calls[0].1.args.len(), 3);
    assert_eq!(ast.ops.len(), 2); // Lt and Sub
    assert_eq!(ast.definitions.len(), 2);
    Ok(())
}

#[test]
fn parse_indexing_exprs() -> Result<(), TError> {
    let ast = setup("[1, 2, 3][0]")?;
    // dbg!(ast);
    assert_eq!(ast.literals.len(), 4);
    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.ops.len(), 2); // [ (array construction), [ (indexing)
    assert_eq!(ast.definitions.len(), 0);
    Ok(())
}

#[test]
fn parse_forall_name() -> Result<(), TError> {
    let ast = setup("forall T: AllowedType")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.identifiers.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_forall_name_arg() -> Result<(), TError> {
    let ast = setup("f(forall T: AllowedType) = 1")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 1);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.definitions.len(), 2);
    Ok(())
}

#[test]
fn parse_forall_name_arg_no_def() -> Result<(), TError> {
    let ast = setup("f(forall T: AllowedType)")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 1);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_forall_name_arg_no_def_as_arg() -> Result<(), TError> {
    let ast = setup("sink(f(forall T: AllowedType))")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 2);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_forall_name_arg_no_def_as_second_arg() -> Result<(), TError> {
    let ast = setup("sink(f(a, forall T: AllowedType))")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 2);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_forall_name_arg_no_def_as_second_arg_with_type() -> Result<(), TError> {
    let ast = setup("sink(f(a, forall T: AllowedType)): Sink(T)")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 3);
    assert_eq!(ast.definitions.len(), 1);
    Ok(())
}

#[test]
fn parse_forall_name_arg_in_def() -> Result<(), TError> {
    let ast = setup("sink(forall T: AllowedType) = Sink(T)")?;

    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 1);
    assert_eq!(ast.definitions.len(), 2);
    Ok(())
}

#[test]
fn parse_arg_with_default() -> Result<(), TError> {
    let ast = setup("f(forall A = B) = A")?;
    // dbg!(ast);
    assert_eq!(ast.literals.len(), 0);
    assert_eq!(ast.ops.len(), 0);
    assert_eq!(ast.calls.len(), 0);
    assert_eq!(ast.definitions.len(), 2);
    Ok(())
}
/*
TODO(testing): Type annotations:
    - "12 : Int"
    - "3 * 4 : Int"
    - "3 * (4 : Int)"
    - "(3 * 4) : 12"
    - "\"hello world\" : String"

TODO(testing): String literals:
    - "\"hello world\""

    TODO(testing): Numeric literals:
    - "-12"

TODO(testing): Operations:
    - "14-12"
    - "\"hello\"+\" world\""

TODO(testing): Errors:
    - "\"hello world\"\n7"

TODO(testing): Definitions:
    - "f(arg=\"hello world\")"
    - "mul(x, y)= x*y"
    - "x()= !\"hello world\";\n7"
*/
