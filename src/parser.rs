// use rand::Rng;
use crate::ast::*;
use crate::error::TError;
use crate::tokens::{Symbol, Token, TokenType};
use log::{debug, trace};
use std::path::Path;

use static_assertions::*;
assert_eq_size!(Partial, [u8; 16]); // TODO(perf): Try to get the size down
assert_eq_size!([Partial; 2], [u8; 32]);

pub fn parse(filepath: &Path, contents: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let tokens = tokens.iter().peekable();
    debug!("Parse {}", filepath.display());
    let mut ast = Ast::new(filepath.to_path_buf());
    let root = expr(&mut ast, contents, tokens);
    ast.roots.extend(root);
    // TODO(testing): REMOVE THIS (it's just to test the threading model)
    // let mut rng = rand::thread_rng();
    // std::thread::sleep(std::time::Duration::from_secs(rng.gen_range(0..10)));
    Ok(ast)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BindingPowerConfig {
    prefix: Option<BindingPower>,
    infix: Option<BindingPower>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BindingPower {
    left: u8,
    right: u8,
}
impl BindingPower {
    const fn new(left: u8, right: u8) -> Self {
        Self { left, right }
    }
}

impl BindingPowerConfig {
    const fn infix(left: u8, right: u8) -> Self {
        Self {
            infix: Some(BindingPower::new(left, right)),
            prefix: None,
        }
    }
    const fn prefix(left: u8, right: u8) -> Self {
        Self::infix(left, right).and_prefix(left, right)
    }
    const fn and_prefix(mut self, left: u8, right: u8) -> Self {
        self.prefix = Some(BindingPower::new(left, right));
        self
    }
}

const fn binding_power(symbol: Symbol) -> BindingPowerConfig {
    // TODO(usability): Use binding trees to avoid complex expressions producing unexpected ASTs.
    // TODO(clarity): Use binding trees to avoid arbitrary binding power numbers.
    match symbol {
        Symbol::OpenParen => BindingPowerConfig::prefix(99, 0),
        Symbol::CloseParen => BindingPowerConfig::infix(0, 100),
        Symbol::OpenCurly => BindingPowerConfig::prefix(99, 0),
        Symbol::CloseCurly => BindingPowerConfig::infix(0, 100),
        Symbol::OpenBracket => BindingPowerConfig::prefix(99, 0),
        Symbol::CloseBracket => BindingPowerConfig::infix(0, 100),
        Symbol::Eqs => BindingPowerConfig::infix(2, 1),
        Symbol::Add | Symbol::Sub => BindingPowerConfig::infix(5, 6).and_prefix(99, 9),
        Symbol::Mul | Symbol::Div => BindingPowerConfig::infix(7, 8),
        Symbol::LogicalNot => BindingPowerConfig::prefix(11, 100),
        Symbol::Dot => BindingPowerConfig::infix(14, 13),
        _ => BindingPowerConfig::infix(3, 4),
    }
}

fn get_binding_power(token: Option<&Token>, is_prefix: bool) -> Option<BindingPower> {
    if let Some(Token {
        kind: TokenType::Op(symbol),
        ..
    }) = token
    {
        let power = binding_power(*symbol);
        if is_prefix {
            return power.prefix;
        } else {
            return power.infix;
        }
    }
    Some(BindingPower::new(99, 100))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
struct Partial {
    min_bp: u8,
    node: Option<NodeId>,
    token: Token,
}

fn expr<'a, T: Iterator<Item = &'a Token>>(
    ast: &mut Ast,
    contents: &str,
    mut tokens: std::iter::Peekable<T>,
) -> Option<NodeId> {
    // Following along with https://matklad.github.io/2020/04/15/from-pratt-to-dijkstra.html
    let mut stack: Vec<Partial> = vec![];
    let mut left = Partial {
        min_bp: 0,
        node: None,
        token: Token::eof(0),
    };
    loop {
        let token = tokens.next();
        trace!("Adding token {:?}", &token);
        let r_bp = loop {
            if let Some(BindingPower {
                left: l_bp,
                right: r_bp,
            }) = get_binding_power(token, left.node.is_none())
            {
                trace!(
                    "Found token {:?} with prec left {:?}, right {:?}",
                    &token,
                    &l_bp,
                    &r_bp
                );
                if token.is_some() && left.min_bp <= l_bp {
                    // Symbol joins left and the next expression,
                    // or needs other special handling.
                    break r_bp;
                }
            }
            // Reached the end of a sub expression.
            let res = left;
            left = match stack.pop() {
                Some(it) => it,
                None => {
                    assert!(tokens.next().is_none(), "Left over tokens");
                    trace!("No more stack");
                    return res.node;
                }
            };
            let location = res.token.location();
            let node = match res.token.kind {
                TokenType::NumLit => {
                    trace!("Saving literal: {res:?}");
                    let _id = ast.string_interner.register_str_by_loc(
                        res.token.get_src(contents).to_string(),
                        location.start,
                    );
                    ast.add_literal(Literal::Numeric, location)
                }
                TokenType::Op(symbol) => {
                    use crate::tokens::{is_assign, assign_op};
                    if is_assign(symbol) {
                        // TODO(clarity): Lowering for assign ops.
                        todo!("Assignment\n{symbol:#?}\n{res:#?}\n{left:#?}\n{op:?}", op=assign_op(symbol));
                    }
                    trace!("Merging {res:?} and {left:?} to prep for {token:?}");
                    let args = [left.node, res.node];
                    ast.add_op(Op::new(symbol, args), location)
                }
                TokenType::Atom => {
                    let name = ast.string_interner.register_str(
                        res.token.get_src(contents).to_string()
                    );
                    ast.add_atom(Atom { name }, location)
                }
                TokenType::Sym => {
                    let name = ast.string_interner.register_str(
                        res.token.get_src(contents).to_string()
                    );
                    ast.add_identifier(name, location)
                }
                _ => todo!("Dunno what to do with this one {:?}", res.token),
            };
            left.node = Some(node);
        };
        if let Some(token) = token {
            if token.kind == TokenType::Op(Symbol::CloseParen) {
                trace!("Special case, close paren");
                assert_eq!(left.token.kind, TokenType::Op(Symbol::OpenParen));
                let res = left;
                left = stack.pop().unwrap();
                left.node = res.node;
                continue;
            }
            trace!("New partial from token {token:?}");
            stack.push(left);
            left = Partial {
                min_bp: r_bp,
                node: None,
                token: *token,
            };
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tokens::lex;
    use std::path::PathBuf;

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
        dbg!(&ast);
        let Ast {
            roots: _, literals, ..
        } = ast;

        dbg!(&literals);

        assert_eq!(
            literals,
            vec![(
                NodeId::from_raw(0),
                Literal::Numeric, // ("123".to_string()),
            )],
            "Should have parsed a number"
        );

        Ok(())
    }

    #[test]
    fn parse_add_literals() -> Result<(), TError> {
        let ast = setup("1+2")?;
        dbg!(&ast);
        let Ast {
            calls, literals, ..
        } = ast;

        assert_eq!(
            literals,
            vec![
                (
                    NodeId::from_raw(0),
                    Literal::Numeric, // ("1".to_string()),
                ),
                (
                    NodeId::from_raw(1),
                    Literal::Numeric, // ("2".to_string()),
                )
            ],
            "Should have parsed a number"
        );

        dbg!(calls);
        dbg!(literals);
        Ok(())
    }

    #[test]
    fn parse_add_mul_literals() -> Result<(), TError> {
        let ast = setup("1+2*3")?;
        dbg!(&ast);
        let Ast {
            calls, literals, ..
        } = ast;

        dbg!(calls);
        dbg!(literals);

        Ok(())
    }

    #[test]
    fn parse_mul_add_literals() -> Result<(), TError> {
        let ast = setup("1+2*3")?;
        dbg!(&ast);
        let Ast {
            calls, literals, ..
        } = ast;

        dbg!(calls);
        dbg!(literals);

        Ok(())
    }

    #[test]
    fn parse_atom() -> Result<(), TError> {
        let ast = setup("$x")?;
        dbg!(&ast);
        let Ast {
            atoms, ..
        } = ast;

        dbg!(atoms);

        Ok(())
    }

    #[test]
    fn parse_identifier() -> Result<(), TError> {
        let ast = setup("x")?;
        dbg!(&ast);
        let Ast {
            identifiers, ..
        } = ast;

        dbg!(identifiers);

        Ok(())
    }

    #[test]
    fn parse_definition() -> Result<(), TError> {
        let ast = setup("x=1")?;
        dbg!(&ast);
        let Ast {
            identifiers, literals, definitions, ..
        } = ast;

        dbg!(identifiers);
        dbg!(literals);
        dbg!(definitions);

        Ok(())
    }


    #[test]
    #[should_panic] // TODO(feature): Implement!
    fn parse_add_assign() {
        setup("x+=1").expect("NOT YET SUPPORTED");
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
}
