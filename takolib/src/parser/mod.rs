pub mod semantics;
pub mod tokens;
// use rand::Rng;
use crate::error::TError;
use crate::location::Location;
use crate::string_interner::Identifier;
use crate::{ast::*, parser::semantics::Literal};
use log::trace;
use semantics::BindingMode;
use std::path::Path;
use thiserror::Error;
use tokens::{assign_op, binding_mode_operation, is_assign, OpBinding, Symbol, Token, TokenType};

#[derive(Debug, Error, PartialEq, Eq, Ord, PartialOrd, Clone, Hash)]
pub enum ParseError {
    UnexpectedEof, // TODO: Add context.
    UnexpectedTokenTypeExpectedOperator {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenTypeExpectedAssignment {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenType {
        got: TokenType,
        location: Location,
        expected: TokenType,
    },
    UnexpectedTokenTypeInExpression {
        got: TokenType,
        location: Location,
    },
    ParseIntError {
        message: String,
    },
    AmbiguousExpression {
        left: Symbol,
        right: Symbol,
        location: Location,
    },
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(error: std::num::ParseIntError) -> Self {
        ParseError::ParseIntError {
            message: error.to_string(),
        }
    }
}

impl ParseError {
    pub fn location(&self) -> Option<&Location> {
        match self {
            ParseError::UnexpectedEof => None,
            ParseError::UnexpectedTokenTypeExpectedOperator { got: _, location } => Some(location),
            ParseError::UnexpectedTokenTypeExpectedAssignment { got: _, location } => {
                Some(location)
            }
            ParseError::UnexpectedTokenType {
                got: _,
                location,
                expected: _,
            } => Some(location),
            ParseError::UnexpectedTokenTypeInExpression { got: _, location } => Some(location),
            ParseError::AmbiguousExpression { left: _, right: _, location } => Some(location),
            ParseError::ParseIntError { .. } => None,
        }
    }
}

impl From<ParseError> for TError {
    fn from(err: ParseError) -> Self {
        TError::ParseError(err)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected eof"),
            ParseError::UnexpectedTokenTypeExpectedOperator { got, .. } => {
                write!(f, "Unexpected {got} expected an operator")
            }
            ParseError::UnexpectedTokenTypeExpectedAssignment { got, .. } => {
                write!(f, "Unexpected {got} expected an assignment")
            }
            ParseError::UnexpectedTokenType { got, expected, .. } => {
                write!(f, "Unexpected {got} expected {expected}")
            }
            ParseError::UnexpectedTokenTypeInExpression { got, .. } => {
                write!(f, "Unexpected {got} in expression")
            }
            ParseError::ParseIntError { message, .. } => {
                write!(f, "{message} expected an integer literal (r.g. 123)")
            }
            ParseError::AmbiguousExpression { left, right, .. } => {
                write!(f, "This expression could be read two ways, use parens to clarify whether {left} or {right} should be performed first")
            }
        }
    }
}

#[derive(Debug)]
enum BindingOrValue {
    Binding(Binding),
    Value(NodeId),
}

#[derive(Debug)]
struct ParseState<'src, 'toks, T: Iterator<Item = &'toks Token>> {
    contents: &'src str,
    ast: Ast,
    tokens: std::iter::Peekable<T>,
}

impl<'src, 'toks, T: Iterator<Item = &'toks Token>> ParseState<'src, 'toks, T> {
    fn peek(&mut self) -> Result<&Token, ParseError> {
        self.tokens.peek().copied().ok_or(ParseError::UnexpectedEof)
    }
    fn token(&mut self) -> Option<Token> {
        self.tokens.next().copied()
    }
    fn peek_kind(&mut self) -> Result<TokenType, ParseError> {
        self.peek().map(|tok| tok.kind)
    }
    fn token_if<OnTok>(
        &mut self,
        test: impl FnOnce(&Token) -> Result<OnTok, ParseError>,
    ) -> Result<OnTok, ParseError> {
        let tk = self.peek()?;
        let res = test(tk)?;
        self.token()
            .expect("Internal error: Token missing after check");
        Ok(res)
    }
    fn token_of_type(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        self.token_if(|got| {
            if got.kind == expected {
                Ok(*got)
            } else {
                Err(ParseError::UnexpectedTokenType {
                    got: got.kind,
                    location: got.location(),
                    expected,
                })
            }
        })
    }
    fn ident(&mut self) -> Result<Token, ParseError> {
        self.token_of_type(TokenType::Ident)
    }
    fn operator_is(&mut self, sym: Symbol) -> Result<Token, ParseError> {
        self.token_of_type(TokenType::Op(sym))
    }
    fn has_type(&mut self) -> Result<Token, ParseError> {
        self.operator_is(Symbol::HasType)
    }
    fn assignment_op(&mut self, binding: Symbol) -> Result<Symbol, ParseError> {
        self.token_if(|got| match got.kind {
            TokenType::Op(assign) if is_assign(assign) && binding.is_looser(assign) => Ok(assign),
            _ => Err(ParseError::UnexpectedTokenTypeExpectedAssignment {
                got: got.kind,
                location: got.location(),
            }),
        })
    }

    fn get_kind(&mut self, token: &Token) -> TokenType {
        if token.kind == TokenType::Ident {
            // TODO(clarity): This should be done in the Tokenizer
            let src = token.get_src(self.contents);
            let name = self.ast.string_interner.register_str(src);
            normalize_keywords_as_ops(&self.ast, name)
        } else {
            token.kind
        }
    }

    fn require(&mut self, expected_token: TokenType) -> Result<(), TError> {
        let _token = self.token_of_type(expected_token)?;
        Ok(())
    }

    fn binding(&mut self, default_mode: BindingMode) -> Result<Option<Binding>, TError> {
        let mode = if let Ok(binding) = self.peek().cloned() {
            let binding_kind = self.get_kind(&binding);
            // TODO: Handle tokens...
            if let TokenType::Op(binding) = binding_kind {
                if let Some(mode) = binding_mode_operation(binding) {
                    self.token(); // Consume the mode.
                    mode
                } else {
                    trace!("Wrong op for binding");
                    return Ok(None);
                }
            } else {
                // Named arg!
                trace!("Named arg?");
                default_mode
            }
        } else {
            trace!("Unexpected eof when looking for binding");
            return Ok(None);
        };
        let name: Identifier = if let Ok(tok) = self.ident() {
            self.name(tok)
        } else {
            trace!("No name found for binding");
            return Ok(None);
        };
        let ty = if self.has_type().is_ok() {
            Some(self.expr(Symbol::HasType)?)
        } else {
            None
        };
        Ok(Some(Binding { mode, name, ty }))
    }

    fn binding_or_arg(
        &mut self,
        has_non_bind_args: &mut bool,
        default_mode: BindingMode,
    ) -> Result<BindingOrValue, TError> {
        if let Some(binding) = self.binding(default_mode)? {
            trace!("Binding: {binding:?}");
            return Ok(BindingOrValue::Binding(binding));
        }
        *has_non_bind_args = true;
        let value = self.expr(Symbol::Comma)?;
        trace!("Arg value: {value:?}");
        Ok(BindingOrValue::Value(value))
    }

    fn call_or_definition(&mut self, name: Token, binding: Symbol) -> Result<NodeId, TError> {
        let name_id = self.name(name);
        trace!(
            "Call or definition: {name:?}: {:?}",
            self.ast.string_interner.get_str(name_id)
        );
        let location = name.location();
        let mut bindings = vec![];
        let mut _has_implicit_args = false;
        let mut has_args = false;
        let mut has_non_bind_args = false; // i.e. this should be a definition...
        if self.operator_is(Symbol::Lt).is_ok() {
            trace!("has implicit arguments");
            _has_implicit_args = true;
            // Read implicit args...
            while self.operator_is(Symbol::Gt).is_err() {
                bindings.push(self.binding_or_arg(&mut has_non_bind_args, BindingMode::Pi)?);
                if self.require(TokenType::Op(Symbol::Comma)).is_err() {
                    self.require(TokenType::Op(Symbol::Gt))?;
                    break;
                }
            }
        }
        if self.operator_is(Symbol::OpenParen).is_ok() {
            trace!("has arguments");
            has_args = true;
            // Read args...
            while self.operator_is(Symbol::CloseParen).is_err() {
                bindings.push(self.binding_or_arg(&mut has_non_bind_args, BindingMode::Lambda)?);
                if self.require(TokenType::Op(Symbol::Comma)).is_err() {
                    self.require(TokenType::Op(Symbol::CloseParen))?;
                    break;
                }
            }
        }
        let ty = if self.has_type().is_ok() {
            trace!("HasType started");
            let ty = self.expr(Symbol::HasType)?;
            trace!("HasType finished");
            Some(ty)
        } else {
            None
        };
        if let Ok(assignment) = self.assignment_op(binding) {
            let op = assign_op(assignment);
            let mut implementation = self.expr(assignment)?;
            if let Some(op) = op {
                let name = self.identifier(name, location);
                implementation = self.ast.add_op(
                    Op {
                        op,
                        args: vec![name, implementation],
                    },
                    location,
                );
            }
            let name = &self.contents
                [(location.start as usize)..(location.start as usize) + (location.length as usize)];
            let name = self
                .ast
                .string_interner
                .register_str_by_loc(name, location.start);
            let mut only_bindings = vec![];
            for binding in bindings {
                match binding {
                    BindingOrValue::Binding(binding) => only_bindings.push(binding),
                    BindingOrValue::Value(value) => {
                        todo!("Don't know how to convert {value:?} to binding.")
                    }
                }
            }
            let bindings = if has_args { Some(only_bindings) } else { None };
            // TODO: USE bindings
            trace!("Add definition");
            if has_non_bind_args {
                todo!("Concern");
            }
            let def = self.ast.add_definition(
                Definition {
                    name,
                    bindings,
                    implementation,
                },
                location,
            );
            if let Some(ty) = ty {
                return Ok(self.ast.add_annotation(def, ty));
            }
            return Ok(def);
        }
        if has_args {
            let inner = self.identifier(name, location);
            // TODO: USE bindings
            trace!("Add call");
            let call = self.ast.add_call(
                Call {
                    inner,
                    args: vec![],
                },
                location,
            );
            if let Some(ty) = ty {
                return Ok(self.ast.add_annotation(call, ty));
            }
            return Ok(call);
        }
        trace!("Add ident");
        let ident = self.identifier(name, location);
        if let Some(ty) = ty {
            return Ok(self.ast.add_annotation(ident, ty));
        }
        Ok(ident)
    }

    fn expr(&mut self, binding: Symbol) -> Result<NodeId, TError> {
        let Ok(token) = self.peek().copied() else {
            return Err(ParseError::UnexpectedEof.into());
        };
        let location = token.location();
        trace!("Expr: {token:?} (binding {binding:?})");
        let mut left = if self.operator_is(Symbol::OpenBracket).is_ok() {
            // TODO: Support tuples.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::Sequence)?;
            self.require(TokenType::Op(Symbol::CloseBracket))?;
            left
        } else if self.operator_is(Symbol::OpenCurly).is_ok() {
            // TODO: Support sequence&dictionary syntax.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::Sequence)?;
            self.require(TokenType::Op(Symbol::CloseCurly))?;
            left
        } else if self.operator_is(Symbol::OpenParen).is_ok() {
            // TODO: Support list syntax.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::Sequence)?;
            self.require(TokenType::Op(Symbol::CloseParen))?;
            left
        } else if let TokenType::Op(symbol) = self.get_kind(&token) {
            if let Some(def_binding) = self.binding(BindingMode::Lambda)? {
                trace!("Binding? {def_binding:?}");
                self.ast.add_binding(def_binding, location)
            } else {
                let _ = self.token();
                match symbol.binding() {
                    OpBinding::Open => todo!("Should have already been handled"),
                    OpBinding::PrefixOp => {
                        let right = self.expr(binding)?;
                        self.ast.add_op(
                            Op {
                                op: symbol,
                                args: vec![right],
                            },
                            location,
                        )
                    }
                    _ => todo!("Operator {symbol} needs a 'left' side."),
                }
            }
        } else if let Ok(token) = self.ident() {
            self.call_or_definition(token, binding)?
        } else if let Ok(token) = self.token_of_type(TokenType::Atom) {
            self.atom(token, location)
        } else if let Ok(token) = self.token_of_type(TokenType::NumLit) {
            self.number_literal(token, location)
        } else {
            return Err(ParseError::UnexpectedTokenTypeInExpression {
                got: token.kind,
                location,
            }
            .into());
        };
        trace!("Maybe continue: {left:?} (binding {binding:?})");
        while let Ok(TokenType::Op(sym)) = self.peek_kind() {
            if sym.binding() == OpBinding::Close {
                trace!("Closing Expr: {left:?} sym: {sym:?}");
                break;
            }
            if sym != binding && sym.is_looser(binding) && binding.is_looser(sym) {
                // If both can be inside the other
                // and theyre not associative...
                // then this is ambiguous and needs parens.
                return Err(ParseError::AmbiguousExpression {
                    left: binding,
                    right: sym,
                    location,
                }
                .into());
            }
            if !binding.is_looser(sym) {
                trace!("Back up Expr: {left:?} binding: {binding:?} inside sym: {sym:?}");
                break;
            }
            trace!("Continuing Expr: {left:?} sym: {sym:?} inside binding: {binding:?}");
            let token = self.token().expect("Internal error");
            let location = token.location();
            let right = self.expr(sym)?;
            // TODO: Check that this is the right kind of operator.
            left = self.ast.add_op(
                Op {
                    op: sym,
                    args: vec![left, right],
                },
                location,
            );
        }
        trace!("Expr done: {}", self.ast.pretty(left));
        Ok(left)
    }

    fn name(&mut self, res: Token) -> Identifier {
        assert!(res.kind == TokenType::Ident);
        let name = res.get_src(self.contents);
        trace!("Name: {name}");
        self.ast
            .string_interner
            .register_str_by_loc(name, res.location().start)
    }

    fn identifier(&mut self, res: Token, location: Location) -> NodeId {
        assert!(res.kind == TokenType::Ident);
        let name = res.get_src(self.contents);
        trace!("Identifier: {name}");
        let name = self.ast.string_interner.register_str(name);
        self.ast.add_identifier(name, location)
    }

    fn atom(&mut self, res: Token, location: Location) -> NodeId {
        assert!(res.kind == TokenType::Atom);
        let name = res.get_src(self.contents);
        trace!("Atom: {name}");
        let name = self.ast.string_interner.register_str(name);
        self.ast.add_atom(Atom { name }, location)
    }

    fn number_literal(&mut self, res: Token, location: Location) -> NodeId {
        assert!(res.kind == TokenType::NumLit);
        trace!("Saving literal: {res:?}");
        let _id = self
            .ast
            .string_interner
            .register_str_by_loc(res.get_src(self.contents), location.start);
        self.ast.add_literal(Literal::Numeric, location)
    }
}

pub fn parse(filepath: &Path, contents: &str, tokens: &[Token]) -> Result<Ast, TError> {
    trace!("Parse {}: {:?}", filepath.display(), &tokens);
    let mut state = ParseState {
        contents,
        ast: Ast::new(filepath.to_path_buf()),
        tokens: tokens.iter().peekable(),
    };
    if !tokens.is_empty() {
        // Support empty files!
        let root = state.expr(Symbol::OpenParen)?;
        state.ast.roots.push(root);
    }
    // TODO(testing): REMOVE THIS (it's just to test the threading model)
    // let mut rng = rand::thread_rng();
    // std::thread::sleep(std::time::Duration::from_secs(rng.gen_range(0..10)));
    assert!(state.tokens.next().is_none(), "Left over tokens");
    Ok(state.ast)
}

fn normalize_keywords_as_ops(ast: &Ast, name: Identifier) -> TokenType {
    let interner = &ast.string_interner;
    let op = if name == interner.lambda {
        Symbol::Lambda
    } else if name == interner.pi {
        Symbol::Pi
    } else if name == interner.forall {
        Symbol::Forall
    } else if name == interner.exists {
        Symbol::Exists
    } else {
        return TokenType::Ident;
    };
    TokenType::Op(op)
}

#[cfg(test)]
pub mod tests {
    use super::semantics::*;
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
            )],
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
            ],
            "Should have parsed a number"
        );
        assert_eq!(ast.ops.len(), 1);
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
        assert_eq!(format!("{}", err), "Unexpected eof");
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
        assert_eq!(ast.bindings.len(), 1);
        Ok(())
    }

    #[test]
    fn parse_lambda_keyword() -> Result<(), TError> {
        let ast = setup("lambda x -> x")?;
        // dbg!(&ast);

        assert_eq!(ast.ops.len(), 1);
        assert_eq!(ast.identifiers.len(), 1);
        assert_eq!(ast.bindings.len(), 1);
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
        // dbg!(&ast);

        assert_eq!(ast.identifiers.len(), 2);
        assert_eq!(ast.atoms.len(), 0);
        assert_eq!(ast.literals.len(), 1);
        assert_eq!(ast.ops.len(), 0);
        assert_eq!(ast.definitions.len(), 1);
        Ok(())
    }

    #[test]
    fn parsed_assignment_with_implicit_args() -> Result<(), TError> {
        let ast = setup("id<T: Type>(y: T): T=y")?;
        eprintln!("{}", &ast.pretty(ast.roots[0]));

        assert_eq!(ast.identifiers.len(), 4);
        assert_eq!(ast.atoms.len(), 0);
        assert_eq!(ast.literals.len(), 0);
        assert_eq!(ast.ops.len(), 0);
        assert_eq!(ast.definitions.len(), 1);
        let (_id, def) = &ast.definitions[0];
        dbg!(def);
        assert_eq!(def.bindings.as_ref().map(|x| x.len()), Some(2));
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
        let ast = setup("1,2")?;
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
        assert_eq!(format!("{}", err), "This expression could be read two ways, use parens to clarify whether || or && should be performed first");
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
    #[should_panic] // TODO(errors): Implement!
    fn parse_atom_atom() {
        setup("$a $b").expect("Disallowed syntax");
    }

    #[test]
    fn parse_empty_file() {
        setup("").expect("Should succeed on empty file");
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
