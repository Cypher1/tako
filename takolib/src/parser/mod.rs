pub mod semantics;
pub mod tokens;
use crate::ast::location::Location;
use crate::ast::string_interner::Identifier;
use crate::ast::{Ast, Atom, Call, Contains, Definition, NodeData, NodeId, Op};
use crate::error::TError;
use better_std::include_strs;
use log::trace;
use semantics::BindingMode;
use semantics::Literal;
use smallvec::smallvec;
use std::path::Path;
use thiserror::Error;
use tokens::{assign_op, binding_mode_operation, is_assign, OpBinding, Symbol, Token, TokenType};

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

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
        location: Option<Location>,
    },
    AmbiguousExpression {
        left: Symbol,
        right: Symbol,
        location: Location,
    },
    UnexpectedExpressionInDefinitionArguments {
        arg: NodeId,
        arg_str: String,
        location: Location,
    },
    MissingLeftHandSideOfOperator {
        op: Symbol,
        location: Location,
    },
    UnparsedTokens {
        token: TokenType,
        location: Location,
    },
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(error: std::num::ParseIntError) -> Self {
        Self::ParseIntError {
            message: error.to_string(),
            location: None,
        }
    }
}

impl ParseError {
    #[must_use]
    pub fn location(&self) -> Option<&Location> {
        match self {
            Self::UnexpectedEof => None,
            Self::ParseIntError { location, .. } => location.as_ref(),
            Self::UnexpectedTokenTypeExpectedOperator { got: _, location }
            | Self::UnexpectedTokenTypeExpectedAssignment { got: _, location }
            | Self::UnexpectedTokenType {
                got: _,
                location,
                expected: _,
            }
            | Self::UnexpectedTokenTypeInExpression { got: _, location }
            | Self::AmbiguousExpression {
                left: _,
                right: _,
                location,
            }
            | Self::UnexpectedExpressionInDefinitionArguments { location, .. }
            | Self::MissingLeftHandSideOfOperator { location, .. } => Some(location),
            Self::UnparsedTokens { location, .. } => Some(location),
        }
    }
}

impl From<ParseError> for TError {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "Unexpected eof"),
            Self::UnexpectedTokenTypeExpectedOperator { got, .. } => {
                write!(f, "Unexpected {got} expected an operator")
            }
            Self::UnexpectedTokenTypeExpectedAssignment { got, .. } => {
                write!(f, "Unexpected {got} expected an assignment")
            }
            Self::UnexpectedTokenType { got, expected, .. } => {
                write!(f, "Unexpected {got} expected {expected}")
            }
            Self::UnexpectedTokenTypeInExpression { got, .. } => {
                write!(f, "Unexpected {got} in expression")
            }
            Self::ParseIntError { message, .. } => {
                write!(f, "{message} expected an integer literal (r.g. 123)")
            }
            Self::AmbiguousExpression { left, right, .. } => {
                write!(f, "This expression could be read two ways, use parens to clarify whether {left} or {right} should be performed first")
            }
            Self::UnexpectedExpressionInDefinitionArguments { arg_str, .. } => {
                write!(f, "Don't know how to convert '{arg_str}' to binding.")
            }
            Self::MissingLeftHandSideOfOperator { op, .. } => {
                write!(
                    f,
                    "Operator {op} needs a 'left' side. (e.g. 'a{op}b' rather than '{op}b'"
                )
            }
            Self::UnparsedTokens { token, .. } => {
                write!(f, "Failed to parse the whole file. Found {token}")
            }
        }
    }
}

#[derive(Debug)]
enum BindingOrValue {
    Identifier(Identifier, Option<NodeId>, Location),
    Binding(NodeId),
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

    fn binding(&mut self) -> Result<Option<NodeId>, TError> {
        let Ok(binding) = self.peek().cloned() else {
            trace!("Unexpected eof when looking for binding");
            return Ok(None);
        };
        let binding_kind = self.get_kind(&binding);
        let binding = if let TokenType::Op(binding) = binding_kind {
            binding
        } else {
            Symbol::OpenParen
        };
        let mode = if let TokenType::Op(binding) = binding_kind {
            let Some(mode) = binding_mode_operation(binding) else {
                trace!("Wrong op for binding");
                return Ok(None);
            };
            self.token(); // Consume the mode.
            mode
        } else {
            // Named arg!
            trace!("Named arg?");
            BindingMode::Lambda
        };
        let Ok(tok) = self.ident() else {
            trace!("No name found for binding");
            return Ok(None);
        };
        let name = self.name(tok);
        let location = tok.location();
        let bindings = None; // TODO: Handling bindings...
        let ty = if self.has_type().is_ok() {
            Some(self.expr(Symbol::HasType)?)
        } else {
            None
        };
        let implementation = if self.assignment_op(binding).is_ok() {
            Some(self.expr(Symbol::Comma)?)
        } else {
            None
        };
        let def = self.ast.add_definition(
            Definition {
                mode,
                name,
                bindings,
                implementation,
            },
            location,
        );
        if let Some(ty) = ty {
            self.ast.add_annotation(def, ty);
        }
        Ok(Some(def))
    }

    fn binding_or_arg(&mut self, has_non_bind_args: &mut bool) -> Result<BindingOrValue, TError> {
        let value = self.expr(Symbol::Comma)?;
        let node = &self.ast.get(value);
        let location = node.location;
        if let NodeData::Definition(binding) = node.id {
            trace!("Definition: {binding:?}");
            return Ok(BindingOrValue::Binding(value));
        }
        if let NodeData::Identifier(ident) = node.id {
            let ty = node.ty;
            let (_node_id, ident) = self.ast.get_mut(ident);
            // TODO: Try for an assignment binding...
            return Ok(BindingOrValue::Identifier(*ident, ty, location));
        }
        *has_non_bind_args = true;
        trace!("Arg value: {value:?}");
        Ok(BindingOrValue::Value(value))
    }

    fn handle_bindings(&mut self, bindings: Vec<BindingOrValue>) -> Result<Vec<NodeId>, TError> {
        let mut args = vec![];
        for binding in bindings {
            let binding = match binding {
                BindingOrValue::Binding(binding) => binding,
                BindingOrValue::Identifier(ident, ty, location) => {
                    let ident = self.ast.add_identifier(ident, location);
                    if let Some(ty) = ty {
                        self.ast.add_annotation(ident, ty);
                    }
                    ident
                }
                BindingOrValue::Value(value) => value,
            };
            args.push(binding);
        }
        Ok(args)
    }

    fn call_or_definition(&mut self, name: Token, binding: Symbol) -> Result<NodeId, TError> {
        let name_id = self.name(name);
        trace!(
            "Call or definition: {name:?}: {:?}",
            self.ast.string_interner.get_str(name_id)
        );
        let location = name.location();
        let mut bindings = vec![];
        let mut has_args = false;
        let mut has_non_bind_args = false; // i.e. this should be a definition...
        if self.operator_is(Symbol::OpenParen).is_ok() {
            trace!("has arguments");
            has_args = true;
            // Read args...
            while self.operator_is(Symbol::CloseParen).is_err() {
                bindings.push(self.binding_or_arg(&mut has_non_bind_args)?);
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
                        args: smallvec![name, implementation],
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
                    BindingOrValue::Identifier(name, ty, _location) => {
                        let def = self.ast.add_definition(
                            Definition {
                                mode: BindingMode::Lambda,
                                name,
                                bindings: None,
                                implementation: None,
                            },
                            location,
                        );
                        if let Some(ty) = ty {
                            self.ast.add_annotation(def, ty);
                        }
                        only_bindings.push(def);
                    }
                    BindingOrValue::Value(value) => {
                        return Err(ParseError::UnexpectedExpressionInDefinitionArguments {
                            arg: value,
                            arg_str: format!("{}", self.ast.pretty_node(value)),
                            location,
                        }
                        .into())
                    }
                }
            }
            let bindings = if has_args {
                Some(only_bindings.into())
            } else {
                None
            };
            // TODO: USE bindings
            trace!("Add definition");
            if has_non_bind_args {
                todo!("Concern");
            }
            let def = self.ast.add_definition(
                Definition {
                    name,
                    mode: BindingMode::Lambda,
                    bindings,
                    implementation: Some(implementation),
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
            let args = self.handle_bindings(bindings)?.into();
            let call = self.ast.add_call(Call { inner, args }, location);
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
        let Ok(mut token) = self.peek().copied() else {
            return Err(ParseError::UnexpectedEof.into());
        };
        token.kind = self.get_kind(&token);
        let location = token.location();
        trace!("Expr: {token:?} (binding {binding:?})");
        let mut left = if self.operator_is(Symbol::OpenBracket).is_ok() {
            // TODO: Support tuples.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::OpenParen)?;
            self.require(TokenType::Op(Symbol::CloseBracket))?;
            left
        } else if self.operator_is(Symbol::OpenCurly).is_ok() {
            // TODO: Support sequence&dictionary syntax.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::OpenParen)?;
            self.require(TokenType::Op(Symbol::CloseCurly))?;
            left
        } else if self.operator_is(Symbol::OpenParen).is_ok() {
            // TODO: Support list syntax.
            // Tuple, parenthesized expr... etc.
            let left = self.expr(Symbol::OpenParen)?;
            self.require(TokenType::Op(Symbol::CloseParen))?;
            left
        } else if let TokenType::Op(symbol) = token.kind {
            if let Some(def_binding) = self.binding()? {
                trace!("Binding? {def_binding:?}");
                def_binding
            } else {
                let _ = self.token();
                match symbol.binding_type() {
                    OpBinding::Open => todo!("Should have already been handled"),
                    OpBinding::PrefixOp | OpBinding::PrefixOrInfixBinOp => {
                        let right = self.expr(binding)?;
                        self.ast.add_op(
                            Op {
                                op: symbol,
                                args: smallvec![right],
                            },
                            location,
                        )
                    }
                    _ => {
                        return Err(ParseError::MissingLeftHandSideOfOperator {
                            op: symbol,
                            location,
                        }
                        .into())
                    }
                }
            }
        } else if let Ok(token) = self.ident() {
            self.call_or_definition(token, binding)?
        } else if let Ok(token) = self.token_of_type(TokenType::Atom) {
            self.atom(token, location)
        } else if let Ok(token) = self.token_of_type(TokenType::NumberLit) {
            self.number_literal(token, location)
        } else if let Ok(token) = self.token_of_type(TokenType::StringLit) {
            self.string_literal(token, location)
        } else {
            return Err(ParseError::UnexpectedTokenTypeInExpression {
                got: token.kind,
                location,
            }
            .into());
        };
        trace!("Maybe continue: {left:?} (binding {binding:?})");
        while let Ok(TokenType::Op(sym)) = self.peek_kind() {
            if sym.binding_type() == OpBinding::Close {
                trace!("Closing Expr: {left:?} sym: {sym:?}");
                break;
            }
            if sym != binding && sym.is_looser(binding) && binding.is_looser(sym) {
                // If both can be inside the other
                // and they're not associative...
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
            if sym == Symbol::OpenParen {
                let token = self.token().expect("Internal error");
                let location = token.location();
                // Require an 'apply' to balance it's parens.
                let mut args = vec![];
                let mut _has_non_bind_args = false;
                while self.operator_is(Symbol::CloseParen).is_err() {
                    let arg = self.binding_or_arg(&mut _has_non_bind_args)?;
                    args.push(arg);
                }
                let args = self.handle_bindings(args)?.into();
                left = self.ast.add_call(Call { inner: left, args }, location);
            } else {
                // TODO: Check that this is the right kind of operator.
                let token = self.token().expect("Internal error");
                let right = self.expr(sym)?;
                let location = token.location();
                left = self.ast.add_op(
                    Op {
                        op: sym,
                        args: smallvec![left, right],
                    },
                    location,
                );
            }
        }
        trace!("Expr done: {}", self.ast.pretty_node(left));
        trace!("(next token: {:?})", self.peek());
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

    fn string_literal(&mut self, res: Token, location: Location) -> NodeId {
        assert!(res.kind == TokenType::StringLit);
        trace!("Saving literal: {res:?}");
        let _id = self
            .ast
            .string_interner
            .register_str_by_loc(res.get_src(self.contents), location.start);
        self.ast.add_literal(Literal::Text, location)
    }

    fn number_literal(&mut self, res: Token, location: Location) -> NodeId {
        assert!(res.kind == TokenType::NumberLit);
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
        state.ast.set_root(root);
    }
    // TODO(testing): REMOVE THIS (it's just to test the threading model)
    // let mut rng = rand::thread_rng();
    // std::thread::sleep(std::time::Duration::from_secs(rng.gen_range(0..10)));
    if let Some(token) = state.tokens.next() {
        Err(ParseError::UnparsedTokens {
            token: token.kind,
            location: token.location(),
        })?;
    }
    Ok(state.ast)
}

fn normalize_keywords_as_ops(ast: &Ast, name: Identifier) -> TokenType {
    let interner = &ast.string_interner;
    let op = if name == interner.kw_lambda {
        Symbol::Lambda
    } else if name == interner.kw_pi {
        Symbol::Pi
    } else if name == interner.kw_forall {
        Symbol::Forall
    } else if name == interner.kw_exists {
        Symbol::Exists
    } else {
        return TokenType::Ident;
    };
    TokenType::Op(op)
}

#[cfg(test)]
pub mod tests {
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
        assert_eq!(def.bindings.as_ref().map(|it| it.len()), None);
        let (_id, def) = &ast.definitions[1];
        dbg!(def);
        assert_eq!(def.bindings.as_ref().map(|it| it.len()), None);
        let (_id, def) = &ast.definitions[2];
        dbg!(def);
        assert_eq!(def.bindings.as_ref().map(|it| it.len()), Some(2));
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
    #[should_panic] // TODO(errors): Implement!
    fn parse_atom_atom() {
        setup("$a $b").expect("Disallowed syntax");
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
