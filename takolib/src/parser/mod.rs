pub mod semantics;
pub mod tokens;
use crate::ast::location::Location;
use crate::ast::string_interner::{Identifier, StrId};
use crate::ast::{Ast, Atom, Call, Contains, Definition, NodeData, NodeId, Op};
use crate::error::TError;
use better_std::include_strs;
use log::{debug, trace};
use semantics::BindingMode;
use semantics::Literal;
use smallvec::{smallvec, SmallVec};
use std::path::Path;
use thiserror::Error;
use tokens::{
    binding_mode_from_op, is_assign, op_from_assign_op, OpBinding, Symbol, Token, TokenType,
};

const CTX_SIZE: usize = 20;
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
        bind_type: OpBinding,
        location: Location,
    },
    MissingRightHandSideOfOperator {
        op: Symbol,
        bind_type: OpBinding,
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
            Self::MissingRightHandSideOfOperator { location, .. } => Some(location),
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
            Self::MissingRightHandSideOfOperator { op, .. } => {
                write!(
                    f,
                    "Operator {op} needs a 'right' side. (e.g. 'a{op}b' rather than 'a{op}'"
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
    depth: std::rc::Rc<()>,
}

impl<'src, 'toks, T: Iterator<Item = &'toks Token>> ParseState<'src, 'toks, T> {
    fn indent(&self) -> String {
        // TODO: Make optional.
        let depth = std::rc::Rc::strong_count(&self.depth);
        "  ".repeat(depth - 1)
    }
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
        let Some(_) = self.token() else {
            unreachable!()
        };
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

    fn arguments(&mut self) -> Result<Option<SmallVec<BindingOrValue, 2>>, TError> {
        let arguments = if self.operator_is(Symbol::OpenParen).is_ok() {
            let _scope = self.depth.clone();
            debug!(
                "{indent}Function style arguments (parens)",
                indent = self.indent()
            );
            let args = self.repeated(Symbol::CloseParen, |this| this.binding_or_arg())?;
            Some(args)
        } else if self.operator_is(Symbol::OpenBracket).is_ok() {
            let _scope = self.depth.clone();
            debug!(
                "{indent}Index style arguments [brackets]",
                indent = self.indent()
            );
            let args = self.repeated(Symbol::CloseBracket, |this| this.binding_or_arg())?;
            Some(args)
        } else if self.operator_is(Symbol::OpenCurly).is_ok() {
            let _scope = self.depth.clone();
            debug!(
                "{indent}Body style arguments {{curlies}}",
                indent = self.indent()
            );
            let args = self.repeated(Symbol::CloseCurly, |this| this.binding_or_arg())?;
            Some(args)
        } else {
            None
        };
        Ok(arguments)
    }

    fn arguments_as_bindings(
        &mut self,
        arguments: SmallVec<BindingOrValue, 2>,
        location: Location,
    ) -> Result<SmallVec<NodeId, 2>, TError> {
        let mut only_bindings: SmallVec<NodeId, 2> = smallvec![];
        for binding in arguments {
            match binding {
                BindingOrValue::Binding(binding) => only_bindings.push(binding.clone()),
                BindingOrValue::Identifier(name, ty, _location) => {
                    let def = self.ast.add_definition(
                        Definition {
                            mode: BindingMode::Given,
                            name: name.clone(),
                            arguments: None,
                            implementation: None,
                        },
                        location,
                    );
                    if let Some(ty) = ty {
                        self.ast.add_annotation(def, ty.clone());
                    }
                    only_bindings.push(def);
                }
                BindingOrValue::Value(value) => {
                    return Err(ParseError::UnexpectedExpressionInDefinitionArguments {
                        arg: value.clone(),
                        arg_str: format!("{}", self.ast.pretty_node(value.clone())),
                        location,
                    }
                    .into())
                }
            }
        }
        Ok(only_bindings)
    }

    fn call_or_definition(&mut self) -> Result<Option<NodeId>, TError> {
        let Ok(head) = self.peek().cloned() else {
            trace!(
                "{indent}Unexpected eof when looking for binding",
                indent = self.indent()
            );
            return Ok(None);
        };
        let (binding_strength, mode) = if let TokenType::Op(op) = self.get_kind(&head) {
            let Some(mode) = binding_mode_from_op(op) else {
                trace!("{indent}Not a binding Op", indent = self.indent());
                return Ok(None);
            };
            debug!(
                "{indent}Binding mode {head:?} => {mode:?}",
                indent = self.indent()
            );
            self.token(); // Consume the mode.
            (op, mode)
        } else {
            // Named arg!
            trace!("{indent}Named arg?", indent = self.indent());
            (Symbol::OpenParen, BindingMode::Given)
        };
        let Ok(tok) = self.token_of_type(TokenType::Ident) else {
            trace!("{indent}No name found for binding", indent = self.indent());
            return Ok(None);
        };
        let name = self.name(tok);
        let location = tok.location();
        let arguments = self.arguments();
        let ty = if self.has_type().is_ok() {
            trace!("{indent}HasType started", indent = self.indent());
            let ty = self.expr(Symbol::HasType)?;
            trace!("{indent}HasType finished", indent = self.indent());
            Some(ty)
        } else {
            None
        };
        if let Ok(arguments) = arguments {
            if let Ok(assignment) = self.assignment_op(binding_strength) {
                let op = op_from_assign_op(assignment);
                debug!("Start binding definition parse {:?}", name);
                let def_impl = self.any_expr()?;
                debug!("Binding value is {:?}", def_impl);
                let implementation = Some(if let Some(op) = op {
                    let ident = self.identifier(name, location);
                    self.ast.add_op(
                        Op {
                            op,
                            args: smallvec![ident, def_impl],
                        },
                        location,
                    )
                } else {
                    def_impl
                });
                let arguments = if let Some(arguments) = arguments {
                    Some(self.arguments_as_bindings(arguments, location)?)
                } else {
                    None
                };
                let def = self.ast.add_definition(
                    Definition {
                        mode,
                        name,
                        arguments,
                        implementation,
                    },
                    location,
                );
                if let Some(ty) = ty {
                    self.ast.add_annotation(def, ty);
                }
                return Ok(Some(def));
            }
            if let Some(arguments) = arguments {
                let inner = self.identifier(name, location);
                trace!("{indent}Add call", indent = self.indent());
                let args = self.handle_bindings(arguments)?.into();
                let call = self.ast.add_call(Call { inner, args }, location);
                if let Some(ty) = ty {
                    return Ok(Some(self.ast.add_annotation(call, ty)));
                }
                return Ok(Some(call));
            }
        }
        trace!("{indent}Add ident", indent = self.indent());
        let ident = self.identifier(name, location);
        if let Some(ty) = ty {
            return Ok(Some(self.ast.add_annotation(ident, ty)));
        }
        Ok(Some(ident))
    }

    fn binding_or_arg(&mut self) -> Result<BindingOrValue, TError> {
        let value = self.any_expr()?;
        let node = &self.ast.get(value);
        let location = node.location;
        if let NodeData::Definition(binding) = node.id {
            debug!("{indent}Definition: {binding:?}", indent = self.indent());
            return Ok(BindingOrValue::Binding(value));
        }
        if let NodeData::Identifier(ident) = node.id {
            let ty = node.ty;
            let (_node_id, ident) = self.ast.get_mut(ident);
            // TODO: Try for an assignment binding...
            return Ok(BindingOrValue::Identifier(*ident, ty, location));
        }
        debug!(
            "{indent}Arg value: {value:?} => {arg_str}",
            value = self.ast.get(value).id,
            arg_str = self.ast.pretty_node(value),
            indent = self.indent()
        );
        Ok(BindingOrValue::Value(value))
    }

    fn handle_bindings(
        &mut self,
        bindings: SmallVec<BindingOrValue, 2>,
    ) -> Result<Vec<NodeId>, TError> {
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

    fn file(&mut self) -> Result<Vec<NodeId>, TError> {
        let mut roots = vec![];
        while self.peek().is_ok() {
            roots.push(self.any_expr()?);
        }
        Ok(roots)
    }

    fn any_expr(&mut self) -> Result<NodeId, TError> {
        self.expr(Symbol::OpenParen)
    }

    fn token_in_context(&mut self) -> String {
        let location = self
            .tokens
            .peek()
            .map(|x| x.location())
            .unwrap_or(Location {
                start: self.contents.len() as u16,
                length: 0,
            });
        let start = location.start as usize;
        let end = (location.start as usize) + (location.length as usize);
        let bytes = &self.contents.as_bytes();
        let before = &bytes[start.saturating_sub(CTX_SIZE)..start];
        let head = &bytes[start..end];
        let after = &bytes[end..std::cmp::min(bytes.len(), end.saturating_add(CTX_SIZE))];
        format!(
            "{before}<{head}>{after}",
            before = std::str::from_utf8(before).unwrap_or("?"),
            head = std::str::from_utf8(head).unwrap_or("?"),
            after = std::str::from_utf8(after).unwrap_or("?"),
        )
    }

    fn expr(&mut self, binding: Symbol) -> Result<NodeId, TError> {
        debug!(
            "{indent}Inside {:?} => {}",
            binding,
            self.token_in_context(),
            indent = self.indent()
        );
        let res = {
            let _scope = self.depth.clone();
            self.expr_impl(binding)?
        };
        debug!(
            "{indent}Done subexpr ({:?})",
            binding,
            indent = self.indent()
        );
        Ok(res)
    }

    fn parse_left(&mut self, binding: Symbol) -> Result<NodeId, TError> {
        let Ok(mut token) = self.peek().copied() else {
            return Err(ParseError::UnexpectedEof.into());
        };
        token.kind = self.get_kind(&token);
        let location = token.location();
        trace!(
            "{indent}Expr: {token:?} (binding {binding:?})",
            indent = self.indent()
        );
        if self.operator_is(Symbol::OpenBracket).is_ok() {
            // Array, Tuple, List, Vector, Matrix, etc.
            let args = self.repeated(Symbol::CloseBracket, |this| this.any_expr())?;
            return Ok(self.ast.add_op(
                Op {
                    op: Symbol::OpenBracket,
                    args,
                },
                location,
            ));
        }
        if self.operator_is(Symbol::OpenCurly).is_ok() {
            // Block, set, dictionary, etc.
            let args = self.repeated(Symbol::CloseCurly, |this| this.any_expr())?;
            return Ok(self.ast.add_op(
                Op {
                    op: Symbol::OpenCurly,
                    args,
                },
                location,
            ));
        }
        if self.operator_is(Symbol::OpenParen).is_ok() {
            // Parenthesized expr... etc.
            let left = self.any_expr()?;
            self.require(TokenType::Op(Symbol::CloseParen))?;
            return Ok(left);
        }
        if let Some(expr) = self.call_or_definition()? {
            debug!(
                "{indent}Binding? {}",
                self.ast.pretty_node(expr),
                indent = self.indent(),
            );
            return Ok(expr);
        }
        if let TokenType::Op(prefix_op) = token.kind {
            let _ = self.token();
            let bind_type = prefix_op.binding_type();
            match bind_type {
                OpBinding::PrefixOp | OpBinding::PrefixOrInfixBinOp => {},
                OpBinding::Open(_) | OpBinding::Close(_) => {
                    return Err(TError::InternalError {
                        message: format!("{prefix_op:?} should have already been handled but was found in an expression in prefix position"),
                        location: Some(location),
                    })
                }
                OpBinding::InfixBinOp | OpBinding::InfixOrPostfixBinOp | OpBinding::PostfixOp => {
                    return Err(ParseError::MissingLeftHandSideOfOperator {
                        op: prefix_op,
                        bind_type,
                        location,
                    }
                    .into());
                }
            };
            // Handle a nested prefix ops
            let right = self.expr(binding)?;
            return Ok(self.ast.add_op(
                Op {
                    op: prefix_op,
                    args: smallvec![right],
                },
                location,
            ));
        }
        let Some(token) = self.token() else {
            return Err(ParseError::UnexpectedEof.into());
        };
        match token.kind {
            TokenType::Atom => self.atom(token, location),
            TokenType::NumberLit => self.number_literal(token, location),
            TokenType::StringLit => self.string_literal(token, location),
            TokenType::ColorLit => self.color_literal(token, location),
            _ => Err(ParseError::UnexpectedTokenTypeInExpression {
                got: token.kind,
                location,
            }
            .into()),
        }
    }

    fn expr_impl(&mut self, binding: Symbol) -> Result<NodeId, TError> {
        let mut left = self.parse_left(binding)?;
        trace!(
            "{indent}Check for right hand side: {left:?} (binding {binding:?})",
            indent = self.indent()
        );
        while let Ok(TokenType::Op(symbol)) = self.peek_kind() {
            let location = self.peek().expect("Internal error").location();
            if let OpBinding::Close(opener) = symbol.binding_type() {
                trace!(
                    "{indent}Closing {opener:?} Expr: {left:?} symbol: {symbol:?}",
                    indent = self.indent()
                );
                break;
            }
            if symbol != binding && symbol.is_looser(binding) && binding.is_looser(symbol) {
                // If both can be inside the other
                // and they're not associative...
                // then this is ambiguous and needs parens.
                return Err(ParseError::AmbiguousExpression {
                    left: binding,
                    right: symbol,
                    location,
                }
                .into());
            }
            if !binding.is_looser(symbol) {
                trace!(
                    "{indent}Back up Expr: {left:?} binding: {binding:?} inside symbol: {symbol:?}",
                    indent = self.indent()
                );
                break;
            }
            trace!("Continuing Expr: {left:?} sym: {symbol:?} inside binding: {binding:?}");
            let token = self.token().expect("Internal error");
            let location = token.location();
            if symbol == Symbol::OpenParen {
                // Require an 'apply' to balance it's parens.
                let mut args = smallvec![];
                while self.operator_is(Symbol::CloseParen).is_err() {
                    args.push(self.binding_or_arg()?);
                }
                let args = self.handle_bindings(args)?.into();
                left = self.ast.add_call(Call { inner: left, args }, location);
            } else {
                // TODO: Check that this is the right kind of operator.
                let bind_type = symbol.binding_type();
                left = match bind_type {
                    OpBinding::Open(closer) => {
                        let right = self.expr(symbol)?;
                        let res = self.ast.add_op(
                            Op {
                                op: symbol, // opener
                                args: smallvec![left, right],
                            },
                            location,
                        );
                        self.require(TokenType::Op(closer))?;
                        res
                    },
                    OpBinding::Close(_) | OpBinding::PrefixOp => {
                        return Err(TError::InternalError {
                            message: format!("{symbol:?} should have already been handled but was found in an expression in postfix or infix position"),
                            location: Some(location),
                        })
                    }
                    OpBinding::PostfixOp => self.ast.add_op(
                        Op {
                            op: symbol,
                            args: smallvec![left],
                        },
                        location,
                    ),
                    OpBinding::InfixBinOp
                    | OpBinding::PrefixOrInfixBinOp
                    | OpBinding::InfixOrPostfixBinOp => {
                        if is_assign(symbol) {
                            return Err(TError::InternalError {
                                message: format!("Assignment op {symbol:?} should have already been handled but was found in an expression in postfix or infix position"),
                                location: Some(location),
                            })
                        }
                        // TODO: Shouldn't consume tokens here if right fails for a post fix op.
                        let right = self.expr(symbol);
                        match right {
                            Ok(right) => self.ast.add_op(
                                Op {
                                    op: symbol,
                                    args: smallvec![left, right],
                                },
                                location,
                            ),
                            Err(_) if bind_type == OpBinding::InfixOrPostfixBinOp => {
                                return Ok(self.ast.add_op(
                                    Op {
                                        op: symbol,
                                        args: smallvec![left],
                                    },
                                    location,
                                ))
                            }
                            Err(TError::ParseError(ParseError::UnexpectedEof)) => {
                                return Err(TError::ParseError(
                                    ParseError::MissingRightHandSideOfOperator {
                                        op: symbol,
                                        bind_type,
                                        location,
                                    },
                                ))
                            }
                            Err(right) => return Err(right),
                        }
                    }
                }
            }
        }
        trace!("Expr done: {}", self.ast.pretty_node(left));
        trace!("(next token: {:?})", self.peek());
        Ok(left)
    }

    fn repeated<A, F: FnMut(&mut Self) -> Result<A, TError>>(
        &mut self,
        closer: Symbol,
        mut get_arg: F,
    ) -> Result<SmallVec<A, 2>, TError> {
        let mut args = smallvec![];
        while self.operator_is(closer).is_err() {
            while self.require(TokenType::Comma).is_ok() {
                // Eat extra commas
            }
            if self.operator_is(closer).is_ok() {
                debug!("{indent}Got a closer {closer:?}", indent = self.indent());
                break;
            }
            let arg = get_arg(self)?;
            args.push(arg);
        }
        debug!("{indent}Got closer {closer:?}", indent = self.indent());
        Ok(args)
    }

    fn name(&mut self, res: Token) -> Identifier {
        assert!(res.kind == TokenType::Ident);
        let name = res.get_src(self.contents);
        trace!("{indent}Name: {name}", indent = self.indent());
        self.ast
            .string_interner
            .register_str_by_loc(name, res.location().start)
    }

    fn identifier(&mut self, name: StrId, location: Location) -> NodeId {
        trace!(
            "{indent}Identifier: {name:?}",
            name = self.ast.string_interner.get_str(name),
            indent = self.indent()
        );
        self.ast.add_identifier(name, location)
    }

    fn atom(&mut self, res: Token, location: Location) -> Result<NodeId, TError> {
        assert!(res.kind == TokenType::Atom);
        let name = res.get_src(self.contents);
        trace!("{indent}Atom: {name}", indent = self.indent());
        let name = self.ast.string_interner.register_str(name);
        Ok(self.ast.add_atom(Atom { name }, location))
    }

    fn string_literal(&mut self, res: Token, location: Location) -> Result<NodeId, TError> {
        assert!(res.kind == TokenType::StringLit);
        trace!("{indent}Saving literal: {res:?}", indent = self.indent());
        let _id = self
            .ast
            .string_interner
            .register_str_by_loc(res.get_src(self.contents), location.start);
        Ok(self.ast.add_literal(Literal::Text, location))
    }

    fn color_literal(&mut self, res: Token, location: Location) -> Result<NodeId, TError> {
        assert!(res.kind == TokenType::ColorLit);
        trace!("{indent}Saving literal: {res:?}", indent = self.indent());
        let _id = self
            .ast
            .string_interner
            .register_str_by_loc(res.get_src(self.contents), location.start);
        Ok(self.ast.add_literal(Literal::Color, location))
    }

    fn number_literal(&mut self, res: Token, location: Location) -> Result<NodeId, TError> {
        assert!(res.kind == TokenType::NumberLit);
        trace!("{indent}Saving literal: {res:?}", indent = self.indent());
        let _id = self
            .ast
            .string_interner
            .register_str_by_loc(res.get_src(self.contents), location.start);
        Ok(self.ast.add_literal(Literal::Numeric, location))
    }
}

pub fn parse(filepath: &Path, contents: &str, tokens: &[Token]) -> Result<Ast, TError> {
    trace!("Parse {}: {:?}", filepath.display(), &tokens);
    let mut state = ParseState {
        contents,
        ast: Ast::new(filepath.to_path_buf()),
        tokens: tokens.iter().peekable(),
        depth: std::rc::Rc::new(()),
    };
    if !tokens.is_empty() {
        // Support empty files!
        let roots = state.file()?;
        if !roots.is_empty() {
            // Use the last root?
            state.ast.set_root(roots[roots.len() - 1]);
        }
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
}
