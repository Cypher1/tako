pub mod error;
pub mod semantics;
#[cfg(test)]
mod tests;
pub mod tokens;
use crate::ast::location::Location;
use crate::ast::string_interner::{Identifier, StrId};
use crate::ast::{Ast, Atom, Call, Contains, Definition, NodeData, NodeId, Op};
use crate::error::TError;
use better_std::include_strs;
pub use error::ParseError;
use log::{debug, trace};
use semantics::BindingMode;
use semantics::Literal;
use smallvec::{smallvec, SmallVec};
use std::path::Path;
use tokens::{
    binding_mode_from_op, is_assign, op_from_assign_op, OpBinding, Symbol, Token, TokenType,
};

const CTX_SIZE: usize = 20;
pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

#[derive(Debug)]
enum BindingOrValue {
    Identifier(BindingMode, Identifier, NodeId, Option<NodeId>, Location),
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
                BindingOrValue::Identifier(mode, name, _ident, _ty, _location) => {
                    let def = self.ast.add_definition(
                        Definition {
                            mode,
                            name: name.clone(),
                            arguments: None,
                            implementation: None,
                        },
                        location,
                    );
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
                let args = self.bindings_as_values(arguments)?.into();
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
            let (node_id, str_id) = self.ast.get_mut(ident);
            // TODO: Try for an assignment binding...
            let mode = BindingMode::Given;
            return Ok(BindingOrValue::Identifier(mode, *str_id, *node_id, ty, location));
        }
        debug!(
            "{indent}Arg value: {value:?} => {arg_str}",
            value = self.ast.get(value).id,
            arg_str = self.ast.pretty_node(value),
            indent = self.indent()
        );
        Ok(BindingOrValue::Value(value))
    }

    fn bindings_as_values(
        &mut self,
        bindings: SmallVec<BindingOrValue, 2>,
    ) -> Result<Vec<NodeId>, TError> {
        let mut args = vec![];
        for binding in bindings {
            let binding = match binding {
                BindingOrValue::Binding(binding) => binding,
                BindingOrValue::Identifier(mode, _str_id, ident, _ty, location) => {
                    if mode != BindingMode::Given {
                        // TODO: Support more binding modes
                        return Err(TError::InternalError {
                            message: format!("{mode:?} for {ident:?} is not supported as a value"),
                            location: Some(location),
                        });
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

    fn expr_impl(&mut self, binding: Symbol) -> Result<NodeId, TError> {
        let mut left = self.parse_left(binding)?;
        trace!(
            "{indent}Check for right hand side: {left:?} (binding {binding:?})",
            indent = self.indent()
        );
        while let Ok(TokenType::Op(op)) = self.peek_kind() {
            let location = self.peek().expect("Internal error").location();
            if let OpBinding::Close(opener) = op.binding_type() {
                trace!(
                    "{indent}Closing {opener:?} Expr: {left:?} symbol: {op:?}",
                    indent = self.indent()
                );
                break;
            }
            if op != binding && op.is_looser(binding) && binding.is_looser(op) {
                // If both can be inside the other
                // and they're not associative...
                // then this is ambiguous and needs parens.
                return Err(ParseError::AmbiguousExpression {
                    left: binding,
                    right: op,
                    location,
                }
                .into());
            }
            if !binding.is_looser(op) {
                trace!(
                    "{indent}Back up Expr: {left:?} binding: {binding:?} inside symbol: {op:?}",
                    indent = self.indent()
                );
                break;
            }
            trace!("Continuing Expr: {left:?} sym: {op:?} inside binding: {binding:?}");
            let token = self.token().expect("Internal error");
            let location = token.location();
            if op == Symbol::OpenParen {
                // Require an 'apply' to balance it's parens.
                let mut args = smallvec![];
                while self.operator_is(Symbol::CloseParen).is_err() {
                    args.push(self.binding_or_arg()?);
                }
                let args = self.bindings_as_values(args)?.into();
                left = self.ast.add_call(Call { inner: left, args }, location);
            } else {
                // TODO: Check that this is the right kind of operator.
                let bind_type = op.binding_type();
                left = match bind_type {
                    OpBinding::Open(closer) => {
                        let right = self.expr(op)?;
                        let res = self.ast.add_op(
                            Op {
                                op, // opener
                                args: smallvec![left, right],
                            },
                            location,
                        );
                        self.require(TokenType::Op(closer))?;
                        res
                    },
                    OpBinding::Close(_) | OpBinding::PrefixOp => {
                        return Err(TError::InternalError {
                            message: format!("{op:?} should have already been handled but was found in an expression in postfix or infix position"),
                            location: Some(location),
                        })
                    }
                    OpBinding::PostfixOp => self.ast.add_op(
                        Op {
                            op,
                            args: smallvec![left],
                        },
                        location,
                    ),
                    OpBinding::InfixBinOp
                    | OpBinding::PrefixOrInfixBinOp
                    | OpBinding::InfixOrPostfixBinOp => {
                        if is_assign(op) {
                            return Err(TError::InternalError {
                                message: format!("Assignment op {op:?} should have already been handled but was found in an expression in postfix or infix position"),
                                location: Some(location),
                            })
                        }
                        // TODO: Shouldn't consume tokens here if right fails for a post fix op.
                        let right = self.expr(op);
                        match right {
                            Ok(right) => self.ast.add_op(
                                Op {
                                    op,
                                    args: smallvec![left, right],
                                },
                                location,
                            ),
                            Err(_) if bind_type == OpBinding::InfixOrPostfixBinOp => {
                                return Ok(self.ast.add_op(
                                    Op {
                                        op,
                                        args: smallvec![left],
                                    },
                                    location,
                                ))
                            }
                            Err(TError::ParseError(ParseError::UnexpectedEof)) => {
                                return Err(TError::ParseError(
                                    ParseError::MissingRightHandSideOfOperator {
                                        op,
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
