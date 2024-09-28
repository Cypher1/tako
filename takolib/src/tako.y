%start Expr
%parse-param ctx: *mut crate::parser::Ast
%%
IdentExpr -> R:
      'Ident'
{
  let str_for_hash = $lexer.span_str($1.as_ref().unwrap().span());
  let str_id = with(ctx)
      .string_interner
      .register_str_by_loc(str_for_hash, $span.start() as u16);
  let id = with(ctx).add_identifier(str_id, $span.into());
  Ok(id)
};

IntegerLitExpr -> R:
  'IntegerLit'
{
  let str_for_hash = $lexer.span_str($1.as_ref().unwrap().span());
  let _id = with(ctx)
      .string_interner
      .register_str_by_loc(str_for_hash, $span.start() as u16);
  Ok(with(ctx).add_literal(Literal::Numeric,
    $span.into()
  ))
};

Block -> R: 'OpenCurly' Sequence 'CloseCurly' { $1 };
SequenceArg -> R: Assign { $1 } | LeftPipe { $1 } | RightPipe { $1 };
AssignArg -> R: ExprWithBinding { $1 };

Expr -> R: HasType { $1 } | ComparisonExpr { $1 } | IfExpr { $1 } | Block { $1 };

IfExpr -> R:
  'if' Expr Block 'else' Block { todo!() }
| 'if' Expr Block { todo!() };

HasTypeArg -> R: UntypedExpr { $1 };
UntypedExpr -> R: MathsExpr { $1 } | Try { $1 };

ComparisonArg -> R: MathsExpr { $1 };

MathsExpr -> R: Add { $1 } | LeftShift { $1 } | RightShift { $1 } | Try { $1 };

LeftShiftArg -> R: BitNot { $1 };
RightShiftArg -> R: BitNot { $1 };

AddArg -> R: Mul { $1 };
MulArg -> R: Exp { $1 };
ExpArg -> R: Or { $1 } | LogicalOr { $1 } | BitXor { $1 } | Modulo { $1 } | GetAddress { $1 };
ModuloArg -> R: SimpleExpr { $1 };
OrArg -> R: And { $1 };
AndArg -> R: SimpleExpr { $1 };
GetAddressArg -> R: SimpleExpr { $1 };
LogicalOrArg -> R: LogicalAnd { $1 };
LogicalAndArg -> R: BitNot { $1 } | LogicalNot { $1 };
BitXorArg -> R: BitNot { $1 } | LogicalNot { $1 };
BitNotArg -> R: SimpleExpr { $1 };
LogicalNotArg -> R: SimpleExpr { $1 };
TryArg -> R: Dot { $1 };
DotArg -> R: Range { $1 };
RangeArg -> R: Spread { $1 };
SpreadArg -> R: SimpleExpr { $1 };

SimpleExpr -> R:
  'OpenParen' Expr 'CloseParen' { $2 }
  | ListLikeExpr { $1 }
  | IntegerLitExpr { $1 }
  | IdentExpr { $1 };

ListLikeExpr -> Rs: 'OpenBracket' MultipleExprs 'CloseBracket' { $2 };

Binding -> R: 'Sigma' { $1 } | 'Lambda' { $1 } | 'Pi' { $1 } | 'Forall' { $1 } | 'Exists' { $1 };

ExprWithBinding -> R:
  Binding Ident Args 'Assign' Expr {
  todo!("not handled")
}
| Binding Ident Args 'DoubleArrow' Expr {
  todo!("not handled")
}
| Ident Args 'Arrow' Expr {
  todo!("not handled")
}
| Ident Args 'Assign' Expr {
  todo!("not handled")
};

HasType -> R:
  HasTypeArg 'HasType' HasTypeArg
  {
    let node = with(ctx).add_annotation($1?, $2?);
    Ok(node)
  }
| HasTypeArg { $1 };

Sequence -> R:
  SequenceArg 'Sequence' Sequence
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::Sequence,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| SequenceArg { $1 };

MultipleExprs -> Rs:
  Expr { Ok(vec![$1?]) }
| MultipleExprs Expr { flatten($1, $2) };

Args -> Rs: 'OpenParen' MultipleArgs 'CloseParen' { $2 };

MultipleArgs -> Rs:
  ExprWithBinding { Ok(vec![$1?]) }
| MultipleArgs ExprWithBinding { flatten($1, $2) };

AssignTarget -> R:
IdentExpr Args {
  // TODO: Add args...
  todo!()
  $1
}
| IdentExpr {
  todo!()
  $1
};

Assign -> R:
  AssignTarget 'Assign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::Assign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'AddAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::AddAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'SubAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::SubAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'DivAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::DivAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'MulAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::MulAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'AndAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::AndAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'OrAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::OrAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'BitXorAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::BitXorAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'LogicalAndAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::LogicalAndAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'LogicalOrAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::LogicalOrAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignTarget 'ModuloAssign' AssignArg
  {
    let op = with(ctx).add_op(Op{
        op: Symbol::ModuloAssign,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    // TODO: Flatten
    Ok(op)
  }
| AssignArg { $1 };

LeftPipe -> R:
  AssignTarget 'LeftPipe' AssignArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::LeftPipe,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| AssignArg { $1 };

RightPipe -> R:
  AssignTarget 'RightPipe' AssignArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::LeftPipe,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| AssignArg { $1 };


ComparisonExpr -> R:
  ComparisonArg 'Eqs' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::Eqs,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg 'NotEqs' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::NotEqs,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg 'Lt' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::Lt,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg 'LtEqs' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::LtEqs,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg 'Gt' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::Gt,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg 'GtEqs' ComparisonArg {
    let mul = with(ctx).add_op(Op{
        op: Symbol::GtEqs,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(mul)
}
| ComparisonArg { $1 };

Mul -> R:
  Mul 'Mul' MulArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Mul,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| Mul 'Div' MulArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Div,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| MulArg { $1 };

Exp -> R:
  ExpArg 'Exp' Exp {
    let op = with(ctx).add_op(Op{
        op: Symbol::Exp,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| ExpArg { $1 };

Add -> R:
  Add 'Add' AddArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Add,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| Add 'Sub' AddArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Sub,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| AddArg { $1 };

Modulo -> R:
  ModuloArg 'Modulo' Modulo {
    let op = with(ctx).add_op(Op{
        op: Symbol::Modulo,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| ModuloArg { $1 };

GetAddress -> R:
  GetAddressArg 'GetAddress' GetAddress {
    let op = with(ctx).add_op(Op{
        op: Symbol::GetAddress,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| GetAddressArg { $1 };

LeftShift -> R:
  LeftShift 'LeftShift' LeftShiftArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::LeftShift,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| LeftShiftArg { $1 };

RightShift -> R:
  RightShift 'RightShift' RightShiftArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::RightShift,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| RightShiftArg { $1 };

Dot -> R:
  Dot 'Dot' DotArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Dot,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| DotArg { $1 };

Range -> R:
  RangeArg 'Range' RangeArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Range,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| RangeArg { $1 };

Or -> R:
  Or 'Or' OrArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Or,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| OrArg { $1 };

LogicalOr -> R:
  LogicalOr 'LogicalOr' LogicalOrArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::LogicalOr,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| LogicalOrArg { $1 };

And -> R:
  And 'And' AndArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::And,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| AndArg { $1 };

LogicalAnd -> R:
  LogicalAnd 'LogicalAnd' LogicalAndArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::LogicalAnd,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| LogicalAndArg { $1 };

BitXor -> R:
  BitXor 'BitXor' BitXorArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::BitXor,
        args: smallvec![$1?, $3?]
      },
      $span.into()
    );
    Ok(op)
}
| BitXorArg { $1 };

BitNot -> R:
  'BitNot' BitNotArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::BitNot,
        args: smallvec![$2?]
      },
      $span.into()
    );
    Ok(op)
}
| BitNotArg { $1 };

LogicalNot -> R:
  'LogicalNot' LogicalNotArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::LogicalNot,
        args: smallvec![$2?]
      },
      $span.into()
    );
    Ok(op)
}
| LogicalNotArg { $1 };

Spread -> R:
  'Spread' SpreadArg {
    let op = with(ctx).add_op(Op{
        op: Symbol::Spread,
        args: smallvec![$2?]
      },
      $span.into()
    );
    Ok(op)
}
| SpreadArg { $1 };

Try -> R:
  TryArg 'Try' {
    let op = with(ctx).add_op(Op{
        op: Symbol::Try,
        args: smallvec![$1?]
      },
      $span.into()
    );
    Ok(op)
}
| TryArg { $1 };

%%
/*
Array 'OpenBracket' ManyTerms 'CloseBracket'
{
  Ok( $2?.len() as NodeId)
}

ManyTerms -> R:
      Term { Ok(vec![$1?]) }
    | ManyTerms Term { flatten($1, $2) }
    ;

%%
*/

type R = Result<NodeId, ParseError>;
type Rs = Result<Vec<NodeId>, ParseError>;

use smallvec::smallvec;
use crate::ast::*;
use crate::parser::{ParseError, Symbol, Location, semantics::Literal};
// Any functions here are in scope for all the grammar actions above.

fn with(ctx: *mut crate::parser::Ast) -> &'static mut Ast {
  // TODO: Replace with this something safer... but... for now
  // the context is guaranteed to live long enough and not be moved.
  unsafe {&mut (*ctx)}
}

impl Into<Location> for cfgrammar::Span {
  fn into(self) -> Location {
    Location {
      start: self.start() as u16,
      length: self.len() as u8,
    }
  }
}

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>)
           -> Result<Vec<T>, ()>
{
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}


/*
    RIGHT_ASSOCIATIVE
        Exp,
        Sequence,

*/
