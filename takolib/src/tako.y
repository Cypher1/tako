%start Expr
%parse-param ctx: *mut crate::parser::Ast
%%
Expr -> Result<u64, ()>:
      Expr 'Add' Term {
        with(ctx).add_op(Op{ op: Symbol::Add, args: smallvec![] }, Location { start: 0, length: 0});
        Ok($1? + $3?)
      }
    | 'OpenBracket' ManyTerms 'CloseBracket' { Ok( $2?.len() as u64) }
    | Term { $1 }
    ;

Term -> Result<u64, ()>:
      Term 'Mul' Factor { Ok($1? * $3?) }
    | Factor { $1 }
    ;

Factor -> Result<u64, ()>:
      'OpenParen' Expr 'CloseParen' { $2 }
    | 'IntegerLit'
      {
          let v = $1.map_err(|_| ())?;
          parse_int($lexer.span_str(v.span()))
      }
    ;

ManyTerms -> Result<Vec<u64>, ()>:
      Term { Ok(vec![$1?]) }
    | ManyTerms Term { flatten($1, $2) }
    ;

%%

use smallvec::smallvec;
use crate::ast::*;
use crate::parser::{Symbol, Location};
// Any functions here are in scope for all the grammar actions above.

fn with(ctx: *mut crate::parser::Ast) -> &'static mut Ast {
  // TODO: Replace with this something safer... but... for now
  // the context is guaranteed to live long enough and not be moved.
  unsafe {&mut (*ctx)}
}

//AssignExpr -> ASTAssign: 'Assign' Expr
//{
 // let v = $2?;
  //ASTAssign { $1.span(), expr: Box.new(v) }
//};

    // | AssignExpr { Ok( 0 ) }
// struct ASTAssign {
    // id: lrpar::Span,
    // expr: Box<u32>,
// }

fn parse_int(s: &str) -> Result<u64, ()> {
    match s.parse::<u64>() {
        Ok(val) => Ok(val),
        Err(_) => {
            eprintln!("{} cannot be represented as a u64", s);
            Err(())
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
        Symbol::Exp,
        Symbol::Sequence,

    static ref LOOSER_THAN_MAP: HashMap<Symbol, Vec<Symbol>> = map!{
        Symbol::Sequence => vec![
            Symbol::Assign,
            Symbol::AddAssign,
            Symbol::SubAssign,
            Symbol::DivAssign,
            Symbol::MulAssign,
            Symbol::AndAssign,
            Symbol::OrAssign,
            Symbol::BitXorAssign,
            Symbol::LogicalAndAssign,
            Symbol::LogicalOrAssign,
            Symbol::ModuloAssign,
        ],
        Symbol::Assign => vec![Symbol::LeftPipe],
        Symbol::AddAssign => vec![Symbol::LeftPipe],
        Symbol::SubAssign => vec![Symbol::LeftPipe],
        Symbol::DivAssign => vec![Symbol::LeftPipe],
        Symbol::MulAssign => vec![Symbol::LeftPipe],
        Symbol::AndAssign => vec![Symbol::LeftPipe],
        Symbol::OrAssign => vec![Symbol::LeftPipe],
        Symbol::BitXorAssign => vec![Symbol::LeftPipe],
        Symbol::LogicalAndAssign => vec![Symbol::LeftPipe],
        Symbol::LogicalOrAssign => vec![Symbol::LeftPipe],
        Symbol::ModuloAssign => vec![Symbol::LeftPipe],
        Symbol::LeftPipe => vec![Symbol::RightPipe],
        Symbol::RightPipe => vec![Symbol::Sigma],
        Symbol::Sigma => vec![Symbol::Lambda],
        Symbol::Lambda => vec![Symbol::Arrow],
        Symbol::Arrow => vec![Symbol::DoubleArrow],
        Symbol::DoubleArrow => vec![Symbol::Forall],
        Symbol::Forall => vec![Symbol::Pi],
        Symbol::Pi => vec![Symbol::Exists],
        Symbol::Exists => vec![
            Symbol::HasType,
            Symbol::Eqs,
            Symbol::NotEqs,
            Symbol::Lt,
            Symbol::LtEqs,
            Symbol::Gt,
            Symbol::GtEqs,
        ],
        Symbol::HasType => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Eqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::NotEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Lt => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::LtEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Gt => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::GtEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::LeftShift => vec![Symbol::BitNot],
        Symbol::RightShift => vec![Symbol::BitNot],
        Symbol::Add => vec![Symbol::Sub],
        Symbol::Sub => vec![Symbol::Div],
        Symbol::Div => vec![Symbol::Mul],
        Symbol::Mul => vec![Symbol::Exp],
        Symbol::Exp => vec![Symbol::And, Symbol::LogicalAnd, Symbol::BitXor, Symbol::Modulo, Symbol::GetAddress],
        Symbol::And => vec![Symbol::Or],
        Symbol::Or => vec![Symbol::And],
        Symbol::LogicalAnd => vec![Symbol::LogicalOr],
        Symbol::LogicalOr => vec![Symbol::LogicalAnd],
        Symbol::BitXor => vec![Symbol::BitNot, Symbol::LogicalNot],
        Symbol::Try => vec![Symbol::Dot],
        Symbol::Dot => vec![Symbol::Range],
        Symbol::Range => vec![Symbol::Spread],
        Symbol::Spread => vec![Symbol::Escape],
    };
*/
