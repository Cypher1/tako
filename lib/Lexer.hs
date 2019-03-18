{-# LANGUAGE FlexibleInstances #-}
module Lexer where

import Data.Functor.Identity (Identity)

import Text.Parsec
import Text.ParserCombinators.Parsec
  ( SourcePos
  , sourceLine
  , sourceColumn
  )

import Language
import Util(Pretty(..))

data Token = Token TokenType Info
  deriving (Show, Eq)

instance Pretty Token where
  pretty (Token ty inf) = "'"++pretty ty ++ "' at " ++ pretty inf

data TokenType
  = Ident String
  | DefinitionOperator
  | Comma
  | OpenParen | CloseParen
  | OpenBrace | CloseBrace
  | Plus | Minus
  deriving (Show, Eq)

instance Pretty TokenType where
  pretty (Ident st) = show st
  pretty DefinitionOperator = assignmentOperator
  pretty Comma = ","
  pretty OpenParen = "("
  pretty CloseParen = ")"
  pretty OpenBrace = "{"
  pretty CloseBrace = "}"
  pretty Plus = plusOperator
  pretty Minus = minusOperator

exprs :: [ParsecT String u Identity TokenType]
exprs = [ Ident <$> identifier
        , const DefinitionOperator <$> assignmentOp
        , const Comma <$> consOperator
        , const OpenParen <$> openParen
        , const CloseParen <$> closeParen
        , const OpenBrace <$> openBrace
        , const CloseBrace <$> closeBrace
        , const Plus <$> plusOp
        , const Minus <$> minusOp
        ]

lexer :: ParsecT String u Identity [Token]
lexer = many lex' <* whiteSpace <* eof

makeToken :: SourcePos -> TokenType -> SourcePos -> Token
makeToken st ty end = Token ty $ infoFrom st end

lex' :: ParsecT String u Identity Token
lex' = makeToken <$> getInfo <*> choice ( map (try.lexeme) exprs) <*> getInfo

data Info = Info
  { at :: SourcePos
  , next_token :: SourcePos
  } deriving (Show, Eq)

instance Pretty Info where
  pretty inf = "Line: " ++ show (sourceLine (at inf)) ++ ", Column: " ++ show (sourceColumn (at inf))

infoFrom :: SourcePos -> SourcePos -> Info
infoFrom start end = Info
  { at = start
  , next_token = end
  }

getInfo :: ParsecT String u Identity SourcePos
getInfo = getPosition
