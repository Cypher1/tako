{-# LANGUAGE FlexibleInstances #-}
module Lexer where

import Data.Functor.Identity (Identity)

import Text.Parsec

import Language

data Token
  = Ident String
  | DefinitionOperator
  | Comma
  | OpenParen | CloseParen
  | OpenBrace | CloseBrace
  deriving (Show, Eq)

exprs :: [ParsecT String u Identity Token]
exprs = [ Ident <$> identifier
        , const DefinitionOperator <$> assignmentOp
        , const Comma <$> consOperator
        , const OpenParen <$> openParen
        , const CloseParen <$> closeParen
        , const OpenBrace <$> openBrace
        , const CloseBrace <$> closeBrace
        ]

lexer :: ParsecT String u Identity [Token]
lexer = do
  tokens' <- many $ choice $ map (try.lexeme) exprs
  _ <- whiteSpace
  _ <- eof
  return tokens'
