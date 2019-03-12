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

data Token = Token TokenType Info
  deriving (Show, Eq)

data TokenType
  = Ident String
  | DefinitionOperator
  | Comma
  | OpenParen | CloseParen
  | OpenBrace | CloseBrace
  | Plus | Minus
  deriving (Show, Eq)

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
lexer = do
  tokens' <- many lex'
  _ <- whiteSpace
  _ <- eof
  return tokens'

lex' :: ParsecT String u Identity Token
lex' = do
  toktype' <- choice $ map (try.lexeme) exprs
  info' <- getInfo
  return $ Token toktype' info'

data Info = Info
  { line :: Int
  , col :: Int
  } deriving (Show, Eq)

infoFrom :: SourcePos -> Info
infoFrom pos = Info
  { line = sourceLine pos
  , col = sourceColumn pos
  }

getInfo :: ParsecT String u Identity Info
getInfo = infoFrom <$> getPosition
