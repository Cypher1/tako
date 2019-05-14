{-# LANGUAGE FlexibleInstances #-}
module Lexer where

import           Data.Functor.Identity          ( Identity )

import           Text.Parsec
import           Text.ParserCombinators.Parsec  ( SourcePos
                                                , sourceLine
                                                , sourceColumn
                                                )

import           Language
import           Util                           ( Pretty(..) )

data Token = Token TokenType Info
  deriving (Show, Eq)

instance Pretty Token where
  pretty (Token ty inf) = pretty ty ++ " at " ++ pretty inf

data TokenType
  = Ident String
  | DefOp
  | Comma
  | OpenParen | CloseParen
  | OpenBrace | CloseBrace
  | Plus | Minus
  | RequireOp | ProvideOp
  deriving (Show, Eq)

instance Pretty TokenType where
  pretty (Ident st) = st
  pretty DefOp = defOperator
  pretty RequireOp = "-|"
  pretty ProvideOp = "|-"
  pretty Comma = ","
  pretty OpenParen = "("
  pretty CloseParen = ")"
  pretty OpenBrace = "{"
  pretty CloseBrace = "}"
  pretty Plus = plusOperator
  pretty Minus = minusOperator

lexes :: [ParsecT String u Identity TokenType]
lexes =
  [ Ident <$> identifier
  , const DefOp <$> defOp
  , const RequireOp <$> requireOp
  , const ProvideOp <$> provideOp
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
lex' = makeToken <$> getInfo <*> choice (map (try . lexeme) lexes) <*> getInfo

data Info = Info
  { at :: SourcePos
  , next_token :: SourcePos
  } deriving (Eq)

instance Show Info where
  show _ = ""

instance Pretty Info where
  pretty inf = "Line: " ++ show (sourceLine (at inf)) ++ ", Column: " ++ show (sourceColumn (at inf))

infoFrom :: SourcePos -> SourcePos -> Info
infoFrom start end = Info {at = start, next_token = end}

getInfo :: ParsecT String u Identity SourcePos
getInfo = getPosition
