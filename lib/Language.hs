{-# LANGUAGE FlexibleContexts #-}
module Language where

import           Control.Monad
import           Data.Functor.Identity          ( Identity )

import           Text.Parsec
import           Text.Parsec.Token              ( GenLanguageDef(..) )
import           Text.ParserCombinators.Parsec  ( SourcePos
                                                , sourceLine
                                                , sourceColumn
                                                )
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

import           Util                           ( Pretty(..) )


takoLang :: Token.TokenParser st
takoLang = Token.makeTokenParser takoLangDef

postConditionKeyword :: String
postConditionKeyword = "give"

unsafeIntroductionKeyword :: String
unsafeIntroductionKeyword = "assume"

preConditionKeyword :: String
preConditionKeyword = "take"

takoLangDef :: (Stream s m Char) => GenLanguageDef s u m
takoLangDef = LanguageDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames   = keywords
  , reservedOpNames = pretty <$> [DefOp, RequireOp, ProvideOp]
  , caseSensitive   = True
  }

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
  pretty DefOp = "="
  pretty RequireOp = "-|"
  pretty ProvideOp = "|-"
  pretty Comma = ","
  pretty OpenParen = "("
  pretty CloseParen = ")"
  pretty OpenBrace = "{"
  pretty CloseBrace = "}"
  pretty Plus = "+"
  pretty Minus = "-"

lexes :: [ParsecT String u Identity TokenType]
lexes =
  [Ident <$> identifier]
    ++ (   tok2Parser
       <$> [ DefOp
           , RequireOp
           , ProvideOp
           , Comma
           , OpenParen
           , CloseParen
           , OpenBrace
           , CloseBrace
           , Plus
           , Minus
           ]
       )

data Token = Token TokenType Info
  deriving (Show, Eq)

instance Pretty Token where
  pretty (Token ty inf) = pretty ty ++ " at " ++ pretty inf

identifier :: ParsecT String u Identity String
identifier = Token.identifier takoLang -- parses an identifier
reserved :: String -> ParsecT String u Identity ()
reserved = Token.reserved takoLang -- parses a reserved name
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp takoLang -- parses an operator
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens takoLang -- parses surrounding parenthesis:
                                      --   parens p
                                      -- takes care of the parenthesis and
                                      -- uses p to parse what's inside them
integer :: ParsecT String u Identity Integer
integer = Token.integer takoLang -- parses an integer
semi :: ParsecT String u Identity String
semi = Token.semi takoLang -- parses a semicolon
whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace takoLang -- parses whitespace

keywords :: [String]
keywords =
  [preConditionKeyword, postConditionKeyword, unsafeIntroductionKeyword]

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = Token.lexeme takoLang

ptok :: TokenType -> ParsecT String u Identity ()
ptok tok' = void $ Token.symbol takoLang $ pretty tok'

consOperator :: ParsecT String u Identity ()
consOperator = void $ Token.comma takoLang

lexer :: ParsecT String u Identity [Token]
lexer = many lex' <* whiteSpace <* eof

tok2Parser :: TokenType -> ParsecT String u Identity TokenType
tok2Parser tok' = tok' <$ (void $ Token.symbol takoLang $ pretty tok')

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
