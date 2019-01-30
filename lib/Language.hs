{-# LANGUAGE FlexibleContexts #-}
module Language where

import Text.Parsec
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.ParserCombinators.Parsec.Token as Token

htriple :: Token.TokenParser st
htriple = Token.makeTokenParser htripleDef

htripleDef :: (Stream s m Char) => GenLanguageDef s u m
htripleDef = LanguageDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , nestedComments = True
  , identStart = letter
  , identLetter = alphaNum <|> oneOf "_'"
  , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames = []
  , reservedOpNames = []
  , caseSensitive = True
  }

identifier = Token.identifier htriple -- parses an identifier
reserved   = Token.reserved   htriple -- parses a reserved name
reservedOp = Token.reservedOp htriple -- parses an operator
parens     = Token.parens     htriple -- parses surrounding parenthesis:
                                      --   parens p
                                      -- takes care of the parenthesis and
                                      -- uses p to parse what's inside them
integer    = Token.integer    htriple -- parses an integer
semi       = Token.semi       htriple -- parses a semicolon
whiteSpace = Token.whiteSpace htriple -- parses whitespace

keywords = [ preConditionKeyword
           , postConditionKeyword
           , invarConditionKeyword
           ]

postConditionKeyword :: String
postConditionKeyword = "post"

invarConditionKeyword :: String
invarConditionKeyword = "invar"

preConditionKeyword :: String
preConditionKeyword = "pre"

assignmentOperator :: String
assignmentOperator = "="
