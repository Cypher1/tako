{-# LANGUAGE FlexibleContexts #-}
module Language where

import           Control.Monad                  ( void )
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
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe )

takoLang :: Token.TokenParser st
takoLang = Token.makeTokenParser takoLangDef

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
  , reservedNames   = []
  , reservedOpNames = pretty <$> [DefOp, RequireOp, ProvideOp]
  , caseSensitive   = True
  }

data TokType
  -- Dual character tokens
  = RequireOp
  | ProvideOp
  -- Single character tokens
  | DefOp
  | Comma
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Plus
  | Minus
  | Semi
  | Dot
  deriving (Show, Eq, Enum, Bounded)

instance Pretty TokType where
  pretty RequireOp = "-|"
  pretty ProvideOp = "|-"
  pretty DefOp = "="
  pretty Comma = ","
  pretty OpenParen = "("
  pretty CloseParen = ")"
  pretty OpenBrace = "{"
  pretty CloseBrace = "}"
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Semi = ";"
  pretty Dot = "."

data PrimTriOpType
  = PrimAnd
  | PrimOr
-- TODO(cypher1): Replace with signed and unsigned versions
-- TODO(cypher1): Write extract, construct, apply function over prims
-- rather than numeric prims.
  | PrimAdd
  | PrimSub
  | PrimDiv
  | PrimMul
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
instance Pretty PrimTriOpType where
  pretty PrimAnd = "And"
  pretty PrimOr = "Or"
  pretty PrimAdd = "Add"
  pretty PrimSub = "Sub"
  pretty PrimDiv = "Div"
  pretty PrimMul = "Mul"

data PrimBiOpType
  = PrimNot
  | PrimNew
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
instance Pretty PrimBiOpType where
  pretty PrimNot = "Not"
  pretty PrimNew = "New"

data PrimValOpType
  = PrimLoad
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Pretty PrimValOpType where
  pretty PrimLoad = "Load"

data PrimUnOpType
  = PrimFree
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
instance Pretty PrimUnOpType where
  pretty PrimFree = "Free"

data PrimOpType
  = PrimVal PrimValOpType
  | PrimUn PrimUnOpType
  | PrimBi PrimBiOpType
  | PrimTri PrimTriOpType
  deriving (Show, Eq)

allPrimOps :: [PrimOpType]
allPrimOps =
  (PrimVal <$> [(minBound :: PrimValOpType) .. maxBound])
    ++ (PrimUn <$> [(minBound :: PrimUnOpType) .. maxBound])
    ++ (PrimBi <$> [(minBound :: PrimBiOpType) .. maxBound])
    ++ (PrimTri <$> [(minBound :: PrimTriOpType) .. maxBound])

instance Enum PrimOpType where
  fromEnum val
    = error ("Unexpected PrimOp "++show val++" in Enum conversion.")`fromMaybe`elemIndex val allPrimOps
  toEnum n = allPrimOps !! n

instance Bounded PrimOpType where
  minBound = head allPrimOps
  maxBound = last allPrimOps

instance Pretty PrimOpType where
  pretty (PrimVal ty) = pretty ty
  pretty (PrimUn ty) = pretty ty
  pretty (PrimBi ty) = pretty ty
  pretty (PrimTri ty) = pretty ty

data TokenType
  = Tok TokType
  | Op PrimOpType
  | LitInt Integer
  | Ident String
  deriving (Show, Eq)

instance Pretty TokenType where
  pretty (Tok ty) = pretty ty
  pretty (Op op) = pretty op
  pretty (LitInt n) = show n
  pretty (Ident st) = st

lexes :: [ParsecT String u Identity TokenType]
lexes = options2Parser <$> options'
 where
  options2Parser = try . Token.lexeme takoLang
  options' =
    (tok2Parser <$> toks')
      ++ (tok2Parser <$> prims')
      ++ [LitInt <$> integer, Ident <$> identifier]
  toks'  = Tok <$> [minBound .. maxBound]
  prims' = Op <$> [minBound .. maxBound]

data Token = Token TokenType Info
  deriving (Show, Eq)

instance Pretty Token where
  pretty (Token ty inf') = pretty ty ++ " at " ++ pretty inf'

reserved :: String -> ParsecT String u Identity ()
reserved = Token.reserved takoLang -- parses a reserved name
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp takoLang -- parses an operator
integer :: ParsecT String u Identity Integer
integer = Token.integer takoLang -- parses an integer
identifier :: ParsecT String u Identity String
identifier = Token.identifier takoLang -- parses an identifier

ptok :: TokenType -> ParsecT String u Identity ()
ptok tok' = void $ Token.symbol takoLang $ pretty tok'

lexer :: ParsecT String u Identity [Token]
lexer = many lex' <* Token.whiteSpace takoLang <* eof

tok2Parser :: Pretty a => a -> ParsecT String u Identity a
tok2Parser tok' = tok' <$ void (Token.symbol takoLang $ pretty tok')

lex' :: ParsecT String u Identity Token
lex' =
  (\e t' s -> Token t' $ infoFrom e s) <$> getInfo <*> choice lexes <*> getInfo

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
