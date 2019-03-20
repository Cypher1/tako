{-# LANGUAGE InstanceSigs #-}
module HtripleParser where

import Data.Functor.Identity (Identity)
import Control.Applicative (Alternative(..))
import Text.Parsec (ParsecT, parse, eof, token, (<?>), SourcePos)
import Lexer (lexer, at, Token(..), TokenType(..))

import Util (Pretty(pretty), prettyList)

type Id = String

data Arg
  = Kw Def
  | A Step
  deriving Show

instance Pretty Arg where
  pretty (Kw def') = pretty def'
  pretty (A expr') = pretty expr'

data Call = Call Id [Arg]
  deriving Show

instance Pretty Call where
  pretty (Call name' []) = name'
  pretty (Call name' args) = name'++"("++prettyList args++")"

data Expr
  = CallExpr Call
  | Dict Scope
  deriving Show

instance Pretty Expr where
  pretty (CallExpr call') = pretty call'
  pretty (Dict scope') = pretty scope'

data Step
  = Step { pre :: [Expr], op :: Expr, post :: [Expr] }
  deriving Show

instance Pretty Step where
  pretty st = pre'++pretty (op st)++post'
    where
      pre'
        | null $ pre st = ""
        | otherwise = "-{"++prettyList (pre st)++"}"
      post'
        | null $ post st = ""
        | otherwise = "+{"++prettyList (post st)++"}"

data Def = Def Id [Arg] Step
  deriving Show

instance Pretty Def where
  pretty (Def name args to) = pretty (Call name args) ++"="++pretty to

newtype Scope = Scope [Def]
  deriving Show

instance Pretty Scope where
  pretty (Scope defs') = "{"++prettyList defs'++"}"

type Parser = ParsecT [Token] () Identity

posFromTok :: Token -> SourcePos
posFromTok (Token _t inf) = at inf

tok :: TokenType -> Parser TokenType
tok exp'
   = token pretty posFromTok testTok
   where
     testTok (Token t _inf)
       | exp' == t = Just t
       | otherwise = Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- Actual parsers

ident :: Parser Id
ident
  = token pretty posFromTok testTok
   where
     testTok (Token t _)
       = case t of
           Ident name' -> Just name'
           _ -> Nothing

def :: Parser Def
def = Def <$> ident <*> argList <* tok DefinitionOperator <*> step

argList :: Parser [Arg]
argList
  = (tok OpenParen *> (arg`sepBy`tok Comma <* tok CloseParen) <?> fl') <|> pure []
  where
    fl' = "a list of arguments"

call :: Parser Call
call = Call <$> ident <*> argList

arg :: Parser Arg
arg = (Kw <$> def) <|> (A <$> step)

expr :: Parser Expr
expr = (Dict <$> scope) <|> (CallExpr <$> call)

step :: Parser Step
step = Step <$> preds' Minus <*> expr <*> preds' Plus
  where
    preds' :: TokenType -> Parser [Expr]
    preds' t
      = (tok t *> (tok OpenBrace *> predTail') <?> fl') <|> pure []
    fl' = "a '{' (at the start of a set of assertions)"
    predTail' = (expr`sepBy`tok Comma <* tok CloseBrace) <?> "a list of predicates"

defs :: Parser [Def]
defs = def`sepBy`(tok Comma <|> pure Comma)

scope :: Parser Scope
scope = Scope <$> (tok OpenBrace *> ((defs <* tok CloseBrace) <?> fl'))
  where
    fl' = "a set of declarations followed by a '}'"

parseFile :: String -> IO Scope
parseFile file = do
  contents' <- readFile file
  case parse lexer file contents' of
    Right toks' -> return $ tokens file toks'
    Left err' -> error $ show err'

tokens :: String -> [Token] -> Scope
tokens file toks'
  = case parse (defs <* eof) file toks' of
      Right mod' -> Scope mod'
      Left err -> error $ show err
