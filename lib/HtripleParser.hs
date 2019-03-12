{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module HtripleParser where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.State.Strict
-- import Data.Char (isSpace, isDigit, ord)

import Text.ParserCombinators.Parsec (parse)
-- import Control.Monad.State
import Lexer (lexer, Token(..), TokenType(..))

import Util (Pretty(pretty), prettyList)


-- info is used for passing context (e.g. source location, the stack)
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

newtype Parser a = Parser { unParser :: StateT [Token] (Either String) a }

runParser :: Parser a -> [Token] -> Either String (a, [Token])
runParser = runStateT . unParser

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ f <$> unParser p

instance Applicative Parser where
    pure :: a -> Parser a
    pure a  = Parser $ pure a
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    f <*> a = Parser $ unParser f <*> unParser a

instance Alternative Parser where
    empty :: Parser a
    empty   = Parser empty
    (<|>) :: Parser a -> Parser a -> Parser a
    a <|> b = Parser $ unParser a <|> unParser b

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= f = Parser $ StateT $ \s -> do
        (a', s') <- runParser a s
        runParser (f a') s'

anyToken :: Parser Token
anyToken = Parser . StateT $ \case
    []     -> empty
    (c:cs) -> pure (c, cs)

tok :: TokenType -> Parser TokenType
tok exp' = do
  Token got' _inf' <- anyToken
  if got' == exp'
     then pure exp'
     else empty -- fail $ "Expected an '"++show exp'++"', got '"++show got'++"' at "++show inf'

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op' a = (p `chainl1` op') <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op' = p >>= rest
    where 
        rest a = (do
            f <- op'
            b <- p
            rest (f a b)) <|> pure a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op' a = (p `chainr1` op') <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op' = scan
    where
        scan   = p >>= rest
        rest a = (do
            f <- op'
            b <- scan
            rest (f a b)) <|> pure a

-- Actual parsers

parseId :: Parser Id
parseId = do
  Token tok' _inf' <- anyToken
  case tok' of
    Ident name' -> pure name'
    _tok'' -> empty -- fail $ "Expected an Identifier, got '"++show tok''++"' at "++show inf'

def :: Parser Def
def
  = Def <$> parseId <*> argList <* tok DefinitionOperator <*> step

argList :: Parser [Arg]
argList
  = tok OpenParen *> arg`sepBy`tok Comma <* tok CloseParen
  <|> pure []

call :: Parser Call
call = Call <$> parseId <*> argList

arg :: Parser Arg
arg = (Kw <$> def) <|> (A <$> step)

expr :: Parser Expr
expr = (Dict <$> scope) <|> (CallExpr <$> call)

step :: Parser Step
step = Step <$> preds' Minus <*> expr <*> preds' Plus
  where
    preds' :: TokenType -> Parser [Expr]
    preds' t
      = tok t *> tok OpenBrace *> expr`sepBy`tok Comma <* tok CloseBrace
      <|> pure []

defs :: Parser [Def]
defs = def`sepBy`(tok Comma <|> pure Comma)

scope :: Parser Scope
scope = Scope <$> (tok OpenBrace *> defs <* tok CloseBrace)

parseFile :: String -> IO Scope
parseFile file = do
  contents' <- readFile file
  case parse lexer "" contents' of
    Right toks' -> tokens toks'
    Left err' -> error $ show err'

tokens :: [Token] -> IO Scope
tokens toks'
  = case runParser defs toks' of
      Right (mod', []) -> pure $ Scope mod'
      Right (_, extra) -> fail $ "Expected EOF, found "++show extra
      Left msg -> fail msg
