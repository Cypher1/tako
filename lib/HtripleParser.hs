module HtripleParser where

import Text.ParserCombinators.Parsec (parse)
-- import Control.Monad.State
import Lexer (lexer, Token(..), TokenType(..))

import Util ((|-), (-|), Pretty(pretty))

instance Pretty a => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x++", "++pretty xs

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

-- info is used for passing context (e.g. source location, the stack)
type Id = String

data Arg
  = Kw Def
  | A Expr
  deriving Show

instance Pretty Arg where
  pretty (Kw def) = pretty def
  pretty (A expr) = pretty expr

data Expr
  = Call Id [Arg]
  | Dict Scope
  deriving Show

instance Pretty Expr where
  pretty (Call name' []) = name'
  pretty (Call name' args) = name'++"("++pretty args++")"
  pretty (Dict scope) = pretty scope

data Def = Def Id [Arg] Expr
  deriving Show

instance Pretty Def where
  pretty (Def name args to) = pretty (Call name args) ++"="++pretty to

data Scope = Scope [Def]
  deriving Show

instance Pretty Scope where
  pretty (Scope defs) = "{"++pretty defs++"}"

type TokenParser tree =  [Token] -> Either (String, [Token]) (tree, [Token])

idT :: TokenParser ()
idT toks = return ((), toks)

tok :: TokenType -> TokenParser ()
tok expected (Token t _:toks) | expected == t = return ((), toks)
tok expected toks = Left ("Expected "++show expected++" in ", toks)

with :: TokenParser a -> TokenParser b -> (a->b->c) -> TokenParser c
with a b f toks = do
  (a', toks') <- a toks
  (b', toks'') <- b toks'
  return (f a' b', toks'')

option :: TokenParser a -> TokenParser b -> (Either a b -> c) -> TokenParser c
option pa pb f toks
  = case pa toks of
      Left _ -> case pb toks of
                  Left err -> Left err
                  Right (b, toks') -> return (f -| Right b, toks')
      Right (a, toks') -> return (f $ Left a, toks')

many1 :: TokenParser v -> TokenParser [v]
many1 p = (:) |- p`with`many p

many :: TokenParser v -> TokenParser [v]
many p = id`either`const [] |- many1 p`option`idT

manySep1 :: TokenParser v -> TokenParser () -> TokenParser [v]
manySep1 p sep
  = (:) |- pSep`with`many p
    where
      pSep = const |- p`with`sep

manySep :: TokenParser v -> TokenParser () -> TokenParser [v]
manySep p sep
  = id`either`id|- pWithSep'`option`end'
    where
      end' = (:[])`either`const [] |- p`option`idT
      pWithSep' = (:) |- p`with` tail'
      tail' = (flip const|- sep`with`manySep p sep)

-- Actual parsers

parseId :: TokenParser Id
parseId (Token (Ident name') _:toks) = return (name', toks)
parseId toks = Left ("Expected Id in ", toks)

parseDef :: TokenParser Def
parseDef = (\(Call name' args')-> Def name' args') |- head'`with`parseExpr
  where
    head' = const |- parseExpr`with`tok DefinitionOperator

parseCall :: TokenParser Expr
parseCall = Call |- parseId`with`(id`either`const []|- args'`option`idT)
  where
    args' = const |- openArgs' `with` tok CloseParen
    openArgs' = flip const|- tok OpenParen`with`argsList'
    argsList' = parseArg`manySep`tok Comma

parseArg :: TokenParser Arg
parseArg = Kw`either`A |- parseDef`option`parseExpr

parseExpr :: TokenParser Expr
parseExpr = Dict`either`id |- parseScope`option`parseCall

parseDefs :: TokenParser [Def]
parseDefs = parseDef`manySep`optionalSep
  where
    optionalSep = id`either`id |- tok Comma`option`idT

parseScope :: TokenParser Scope
parseScope = const Scope |- tok OpenBrace`with`(const |- parseDefs`with`tok CloseBrace)

parseFile :: String -> IO Scope
parseFile file = do
  contents' <- readFile file
  case parse lexer "" contents' of
    Right toks -> parseTokens toks
    Left err' -> error $ show err'

parseTokens :: [Token] -> IO Scope
parseTokens toks
  = case parseDefs toks of
      Right (mod', []) -> return $ Scope mod'
      Right (_, extra) -> fail $ "Expected EOF, found "++show extra
      Left (msg, extra) -> fail $ msg ++ show extra
