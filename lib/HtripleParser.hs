module HtripleParser where

import Text.ParserCombinators.Parsec (parse)
-- import Control.Monad.State
import Lexer (lexer, Token(..), TokenType(..))

import Util ((|-), (-|), Pretty(pretty), prettyList)

-- info is used for passing context (e.g. source location, the stack)
type Id = String

data Arg
  = Kw Def
  | A Step
  deriving Show

instance Pretty Arg where
  pretty (Kw def) = pretty def
  pretty (A expr) = pretty expr

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
  pretty (CallExpr call) = pretty call
  pretty (Dict scope) = pretty scope

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
  pretty (Scope defs) = "{"++prettyList defs++"}"

type TokenParser tree = [Token] -> Either (String, [Token]) (tree, [Token])

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

orJust :: TokenParser v -> v -> TokenParser v
orJust p v = id`either`const v |- p`option`idT

many1 :: TokenParser v -> TokenParser [v]
many1 p = (:) |- p`with`many p

many :: TokenParser v -> TokenParser [v]
many p = many1 p`orJust`[]

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
      tail' = flip const|- sep`with`manySep p sep

between :: TokenParser () -> TokenParser a -> TokenParser () -> (a->b) -> TokenParser b
between open' val cls f = const f |- open'`with`(const |- val`with`cls)

-- Actual parsers

parseId :: TokenParser Id
parseId (Token (Ident name') _:toks) = return (name', toks)
parseId toks = Left ("Expected Id in ", toks)

parseDef :: TokenParser Def
parseDef = def' |- (const |- ((,) |- (parseId`with`parseArgList))`with`tok DefinitionOperator)`with`parseStep
  where
    def' :: (Id, [Arg]) -> Step -> Def
    def' (name', args') val' = Def name' args' val'

parseArgList :: TokenParser [Arg]
parseArgList = (id |- between (tok OpenParen) argsList' (tok CloseParen))`orJust`[]
  where
    argsList' :: TokenParser [Arg]
    argsList' = parseArg`manySep`tok Comma


parseCall :: TokenParser Call
parseCall = Call |- parseId`with`parseArgList

parseArg :: TokenParser Arg
parseArg = Kw`either`A |- parseDef`option`parseStep

parseExpr :: TokenParser Expr
parseExpr = Dict`either`CallExpr |- parseScope`option`parseCall

parseStep :: TokenParser Step
parseStep = (\pre'' (expr'', post'') -> Step pre'' expr'' post'') |- (pre'`orJust`[])`with`tail'
  where
    tail' :: TokenParser (Expr, [Expr])
    tail' = (,) |- parseExpr`with`(post'`orJust`[])
    pre' :: TokenParser [Expr]
    pre' = id |- between (const |- tok Minus`with`tok OpenBrace) (parseExpr`manySep`tok Comma) (tok CloseBrace)
    post' :: TokenParser [Expr]
    post' = id |- between (const |- tok Plus`with`tok OpenBrace) (parseExpr`manySep`tok Comma) (tok CloseBrace)

parseDefs :: TokenParser [Def]
parseDefs = parseDef`manySep`(tok Comma`orJust`())

parseScope :: TokenParser Scope
parseScope = Scope |- between (tok OpenBrace) parseDefs (tok CloseBrace)

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
