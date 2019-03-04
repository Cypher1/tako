module HtripleParser where

-- import Control.Monad.State
import Lexer (Token(..))


infixl 0 |-, -|
(|-) = flip ($)
(-|) = ($)

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x++", "++pretty xs

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

-- info is used for passing context (e.g. source location, the stack)
data Id = Id String
  deriving Show

instance Pretty Id where
  pretty (Id name) = name

data Expr
  = Call Id [Expr]
  | Dict Scope
  deriving Show

instance Pretty Expr where
  pretty (Call name []) = pretty name
  pretty (Call name args) = pretty name++"("++pretty args++")"
  pretty (Dict scope) = pretty scope

data Def = Def Expr Expr
  deriving Show

instance Pretty Def where
  pretty (Def from to) = pretty from++"="++pretty to

data Scope = Scope [Def]
  deriving Show

instance Pretty Scope where
  pretty (Scope defs) = "{"++pretty defs++"}"

data Module = Module Id Scope
  deriving Show

instance Pretty Module where
  pretty (Module name' defs) = pretty name'++pretty defs


type TokenParser tree =  [Token] -> Either (String, [Token]) (tree, [Token])

idT :: TokenParser ()
idT toks = return ((), toks)

tok :: Token -> TokenParser ()
tok exp (t:toks) | exp == t = return ((), toks)
tok exp toks = Left ("Expected "++show exp++" in ", toks)

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
parseId (Ident name':toks) = return (Id name', toks)
parseId toks = Left ("Expected Id in ", toks)

parseDef :: TokenParser Def
parseDef = Def |- head'`with`parseExpr
  where
    head' = const |- parseExpr`with`tok DefinitionOperator

parseCall :: TokenParser Expr
parseCall = Call |- parseId`with`(id`either`const []|- args'`option`idT)
  where
    args' = const |- openArgs' `with` tok CloseParen
    openArgs' = flip const|- tok OpenParen`with`argsList'
    argsList' = parseExpr`manySep`tok Comma

parseExpr :: TokenParser Expr
parseExpr = Dict`either`id |- parseScope`option`parseCall

parseScope :: TokenParser Scope
parseScope = const Scope |- tok OpenBrace`with`scope'
  where
    scope' = const |- (parseDef`manySep`optionalSep)`with`tok CloseBrace
    optionalSep = id`either`id |- tok Comma`option`idT

parseModule :: TokenParser Module
parseModule = Module |- parseId`with`parseScope
