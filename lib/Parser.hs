{-# LANGUAGE InstanceSigs #-}
module Parser where

import           Data.Functor.Identity          ( Identity )
import           Control.Applicative            ( Alternative(..) )
import           Text.Parsec                    ( ParsecT
                                                , parse
                                                , eof
                                                , token
                                                , (<?>)
                                                , SourcePos
                                                , try
                                                )
import           Lexer                          ( lexer
                                                , at
                                                , Token(..)
                                                , TokenType(..)
                                                )

import           Util                           ( Pretty(pretty)
                                                , prettySet
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

type Id = String

data Arg
  = A Int Step
  | Kw Def
  deriving (Show, Ord, Eq)

instance Pretty Arg where
  pretty (Kw def') = pretty def'
  pretty (A _ expr') = pretty expr'

data Call = Call Id (Set Arg)
  deriving (Show, Ord, Eq)

instance Pretty Call where
  pretty (Call name' args)
    = if S.null args
         then name'
         else name'++"("++prettySet args++")"

data Expr
  = CallExpr Call
  | Dict Scope
  deriving (Show, Ord, Eq)

instance Pretty Expr where
  pretty (CallExpr call') = pretty call'
  pretty (Dict scope') = pretty scope'

data Step
  = Step { pre :: Set Expr, op :: Expr, post :: Set Expr }
  deriving (Show, Ord, Eq)

instance Pretty Step where
  pretty st = pre'++pretty (op st)++post'
    where
      pre'
        | null $ pre st = ""
        | otherwise = "-{"++prettySet (pre st)++"}"
      post'
        | null $ post st = ""
        | otherwise = "+{"++prettySet (post st)++"}"

data Def = Def Id (Set Arg) Step
  deriving (Show, Ord, Eq)

instance Pretty Def where
  pretty (Def name args to) = pretty (Call name args) ++"="++pretty to

newtype Scope = Scope (Set Def)
  deriving (Show, Ord, Eq)

instance Pretty Scope where
  pretty (Scope defs') = "{"++prettySet defs'++"}"

type Parser = ParsecT [Token] () Identity

posFromTok :: Token -> SourcePos
posFromTok (Token _t inf) = at inf

tok :: TokenType -> Parser TokenType
tok exp' = token pretty posFromTok testTok <?> ("a " ++ pretty exp')
 where
  testTok (Token t _inf) | exp' == t = Just t
                         | otherwise = Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- Actual parsers

ident :: Parser Id
ident = token pretty posFromTok testTok
 where
  testTok (Token t _) = case t of
    Ident name' -> Just name'
    _           -> Nothing

def :: Parser Def
def = Def <$> ident <*> argList <* tok DefinitionOperator <*> step

argList :: Parser (Set Arg)
argList = (tok OpenParen *> args' <* tok CloseParen) <|> return S.empty
 where
  args' = numberArgs <$> (arg `sepBy` tok Comma) <?> "a list of arguments"

numberArgs :: [Arg] -> Set Arg
numberArgs = S.fromList . na' 0
 where
  na' _ []              = []
  na' n (A (-1) x : xs) = A n x : na' (n + 1) xs
  na' _ (A k x : xs) =
    error
      $  "Expected Arg to be unnumbered but had number "
      ++ show k
      ++ ", "
      ++ show (A k x : xs)
  na' n (Kw def' : xs) = Kw def' : na' n xs

call :: Parser Call
call = Call <$> ident <*> argList

arg :: Parser Arg
arg =
  (try (Kw <$> def) <?> "a keyword argument")
    <|> ((A (-1) <$> step) <?> "an argument")

expr :: Parser Expr
expr = (Dict <$> scope) <|> (CallExpr <$> call)

step :: Parser Step
step = Step <$> preds' Minus <*> expr <*> preds' Plus
 where
  preds' :: TokenType -> Parser (Set Expr)
  preds' t = (tok t *> ((tok OpenBrace *> predTail') <?> fl')) <|> pure S.empty
  fl'       = "a '{' (at the start of a set of assertions)"
  predTail' = (reqs' <* tok CloseBrace) <?> "a list of predicates"
  reqs'     = S.fromList <$> expr `sepBy` tok Comma

defs :: Parser (Set Def)
defs = S.fromList <$> def `sepBy` (tok Comma <|> pure Comma)

scope :: Parser Scope
scope = Scope <$> (tok OpenBrace *> ((defs <* tok CloseBrace) <?> fl'))
  where fl' = "a set of declarations followed by a '}'"

parseFile :: String -> IO Scope
parseFile file = do
  contents' <- readFile file
  case parse lexer file contents' of
    Right toks' -> return $ tokens file toks'
    Left  err'  -> error $ show err'

tokens :: String -> [Token] -> Scope
tokens file toks' = case parse (defs <* eof) file toks' of
  Right mod' -> Scope mod'
  Left  err  -> error $ show err
