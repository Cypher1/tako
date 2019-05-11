{-# LANGUAGE InstanceSigs #-}
module Parser where

import           Data.Functor.Identity          ( Identity )
import           Control.Applicative            ( Alternative(..) )
import           Text.Parsec                    ( ParsecT
                                                , parse
                                                , eof
                                                , token
                                                , getPosition
                                                , (<?>)
                                                , getParserState
                                                , stateInput
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
-- import Text.Parsec
-- import Text.Parsec.Prim

import           Debug.Trace                    ( trace )

type Id = String

data Expr
  = A Int Step
  | Kw Call Step
  deriving (Show, Ord, Eq)

instance Pretty Expr where
  pretty (Kw call' val') = pretty call'++"="++pretty val'
  pretty (A _ expr') = pretty expr'

data Call = Call Id (Set Expr)
  deriving (Show, Ord, Eq)

instance Pretty Call where
  pretty (Call name args)
    | S.null args = name
    | otherwise = name++"("++prettySet args++")"

data Step
  = CallStep Call
  | Step { pre :: Set Expr, op :: Set Expr, post :: Set Expr}
  deriving (Show, Ord, Eq)

instance Pretty Step where
  pretty (CallStep call') = pretty call'
  pretty st = "{"++pre'++" "++op'++" "++post'++"}"
    where
      pre'
        | null (pre st) = ""
        | otherwise = " "++prettySet (pre st)++" -|"
      post'
        | null (post st) = ""
        | otherwise = "|- " ++ prettySet (post st)++" "
      op' = prettySet $ op st

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

pTrace :: Show a => String -> Parser a -> Parser a
pTrace _ p | 1 > 0 = p
pTrace lbl p       = do
  pos <- getPosition
  seeNext ("\n" ++ lbl ++ ": " ++ show pos ++ " \n  ") 3 *> p <|> trace
    (lbl ++ " fail")
    (fail lbl)

seeNext :: String -> Int -> Parser ()
seeNext msg n = do
  s <- getParserState
  let out = take n (stateInput s)
  trace (msg ++ show out) $ return ()

-- Parser usage

parseFile :: String -> IO (Set Expr)
parseFile file = fromTokens file <$> tokenizeFile file

tokenizeFile :: String -> IO [Token]
tokenizeFile file = do
  contents' <- readFile file
  case parse lexer file contents' of
    Right toks' -> return $ toks'
    Left  err'  -> error $ show err'

fromTokens :: String -> [Token] -> (Set Expr)
fromTokens file toks' = case parse (exprs <* eof) file toks' of
  Right mod' -> mod'
  Left  err  -> error $ show err

numberExprs :: [Expr] -> Set Expr
numberExprs = S.fromList . snd . foldl na' (0, [])
 where
  na' (n, xs) (A (-1) x) = (n + 1, A n x : xs)
  na' (_, xs) a@(A k _) =
    error
      $  "Expected Expr to be unnumbered but had number "
      ++ show k
      ++ ", "
      ++ show a
      ++ " in "
      ++ show xs
  na' (n, xs) k = (n, k : xs)

-- Actual parsers

ident :: Parser Id
ident = token pretty posFromTok testTok
 where
  testTok (Token t _) = case t of
    Ident name' -> Just name'
    _           -> Nothing

expr :: Parser Expr
expr =
  try (pTrace "ass" (Kw <$> call <*> (tok DefOp *> step)))
    <|> (pTrace "step" (A (-1) <$> step <?> "an argument"))

step :: Parser Step
step = try (pTrace "step" step') <|> call'
 where
  step' =
    Step
      <$> (tok OpenBrace *> pre')
      <*> exprs
      <*> (post' <* tok CloseBrace)
      <?> "an expression with guards"
  pre'  = (try (exprs <* tok RequireOp)) <|> pure S.empty
  post' = (try (tok ProvideOp *> exprs)) <|> pure S.empty
  call' = pTrace "stepCall" (CallStep <$> call)

call :: Parser Call
call = Call <$> (pTrace "call" ident) <*> argList
 where
  argList =
    pTrace "argList"
      $   (wrappedExprs OpenParen CloseParen <?> "a list of arguments")
      <|> return S.empty


wrappedExprs :: TokenType -> TokenType -> Parser (Set Expr)
wrappedExprs h t = (tok h *> exprs <* tok t)

exprs :: Parser (Set Expr)
exprs = numberExprs <$> (expr `sepBy` (tok Comma <|> pure Comma))
