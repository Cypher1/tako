{-# LANGUAGE InstanceSigs #-}
module Parser where

import           Data.Functor.Identity          ( Identity )
import           Control.Applicative            ( Alternative((<|>)) )
import           Control.Monad                  ( void )
import           Text.Parsec                    ( ParsecT
                                                , ParseError
                                                , parse
                                                , eof
                                                , token
                                                -- , getPosition
                                                -- , getParserState
                                                -- , stateInput
                                                , choice
                                                , (<?>)
                                                , try
                                                , many
                                                )
import           Language                       ( TokenType(..)
                                                , PrimOpType(..)
                                                , PrimValOpType(..)
                                                , PrimUnOpType(..)
                                                , PrimBiOpType(..)
                                                , PrimTriOpType(..)
                                                , lexer
                                                , at
                                                , Token(..)
                                                , TokType(..)
                                                )
import           PrimOpType                     ( PrimOp(..) )
import           PrimType                       ( Value(..) )
import           Util                           ( Pretty(pretty)
                                                , prettySet
                                                , prettyList
                                                , pretty'
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

-- import           Debug.Trace                    ( trace )

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
  | Ops [PrimOp]
  | Step { pre :: Set Expr, ops :: Set Expr, post :: Set Expr}
  deriving (Show, Ord, Eq)

instance Pretty Step where
  pretty (CallStep call') = pretty call'
  pretty (Ops ops') = prettyList ops'
  pretty st = "{"++pre'++" "++ops'++" "++post'++"}"
    where
      pre'
        | null (pre st) = ""
        | otherwise = " "++prettySet (pre st)++" -|"
      post'
        | null (post st) = ""
        | otherwise = "|- " ++ prettySet (post st)++" "
      ops' = prettySet $ ops st

type Parser = ParsecT [Token] () Identity

primL :: Parser PrimValOpType
primL = tryTok test <?> "a value-load"
 where
  test (Token (Op (PrimVal op')) _) = Just op'
  test _                            = Nothing

primU :: Parser PrimUnOpType
primU = tryTok test <?> "a unary-operation"
 where
  test (Token (Op (PrimUn op')) _) = Just op'
  test _                           = Nothing

primB :: Parser PrimBiOpType
primB = tryTok test <?> "a binary-operation"
 where
  test (Token (Op (PrimBi op')) _) = Just op'
  test _                           = Nothing

primT :: Parser PrimTriOpType
primT = tryTok test <?> "a ternary-operation"
 where
  test (Token (Op (PrimTri op')) _) = Just op'
  test _                            = Nothing

tok :: TokType -> Parser TokType
tok tok' = tryTok test <?> pretty' tok'
 where
  test (Token (Tok t) _inf) | tok' == t = Just tok'
  test _ = Nothing

tryTok :: (Token -> Maybe a) -> Parser a
tryTok = token pretty (\(Token _ inf) -> at inf)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

pTrace :: Show a => String -> Parser a -> Parser a
pTrace _ p = p
{- pTrace lbl p       = do
  pos <- getPosition
  seeNext ("\n" ++ lbl ++ ": " ++ show pos ++ " \n  ") 3 *> p <|> trace
    (lbl ++ " fail")
    (fail lbl)

seeNext :: String -> Int -> Parser ()
seeNext msg n = do
  s <- getParserState
  let out = take n (stateInput s)
  trace (msg ++ show out) $ return ()
-}

-- Parser usage


parseFile :: String -> IO (Set Expr)
parseFile file = fromTokens file <$> tokenizeFile file

tokenizeFile :: String -> IO [Token]
tokenizeFile file = tokenizeString file <$> readFile file

tokenizeString :: String -> String -> [Token]
tokenizeString file contents' = case parse lexer file contents' of
  Right toks' -> toks'
  Left  err'  -> error $ show err'

fromTokens :: String -> [Token] -> Set Expr
fromTokens file toks' = case parse (exprs <* eof) file toks' of
  Right mod' -> mod'
  Left  err  -> error $ show err

parsePrimOpsFile :: String -> Either ParseError [PrimOp]
parsePrimOpsFile cnts = primFromTokens <$> parse lexer "?" cnts
 where
  primFromTokens toks' = case parse (many primOp <* eof) "?" toks' of
    Right mod' -> mod'
    Left  err  -> error $ show err

convert :: String -> Either ParseError PrimOp
convert s = parse (primOp <* eof) "stdin" (tokenizeString "stdin" s)

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

-- PrimOp Parsers

primOp :: Parser PrimOp
primOp = choice $ try <$> pops
 where
  pops =
    [ L <$> primL <*> litValue <*> ident
    , U <$> primU <*> ident
    , B <$> primB <*> ident <*> ident
    , T <$> primT <*> ident <*> ident <*> ident
    ]

-- Actual parsers

litValue :: Parser Value
litValue = pTrace "Values" $ vals <|> pTrace "Value" val
 where
  vals =
    Values
      <$> (   (tok OpenParen *> many litValue <* tok CloseParen)
          <|> ([] <$ tok Comma)
          )
  val = Value <$> (fromInteger <$> litInt) <*> litValue

ident :: Parser Id
ident = tryTok test <?> "an identifier"
 where
  test (Token (Ident name') _) = Just name'
  test _                       = Nothing

litInt :: Parser Integer
litInt = tryTok test <?> "an integer"
 where
  test (Token t _) = case t of
    LitInt name' -> Just name'
    _            -> Nothing

expr :: Parser Expr
expr = try (pTrace "ass" (Kw <$> call <*> (tok DefOp *> step)))
  <|> pTrace "step" (A (-1) <$> step <?> "an argument")

step :: Parser Step
step = try (pTrace "step" step') <|> call' <|> prims'
 where
  step' =
    Step
      <$> (tok OpenBrace *> pre')
      <*> exprs
      <*> (post' <* tok CloseBrace)
      <?> "an expression with guards"
  pre'   = try (exprs <* tok RequireOp) <|> pure S.empty
  post'  = try (tok ProvideOp *> exprs) <|> pure S.empty
  call'  = pTrace "stepCall" (CallStep <$> call)
  prims' = pTrace "primOps" (Ops <$> primOp `sepBy1` tok Semi)

call :: Parser Call
call = Call <$> pTrace "call" ident <*> argList
 where
  argList =
    pTrace "argList"
      $   (wrappedExprs OpenParen CloseParen <?> "a list of arguments")
      <|> return S.empty


wrappedExprs :: TokType -> TokType -> Parser (Set Expr)
wrappedExprs h t = tok h *> exprs <* tok t

exprs :: Parser (Set Expr)
exprs = numberExprs <$> (expr `sepBy` comma')
  where comma' = void (tok Comma) <|> pure ()
