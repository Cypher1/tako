module PredParser where

import Pred

import Text.Parsec
import Data.List (elemIndex)

data PTerm
  = TmVar Info Int Int
  | TmAbs Info String PTerm
  | TmApp Info PTerm PTerm

data Info = Info
  { line :: Int
  , col :: Int
  } deriving (Show, Eq)

type BoundContext = [String]

infoFrom :: SourcePos -> Info
infoFrom pos = Info
  { line = sourceLine pos
  , col = sourceColumn pos
  }

type PParser = Parsec String BoundContext PTerm

parseVarName :: Parsec String u String
parseVarName = do
  h <- letter
  ts <- many $ letter <|> digit
  return (h:ts)

parseAbs :: PParser -> PParser
parseAbs termParser = do
  _ <- char '\\'
  v <- parseVarName
  modifyState (v:)
  _ <- char '.'
  term <- termParser
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

parseVar :: PParser
parseVar = do
  v <- parseVarName
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> PParser
findVar v list
  = case elemIndex v list of
      Nothing -> fail $ "The variable '" ++ v ++ "' has not been bound"
      Just n -> do
        pos <- getPosition
        return $ TmVar (infoFrom pos) n (length list)

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseNonApp :: PParser
parseNonApp
  = parens parseTerm -- (M)
  <|> parseAbs parseTerm -- $\lambda $x.M
  <|> parseVar

parseTerm :: PParser
parseTerm
  = chainl1 parseNonApp $ do
    _ <- space
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseLine :: PParser
parseLine = do
  term' <- parseTerm
  _ <- char ';'
  return term'

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Predicate Parser"

parse :: String -> Either ParseError PTerm
parse = parseWith parseLine
