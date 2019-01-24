module PredParser where

import Data.List (intersperse)
import Text.Parsec

import Util (indent)

type Name = String
type Path = String

data PTerm
  = TmFuncDef Info PFunc
  | TmFuncCall Info PTerm Scope
  | TmVar Info String
  -- deriving Show

instance Show PTerm where
  show (TmVar _i name) = name
  show (TmFuncCall _i name args) = show name ++ "(\n" ++ args'  ++ "\n)"
    where
      args' = indent $ concat $ intersperse ",\n" $ map (\(name', t) -> name'++ [assignmentOperator] ++ show t) args
  show (TmFuncDef _i func) = show func

type Scope = [(Name, PTerm)]

data PFunc = PFunc
  { name::Name
  , info::Info
  , args::[PTerm]
  , pre::[PTerm]
  , post::[PTerm]
  , invar::[PTerm] -- maybe should be called static?
  , defs::Scope -- internal scope
  }

instance Show PFunc where
  show f = name f++"("++args'++")\n"++pre'++invar'++post'++defs'
    where
      subclause f lst = indent $ (++"\n") $ concat $ intersperse ",\n" $ map f lst
      pre' = subclause (\t -> preConditionKeyword++[assignmentOperator]++show t) $ pre f
      invar' = subclause (\t -> invarConditionKeyword++[assignmentOperator]++show t) $ invar f
      post' = subclause (\t -> postConditionKeyword++[assignmentOperator]++show t) $ post f
      args' = concat $ intersperse ", " $ map show $ args f
      defs' = subclause (\(name', t) -> name' ++[assignmentOperator]++show t) $ defs f

data PModule = PModule Name Path Scope

data Info = Info
  { line :: Int
  , col :: Int
  } deriving (Show, Eq)


keywords :: [String]
keywords = [ preConditionKeyword
           , postConditionKeyword
           , invarConditionKeyword
           ]

postConditionKeyword :: String
postConditionKeyword = "post"

invarConditionKeyword :: String
invarConditionKeyword = "invar"

preConditionKeyword :: String
preConditionKeyword = "pre"

infoFrom :: SourcePos -> Info
infoFrom pos = Info
  { line = sourceLine pos
  , col = sourceColumn pos
  }

getInfo :: Parsec String u Info
getInfo = infoFrom <$> getPosition

openParen :: Parsec String u ()
openParen = do
  _ <- char '('
  _ <- many space
  return ()

closeParen :: Parsec String u ()
closeParen = do
  _ <- many space
  _ <- char ')'
  return ()

withWhiteSpace :: Parsec String u r -> Parsec String u r
type PParser = Parsec String () PTerm

newlineIndent :: Parsec String u ()
newlineIndent = do
  _ <- char '\n'
  _ <- char ' '
  _ <- char ' '
  return ()

withWhiteSpace exp = do
  r <- exp
  _ <- many $ char ' '<|>char '\n'
  return r

separator :: Parsec String u ()
separator = do
  _ <- withWhiteSpace $ char ','
  return ()

parseVarName :: Parsec String u String
parseVarName = do
  h <- letter
  ts <- many $ letter <|> digit <|> char '_'
  return (h:ts)

parseVar :: PParser
parseVar = do
  name <- label parseVarName "variable name (e.g. foo1)"
  pos <- getInfo
  return $ TmVar pos name

parseFuncDefArguments :: Parsec String () [PTerm]
parseFuncDefArguments = sepBy parseVar separator

assignmentOperator :: Char
assignmentOperator = '='

parseAssignmentOperator :: Parsec String u ()
parseAssignmentOperator = do
  _ <- many space
  _ <- char assignmentOperator
  _ <- many space
  return ()

parseAssignment :: Parsec String () (Name, PTerm)
parseAssignment = do
  name' <- try namedAssignment <|> anonAssignment
  term' <- parseTerm
  return (name', term')
  where
    namedAssignment = do
      name' <- parseVarName
      parseAssignmentOperator
      return name'
    anonAssignment
      = return "<TODO find name from context>"

parseFuncCallArguments :: Parsec String () [(Name, PTerm)]
parseFuncCallArguments = sepBy parseAssignment separator

parseFuncCall :: PParser
parseFuncCall = do
  name' <- try $ do
    name'' <- parseVar
    openParen
    return name''
  args' <- parseFuncCallArguments
  closeParen
  pos <- getInfo
  return $ TmFuncCall pos name' args'


parseFuncAssignment :: Parsec String () (Name, PTerm)
parseFuncAssignment = do
    try newlineIndent
    parseAssignment

parseFuncDef :: Parsec String () PFunc
parseFuncDef = do
  name' <- try $ do
    name'' <- parseVarName
    openParen
    return name''
  args' <- label parseFuncDefArguments "'(<arg> = <term>)'"
  closeParen
  pos <- getInfo
  defs' <- many parseFuncAssignment
  let defs'' = filter (not.(`elem`keywords).fst) defs'
  let preConditions = snd <$> filter ((==preConditionKeyword).fst) defs'
  let invarConditions = snd <$> filter ((==invarConditionKeyword).fst) defs'
  let postConditions = snd <$> filter ((==postConditionKeyword).fst) defs'
  _ <- many space
  return $ PFunc { name = name'
                , info = pos
                , args = args'
                , pre = preConditions
                , post = postConditions
                , invar = invarConditions
                , defs = defs''
                }

parseFuncDefTerm :: PParser
parseFuncDefTerm = do
  func' <- parseFuncDef
  return $ TmFuncDef (info func') func'

parseTerm :: PParser
parseTerm
  = parseFuncCall -- func(foo = var, bar = sum(a))
  <|> parseFuncDefTerm -- func(foo, bar, baz)\n\tbaz=foo+bar
  <|> parseVar


parseModule :: Parsec String () [PFunc]
parseModule = do
  defs <- many parseFuncDef
  _ <- eof
  return defs

parseWith :: Parsec String () a -> String -> Either ParseError a
parseWith p = runParser p () "Predicate Parser"

parse :: String -> Either ParseError [PFunc]
parse = parseWith parseModule
