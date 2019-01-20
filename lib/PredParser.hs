module PredParser where
import Debug.Trace (trace)
-- import Pred

import Text.Parsec
-- import Data.List (elemIndex)

type Name = String
type Path = String

data PTerm
  = TmFuncDef Info PFunc
  | TmFuncCall Info PTerm Scope
  | TmVar Info String
  deriving Show

type Scope = [(Name, PTerm)]

data PFunc = PFunc
  { name::Name
  , info::Info
  , args::[PTerm]
  , pre::[PTerm]
  , post::[PTerm]
  , invar::[PTerm] -- maybe should be called static?
  , defs::Scope -- internal scope
  } deriving Show

data PModule = PModule Name Path Scope

data Info = Info
  { line :: Int
  , col :: Int
  } deriving (Show, Eq)

type BoundContext = [Name]

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
withWhiteSpace exp = do
  r <- exp
  _ <- many $ char ' '<|>char '\n'
  return r

type PParser = Parsec String BoundContext PTerm

newlineIndent :: Parsec String u ()
newlineIndent = do
  _ <- char '\n'
  _ <- char ' '
  _ <- char ' '
  return ()

parseVarName :: Parsec String u String
parseVarName = do
  h <- trace ">> parseVarName" letter
  ts <- many $ letter <|> digit <|> char '_'
  return (h:ts)

parseVar :: PParser
parseVar = do
  name <- label parseVarName "variable name (e.g. foo1)"
  pos <- getInfo
  return $ TmVar pos name

parseFuncDefArguments :: Parsec String BoundContext [PTerm]
parseFuncDefArguments = sepBy parseVar (withWhiteSpace (char ','))

parseAssignment :: Parsec String BoundContext (Name, PTerm)
parseAssignment = do
  name' <- parseVarName
  _ <- many space
  _ <- char '='
  _ <- many space
  term' <- parseTerm
  return (name', term')

parseFuncCallArguments :: Parsec String BoundContext [(Name, PTerm)]
parseFuncCallArguments = trace ">> parseFuncCallArguments" $
  sepBy parseAssignment $ withWhiteSpace $ char ','

parseFuncCall :: PParser
parseFuncCall = do
  name' <- try $ do
    name'' <- trace ">> parseFuncCall" parseVar
    _ <- openParen
    return name''
  args' <- parseFuncCallArguments
  _ <- closeParen
  pos <- getInfo
  return $ TmFuncCall pos name' args'


parseFuncAssignment :: Parsec String BoundContext (Name, PTerm)
parseFuncAssignment = do
    _ <- try newlineIndent
    parseAssignment

parseFuncDef :: Parsec String BoundContext PFunc
parseFuncDef = do
  name' <- try $ do
    name'' <- parseVarName
    _ <- openParen
    return name''
  args' <- label parseFuncDefArguments "'(<arg> = <term>)'"
  _ <- closeParen
  pos <- getInfo
  defs' <- many parseFuncAssignment
  _ <- many space
  return $ PFunc { name = name'
                , info = pos
                , args = args'
                , pre = []
                , post = []
                , invar = []
                , defs = defs'
                }

parseFuncDefTerm :: PParser
parseFuncDefTerm = do
  func' <- trace ">> parseFuncDefTerm" parseFuncDef
  return $ TmFuncDef (info func') func'

parseTerm :: PParser
parseTerm
  = parseFuncCall -- func(foo = var, bar = sum(a))
  <|> parseFuncDefTerm -- func(foo, bar, baz)\n\tbaz=foo+bar
  <|> parseVar


parseModule :: Parsec String [Name] [PFunc]
parseModule = do
  defs <- many parseFuncDef
  _ <- eof
  return defs

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Predicate Parser"

parse :: String -> Either ParseError [PFunc]
parse = parseWith parseModule
