module PredParser where

import Data.List (intersperse)

import Text.ParserCombinators.Parsec
import Language
import Util (indent)

type Name = String
type Path = String

data PTerm
  = TmFuncDef PFunc
  | TmFuncCall Info PTerm Scope
  | TmVar Info String
  -- deriving Show

instance Show PTerm where
  show (TmVar _i name') = name'
  show (TmFuncCall _i name' args'') = show name' ++ "(" ++ args'  ++ ")"
    where
      args' = concat $ intersperse ", " $ map (\(name'', t) -> name''++ assignmentOperator ++ show t) args''
  show (TmFuncDef func) = show func

type Scope = [(Name, PTerm)]

data PFunc = PFunc
  { name::PTerm
  , info::Info
  , args::[PTerm]
  , pre::[PTerm]
  , post::[PTerm]
  , invar::[PTerm] -- maybe should be called static?
  , defs::Scope -- internal scope
  }

instance Show PFunc where
  show fun = show(name fun)++"("++args'++") {"++contents++"}"
    where
      contents = pre'++invar'++post'++defs'
      subclause f lst = indent $ concat $ intersperse ", " $ map f lst
      keyw kw getter = subclause (\t -> kw++assignmentOperator++show t) $ getter fun
      pre' = keyw preConditionKeyword pre
      invar' = keyw invarConditionKeyword invar
      post' = keyw postConditionKeyword post
      args' = concat $ intersperse ", " $ map show $ args fun
      defs' = subclause (\(name', t) -> name' ++assignmentOperator++show t) $ defs fun

data PModule = PModule Name Path Scope

data Info = Info
  { line :: Int
  , col :: Int
  } deriving (Show, Eq)

infoFrom :: SourcePos -> Info
infoFrom pos = Info
  { line = sourceLine pos
  , col = sourceColumn pos
  }

getInfo :: Parser Info
getInfo = infoFrom <$> getPosition

separator :: Parser ()
separator = do
  _ <- many space >> char ',' >> many space
  return ()

listOf :: Parser a -> Parser [a]
listOf p = sepBy p separator

variable :: Parser PTerm
variable = do
  name' <- label identifier "variable name (e.g. foo1)"
  pos <- getInfo
  return $ TmVar pos name'

assignment :: Parser (Name, PTerm)
assignment = do
  _ <- many space
  name' <- identifier
  _ <- many space >> reservedOp assignmentOperator >> many space
  term' <- statement
  return (name', term')

implicitAssignment :: Parser (Name, PTerm)
implicitAssignment = try assignment <|> do
  term' <- statement
  return ("_n", term') -- TODO(jopra): Find good values for 'n' here

nameArgs :: Parser a -> Parser (PTerm, [a])
nameArgs argType = do
  name' <- variable
  args' <- parens (listOf argType)
  return (name', args')

funcCall :: Parser PTerm
funcCall = do
  (name', args') <- try $ nameArgs implicitAssignment
  pos <- getInfo
  return $ TmFuncCall pos name' args'

funcDef :: Parser PFunc
funcDef = do
  (name', args') <- try $ nameArgs variable
  _ <- char '{'
  pos <- getInfo
  defs' <- many assignment
  _ <- many space
  _ <- char '}'
  _ <- many space
  let defs'' = filter (not.(`elem`keywords).fst) defs'
  let preConditions = snd <$> filter ((==preConditionKeyword).fst) defs'
  let invarConditions = snd <$> filter ((==invarConditionKeyword).fst) defs'
  let postConditions = snd <$> filter ((==postConditionKeyword).fst) defs'
  return $ PFunc { name = name'
                , info = pos
                , args = args'
                , pre = preConditions
                , post = postConditions
                , invar = invarConditions
                , defs = defs''
                }

funcDefinition :: Parser PTerm
funcDefinition = TmFuncDef <$> funcDef

statement :: Parser PTerm
statement
  = funcCall -- func(foo = var, bar = sum(a))
  <|> funcDefinition -- func(foo, bar, baz)\n\tbaz=foo+bar
  <|> variable

moduleDef :: Parser [PFunc]
moduleDef = do
  _ <- whiteSpace
  defs' <- many funcDef
  _ <- eof
  return defs'

parseString :: String -> Either ParseError [PFunc]
parseString = parse moduleDef ""

parseFile :: String -> IO (Either ParseError [PFunc])
parseFile file = parse moduleDef file <$> readFile file
