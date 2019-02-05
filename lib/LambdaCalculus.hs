module LambdaCalculus where
import Text.Parsec
-- import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)

data Term
  = TmVar Info Int Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Show, Eq)

data Info = Info { row :: Int, col :: Int } deriving (Show, Eq)

type BoundContext = [String]

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

type LCParser = Parsec String BoundContext Term


parseVarName :: Parsec String u String
parseVarName = do
  h <- letter
  ts <- many $ letter <|> digit
  return (h:ts)

parseAbs :: LCParser -> LCParser
parseAbs termParser = do
  _ <- char '\\'
  v <- parseVarName
  modifyState (v:)
  _ <- char '.'
  term <- termParser
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser
parseVar = do
  v <- parseVarName
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseNonApp :: LCParser
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs parseTerm -- $\lambda$x.M
           <|> parseVar           -- x

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
  _ <- space
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseLine :: LCParser
parseLine = do
  term' <- parseTerm
  _ <- char ';'
  return term'

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

parse :: String -> Either ParseError Term
parse = parseWith parseLine


data Binding = NameBind deriving (Show)

type Context = [((String, Int), Binding)]

ctxLength :: Context -> Int
ctxLength = length

indexToName :: Context -> Int -> String
indexToName ctx n = (\(x, n') -> showNumbered x n') $ fst $ ctx !! n

showNumbered :: String -> Int -> String
showNumbered x 0 = x
showNumbered x n' = x++show (n'-1)

pickFreshName :: Context -> (String, Int) -> (Context, String)
pickFreshName ctx (x, n)
  | (x, n) `elem` map fst ctx = pickFreshName ctx (x, n+1)
  | otherwise = (((x, n), NameBind) : ctx, showNumbered x n)

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmAbs _ x t1 -> let
      (ctx', x') = pickFreshName ctx (x,0)
    in "\\" ++ x' ++ "." ++ printTm ctx' t1 ++ ""
  TmApp _ t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  TmVar _ x n ->
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"

-- beta reduction
run :: Term -> Term
run (TmApp _i (TmAbs _i2 _vn func) arg') = replace arg' func
run app@(TmApp i f a)
  | app == napp = app
  | otherwise = run napp
  where
    napp = TmApp i (run f) a
run abs'@(TmAbs i vn f)
  | abs' == nabs = abs'
  | otherwise = run nabs
  where
    nabs = TmAbs i vn (run f)
run v = v

updateIndexes :: (Int -> Int) -> (Int -> Int) -> Term -> Term
updateIndexes d n (TmVar i dbi num) = TmVar i (d dbi) (n num)
updateIndexes d n (TmAbs i vn func) = TmAbs i vn (updateIndexes d n func)
updateIndexes d n (TmApp i func arg) = TmApp i func (updateIndexes d n arg)

replace :: Term -> Term -> Term
replace = replace' 0
  where
    replace' d arg v@(TmVar _i dbi num)
      | dbi == (num-1) = updateIndexes (+0) (+d) arg
      | otherwise = updateIndexes (+0) (\x->x-1) v
    replace' d arg (TmAbs i var_name func) = TmAbs i var_name (replace' (d+1) arg func)
    replace' d arg (TmApp i arg' func) = TmApp i (replace' d arg arg') (replace' d arg func)
