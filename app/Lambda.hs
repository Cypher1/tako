module Main where
import LambdaCalculus (parse, printTm, Term (TmVar, TmAbs, TmApp))

main :: IO ()
main
  = do
    interpret "\\x.x;"
    interpret "\\x.\\y.x y;"
    interpret "\\x.\\y.y x;"
    interpret "((\\f.\\x.f (f x)) (\\y.(\\x.y))) (\\a.a) (\\b.b) (\\c.c) (\\d.d);"
    interpret "(\\x.(\\y.x y)) (\\z.z);"
    interpret "(\\x.(\\y.x y)) (\\z.z) (\\a.a);"
    interpret "(\\x.(\\y.y x)) (\\z.z) (\\a.a);"

interpret :: String -> IO ()
interpret prog
  = do
    putStrLn prog
    let p = parse prog
    case p of
      Left err -> print err
      Right parsed -> do
        -- print parsed
        putStrLn $ "<- " ++ (printTm [] parsed)
        putStrLn $ "-> " ++ (printTm [] $ run $ parsed)
