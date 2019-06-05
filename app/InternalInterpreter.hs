module InternalInterpreter where
import           System.Console.Haskeline
import           Parser                         ( convert )
import           PrimOpType                     ( Mem )
import           Operation                      ( exec )
import           Util                           ( pretty )

runIR :: IO ()
runIR = runInputT defaultSettings (mainLoop [])

mainLoop :: Mem -> InputT IO ()
mainLoop mem = do
  outputStrLn $ concat [ s ++ "=" ++ pretty v ++ " " | (s, v) <- mem ]
  minput <- getInputLine "% "
  case minput of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just ""     -> mainLoop mem
    Just cmd    -> do
      mem' <- handleCommand cmd mem
      mainLoop mem'

handleCommand :: String -> Mem -> InputT IO Mem
handleCommand cmd mem = case convert cmd of
  Right ins -> return $ exec ins mem
  Left  err -> do
    outputStrLn $ "Sorry '" ++ cmd ++ "' couldn't be parsed.\n\t" ++ show err
    return mem
