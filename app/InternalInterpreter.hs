module InternalInterpreter where
import System.Console.Haskeline
import Text.Read (readMaybe)
import Operation (Mem, exec)

runIR = runInputT defaultSettings (mainLoop [])

mainLoop :: Mem -> InputT IO ()
mainLoop mem
  = do
    outputStrLn $ show mem
    minput <- getInputLine "% "
    case minput of
      Nothing      -> return ()
      Just "exit"  -> return ()
      Just ""      -> mainLoop mem
      Just cmd -> do
        mem' <- handleCommand cmd mem
        mainLoop mem'

handleCommand :: String -> Mem -> InputT IO Mem
handleCommand cmd mem
  = case readMaybe cmd of
      Just ins -> return $ exec ins mem
      Nothing -> do
        outputStrLn $ "Sorry '"++cmd++"' couldn't be parsed."
        return mem
