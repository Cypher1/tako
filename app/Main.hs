module Main where
import           InternalInterpreter            ( runIR )
import           System.Console.ParseArgs
import           Control.Monad                  ( when
                                                , unless
                                                )
import           Paths_tako                     ( version )
import           Data.Version                   ( showVersion )

takoInfo :: String
takoInfo =
  "The tako compiler and proof assistant, version " ++ showVersion version

argsDef :: [Arg String]
argsDef =
  [ Arg
    { argIndex = "verbosity"
    , argAbbr  = Just 'v'
    , argName  = Just "verbosity"
    , argData  = argDataDefaulted "level" ArgtypeInt 0
    , argDesc  = "Level of detail used in output (default 0)"
    }
  , Arg
    { argIndex = "help"
    , argAbbr  = Just 'h'
    , argName  = Just "help"
    , argData  = Nothing
    , argDesc  = "Print this usage information"
    }
  , Arg
    { argIndex = "version"
    , argAbbr  = Just 'V'
    , argName  = Just "version"
    , argData  = Nothing
    , argDesc  = "Print version = " ++ showVersion version ++ ")"
    }
  , Arg
    { argIndex = "interactive"
    , argAbbr  = Just 'i'
    , argName  = Just "interactive"
    , argData  = Nothing
    , argDesc  = "Turn on interactive mode"
    }
  , Arg
    { argIndex = "steps"
    , argAbbr  = Just 's'
    , argName  = Just "steps"
    , argData  = argDataOptional "steps" ArgtypeInt
    , argDesc  = "Run compilation steps up to step <step> (default all)"
    }
  , Arg
    { argIndex = "output"
    , argAbbr  = Just 'o'
    , argName  = Just "output"
    , argData  = argDataDefaulted "file" ArgtypeString "a.out"
    , argDesc  = "Write output to <file>"
    }
  ]

expect :: String -> Maybe a -> a
expect _   (Just a) = a
expect msg Nothing  = error msg

printL :: Show a => String -> a -> IO ()
printL l v = putStrLn $ l ++ ": " ++ show v

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argsDef
  let version' = gotArg args "version"
  when version' $ putStrLn takoInfo
  let help = gotArg args "help"
  when help $ putStrLn $ argsUsage args
  unless (help || version') $ main' args

main' :: Args String -> IO ()
main' args = do
  let verbosity = expect "verbosity" (getArg args "verbosity" :: Maybe Int)
  printL "Verbosity" verbosity
  let steps = getArg args "steps" :: Maybe Int
  printL "Last Step" steps
  let interactive = gotArg args "interactive"
  printL "Interactive Mode" interactive
  let output = expect "output" (getArg args "output" :: Maybe String)
  printL "Output" output
  let input = argsRest args
  printL "Input" input
  runIR
