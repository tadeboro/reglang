module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Enumerator

-- Flag data type
data Options = Options
  { offset  :: [String] -> [String]
  , fetch   :: [String] -> [String]
  , longer  :: [String] -> [String]
  , shorter :: [String] -> [String]
  }

defaultOptions = Options
  { offset  = id
  , fetch = id
  , longer = id
  , shorter = id
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o'] ["offset"]
      (ReqArg (\a o -> o {offset = drop (read a)}) "NUM")
      "NUM of words to skip"
  , Option ['f'] ["fetch"]
      (ReqArg (\a o -> o {fetch = take (read a)}) "NUM")
      "NUM of words to print"
  , Option ['l'] ["longer"]
      (ReqArg (\a o -> o {longer = dropWhile (\x -> read a > length x)}) "NUM")
      "print only words longer than NUM characters"
  , Option ['s'] ["shorter"]
      (ReqArg (\a o -> o {shorter = takeWhile (\x -> read a > length x)}) "NUM")
      "print only words shorter than NUM characters"
  ]

getOptions :: [String] -> IO (Options, String)
getOptions argv =
  case getOpt Permute options argv of
    (o, [e], []) -> return (foldl (flip id) defaultOptions o, e)
    (_, _  , es) -> ioError $ userError $ concat es ++ usageInfo header options
      where header = "Usage: listlang [OPTION...] regex"

adjust :: Options -> [String] -> [String]
adjust opts = (fetch opts) . (offset opts) . (longer opts) . (shorter opts)

printWords opts expr =
  case enumerate expr of
    Left e   -> putStrLn e
    Right ws -> mapM_ putStrLn $ adjust opts ws

main = do
  argv <- getArgs
  (opts, expr) <- getOptions argv
  printWords opts expr
