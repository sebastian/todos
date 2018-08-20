module Main where

import           Control.Lens
import qualified FileScanner
import qualified System.Environment as Env
import           Todo

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    []         -> putStrLn "Please provide a path to scan for todo's"
    (path : _) -> scan path
  where scan path = do
          todos <- FileScanner.fromPath path
          mconcat $ putStrLn <$> map (show . view todo) todos
