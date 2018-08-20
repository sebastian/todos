module FileScanner (
    fromPath
  ) where

import           Data.Maybe
import qualified System.Directory as Dir
import qualified System.IO        as SIO
import           Todo


---------------------------------------------------------------------
-- API
---------------------------------------------------------------------

fromPath :: String -> IO [WrappedTodo]
fromPath path = do
  isDir <- Dir.doesDirectoryExist path
  if isDir
    then do
      paths <- recursivelyListAllFiles (pure []) path
      foldr foldDirectories (pure []) paths
    else do
      isFile <- Dir.doesFileExist path
      if isFile
        then fromFile path
        else return []


---------------------------------------------------------------------
-- Internal functions
---------------------------------------------------------------------

fromFile :: String -> IO [WrappedTodo]
fromFile path = do
  content <- readFile path
  return $ mapMaybe (parseTodo path) $ zip [1..] $ lines content

foldDirectories :: SIO.FilePath -> IO [WrappedTodo] -> IO [WrappedTodo]
foldDirectories path acc = do
  todos <- fromFile path
  existingTodos <- acc
  return $ todos ++ existingTodos

parseTodo :: String -> (Integer, String) -> Maybe WrappedTodo
parseTodo path (lnum, line) =
  case parse line of
    Just t ->
      let wt = WrappedTodo {
        _lineNumber = lnum,
        _fullPath = path,
        _fullLine = line,
        _todo = t
      } in
      Just wt
    Nothing -> Nothing

recursivelyListAllFiles :: IO [SIO.FilePath] -> SIO.FilePath -> IO [SIO.FilePath]
recursivelyListAllFiles acc path = do
  paths <- Dir.listDirectory path
  foldr (recurseOnSubDirs path) acc paths

recurseOnSubDirs :: SIO.FilePath -> SIO.FilePath -> IO [SIO.FilePath] -> IO [SIO.FilePath]
recurseOnSubDirs basePath path acc = do
  let completePath = basePath ++ "/" ++ path
  isFile <- Dir.doesFileExist completePath
  if isFile then (:) <$> pure completePath <*> acc
            else do
              isDir <- Dir.doesDirectoryExist completePath
              if isDir then recursivelyListAllFiles acc completePath
                       else acc
