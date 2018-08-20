module Todo (
    Todo(..),
    WrappedTodo(..),
    State(..),
    Task,

    -- lenses
    todo,
    state,
    lineNumber,
    fullPath,
    fullLine,

    setState,
    isTodo,
    parse
  ) where

import           Control.Lens
import           Todo.Internal
import qualified Todo.Parser   as Parser


---------------------------------------------------------------------
-- API
---------------------------------------------------------------------

setState :: State -> WrappedTodo -> IO ()
setState ts wt = do
  let targetTodo = craftTodoLine ts (view (todo . task) wt)
      path = view fullPath wt
  fc <- readFile path
  let updatedContent = unlines $ replaceLine (view lineNumber wt) targetTodo $ zip [1..] $ lines fc
  writeFile path updatedContent

parse :: String -> Maybe Todo
parse = Parser.parse

isTodo :: String -> Bool
isTodo l =
  case Parser.parse l of
    Just _  -> True
    Nothing -> False


---------------------------------------------------------------------
-- Internal functoins
---------------------------------------------------------------------

craftTodoLine :: State -> String -> String
craftTodoLine st t = stToMd st ++ t

stToMd :: State -> String
stToMd Done      = "[x]"
stToMd Pending   = "[ ]"
stToMd Cancelled = "[.]"

replaceLine :: Integer -> String -> [(Integer, String)] -> [String]
replaceLine n t = foldr rep []
  where rep (ln, c) acc
            | ln == n = t : acc
            | otherwise = c : acc
