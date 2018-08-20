module Todo.Internal where

import Control.Lens hiding (element)

data State =
    Pending
  | Done
  | Cancelled
  deriving (Eq, Show)

type Task = String

data WrappedTodo = WrappedTodo {
  _fullLine :: String,
  _lineNumber :: Integer,
  _fullPath :: String,
  _todo :: Todo
} deriving (Eq, Show)

data Todo = Todo {
  _state    :: State,
  _task     :: Task
} deriving (Eq, Show)

makeLenses ''Todo
makeLenses ''WrappedTodo
