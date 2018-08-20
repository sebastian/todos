module Todo.Parser (
    parse
  ) where

import           Control.Applicative
import           Text.Parser.Combinators
import           Text.Trifecta hiding (line)
import qualified Text.Trifecta.Result    as Result
import           Todo.Internal

parseState :: Parser State
parseState = do
  _ <- char '['
  rawState <- manyTill anyChar $ char ']'
  let parsedState = case rawState of
                "x" -> Done
                "." -> Cancelled
                _   -> Pending
  return parsedState

parseText :: Parser String
parseText = many (noneOf "\n")

parseContent :: Parser String
parseContent = do
  _ <- skipMany space
  content <- parseText
  return content

parseTodo :: Parser Todo
parseTodo = do
  _ <- spaces
  _ <- char '-'
  _ <- spaces
  parsedState <- parseState
  content <- parseContent
  return $ Todo parsedState content

parse :: String -> Maybe Todo
parse l =
  case parseString parseTodo mempty l of
                (Result.Success t) -> Just t
                _                  -> Nothing
