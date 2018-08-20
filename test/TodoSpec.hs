module TodoSpec (spec) where

import           Control.Lens
import           Data.Maybe
import qualified FileScanner
import qualified System.Directory as Dir
import qualified System.IO        as SIO
import           Test.Hspec
import           Todo

spec :: Spec
spec = do
  describe "isTodo?" $ do
    it "plain text line is not a todo" $
      Todo.isTodo "hello there" `shouldBe` False
    it "plain todo is a todo" $
      Todo.isTodo "- [ ] todo" `shouldBe` True
    it "indented plain todo is a todo" $
      Todo.isTodo "    - [ ] todo" `shouldBe` True
    it "completed todo is a todo" $
      Todo.isTodo "- [x] todo" `shouldBe` True
    it "cancelled todo is a todo" $
      Todo.isTodo "- [.] todo" `shouldBe` True
    it "todo missing space is also considered a TODO (for good measure)" $
      Todo.isTodo "- [] todo" `shouldBe` True

  describe "parsing state" $ do
    it "parses - [ ] as pending" $
      stateFrom "- [ ] foo" `shouldBe` Pending
    it "parses - [] as pending" $
      stateFrom "- [] foo" `shouldBe` Pending
    it "parses - [x] as done" $
      stateFrom "- [x] foo" `shouldBe` Done
    it "parses - [.] as done" $
      stateFrom "- [.] foo" `shouldBe` Cancelled

  describe "capturing content" $ do
    it "keeps todo list text" $
      taskFrom "- [ ] content" `shouldBe` "content"
    it "can mark todo as done" $
      withPendingTodo markDone
      where markDone path = do
              [t1] <- FileScanner.fromPath path
              view (todo . state) t1 `shouldBe` Pending
              _ <- Todo.setState Done t1
              [t2] <- FileScanner.fromPath path
              view (todo . state) t2 `shouldBe` Done


---------------------------------------------------------------------
-- Helper function
---------------------------------------------------------------------

withPendingTodo :: (SIO.FilePath -> IO a) -> IO a
withPendingTodo = withTodo "- [ ] a todo"

withTodo :: String -> (SIO.FilePath -> IO a) -> IO a
withTodo content callback = do
  let path = "testfiles/todo.md"
  _ <- SIO.writeFile path content
  let result = callback path
  _ <- Dir.removeFile path
  result

stateFrom :: String -> State
stateFrom = _state . fromJust . Todo.parse

taskFrom :: String -> Task
taskFrom = _task . fromJust . Todo.parse
