module FileScannerSpec (spec) where

import           Control.Lens
import qualified FileScanner
import           Test.Hspec
import           Todo

spec :: Spec
spec =
  describe "fromPath" $ do
    it "list all todo's across all files" $ do
      todos <- FileScanner.fromPath "samples/"
      length todos `shouldBe` 4
    it "works given a file, providing the todo's of that file" $ do
      todos <- FileScanner.fromPath "samples/simple.md"
      length todos `shouldBe` 1
    it "lists all todo's from a file" $ do
      todos <- FileScanner.fromPath "samples/simple.md"
      todos `shouldBe` todos
    it "records line number" $ do
      [t] <- FileScanner.fromPath "samples/simple.md"
      view lineNumber t `shouldBe` 2
    it "records file path" $ do
      [t] <- FileScanner.fromPath "samples/simple.md"
      view fullPath t `shouldBe` "samples/simple.md"
    it "records full todo-list line" $ do
      [t] <- FileScanner.fromPath "samples/simple.md"
      view fullLine t `shouldBe` "- [ ] a todo"
    it "returns empty list for file without todo's" $ do
      todos <- FileScanner.fromPath "samples/empty.md"
      todos `shouldBe` []
    it "returns empty list for path that doesn't exist" $ do
      todos <- FileScanner.fromPath "bogus"
      todos `shouldBe` []
