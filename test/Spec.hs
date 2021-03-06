{-# LANGUAGE OverloadedStrings #-}
import Data.Text ()
import Language.Haskell.Edit
import Language.Haskell.Edit.Result
import System.IO (hClose)
import System.IO.Temp
import Test.Hspec

testFile :: String
testFile = unlines [
    "-- | A test module"
  , "module Test (myfun, yourfun) where"
  , "-- Here are my imports"
  , "import Data.Foldable"
  , "import Pipes (Producer, (>->))"
  , "import Data.Map (Map)"
  , "import System.Exit"
  , "  ( ExitCode(ExitFailure, ExitSuccess)"
  , "  )"
  , ""
  , "myfun :: Int -> Int"
  , "myfun = (+42)"
  , ""
  , "yourfun :: Bool -> Bool"
  , "yourfun = not"
  ]

main :: IO ()
main =
  withSystemTempFile "hsedit-spec" $ \f h -> do
    hClose h
    writeFile f testFile
    hspec $ do
      describe "Add new import" $
        do r1 <- runIO (addImportToFile f (ModuleName "Data.Maybe") "fromMaybe")
           it "Can add a new import line" $
             fmap printResult r1 `shouldBe`
             Right "(add-line 5 \"import Data.Maybe (fromMaybe)\")"
           r2 <- runIO (addImportToFile f (ModuleName "Pipes") "Consumer")
           it "Updates the right import" $
             fmap printResult r2 `shouldBe`
             Right "(replace-lines 5 5 \"import Pipes (Consumer, Producer, (>->))\")"
           r3 <- runIO (addImportToFile f (ModuleName "Data.Map") "insertWith")
           it "Can cope with unsorted imports" $
             fmap printResult r3 `shouldBe`
             Right "(replace-lines 6 6 \"import Data.Map (Map, insertWith)\")"
      describe "Remove an import" $
        do r1 <- runIO (removeImportFromFile f (ModuleName "Data.Foldable") Nothing)
           it "Removes the right import line" $
             fmap printResult r1 `shouldBe` Right "(remove-lines 4 4)"
           r2 <- runIO (removeImportFromFile
                         f (ModuleName "Pipes") (Just ("Producer", Nothing)))
           it "Can remove an imported type" $
             fmap printResult r2 `shouldBe`
             Right "(replace-lines 5 5 \"import Pipes ((>->))\")"
           r3 <- runIO (removeImportFromFile
                        f (ModuleName "Pipes") (Just (">->", Nothing)))
           it "Can remove an imported operator" $
             fmap printResult r3 `shouldBe`
             Right "(replace-lines 5 5 \"import Pipes (Producer)\")"
           r4 <- runIO (removeImportFromFile
                         f (ModuleName "Data.Map") (Just ("Map", Nothing)))
           it "Removes vestigial imports" $
             fmap printResult r4 `shouldBe`
             Right "(replace-lines 6 6 \"import Data.Map ()\")"
           r5 <- runIO (removeImportFromFile
                         f (ModuleName "System.Exit")
                         (Just ("ExitCode", Just "ExitFailure")))
           it "Removes specific constructor imports" $
             fmap printResult r5 `shouldBe`
             Right "(replace-lines 7 9 \"import System.Exit (ExitCode(ExitSuccess))\")"
