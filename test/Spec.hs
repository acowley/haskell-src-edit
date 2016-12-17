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
        do r <- runIO (addImportToFile f (ModuleName "Data.Maybe") "fromMaybe")
           it "Finds the right line" $
             fmap printResult r `shouldBe` Right "(add-line 5 \"import Data.Maybe (fromMaybe)\")"
      describe "Updates an existing import" $
        do r <- runIO (addImportToFile f (ModuleName "Pipes") "Consumer")
           it "Updates the right import" $
             fmap printResult r `shouldBe` Right "(replace-line 5 \"import Pipes (Consumer, Producer, (>->))\")"
