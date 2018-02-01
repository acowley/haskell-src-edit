{-# LANGUAGE LambdaCase #-}
module Main where
import Control.Arrow ((***))
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Language.Haskell.Edit
import Language.Haskell.Edit.Command
import qualified Language.Haskell.Edit.Result as R
import System.Environment

showHelp :: String
showHelp = "Usage: hsedit 'sexp'"

interpret :: Command -> ExceptT String IO String
interpret s@(SrcSpan _ _ _ _) = pure (T.unpack (printCommand s))
interpret (AddImport (FileName f) m thing) = ExceptT $
  fmap (fmap (T.unpack . R.printResult))
       (addImportToFile (T.unpack f) m (T.unpack thing))
interpret (RemoveImport (FileName f) m t) = ExceptT $
  fmap (fmap (T.unpack . R.printResult))
       (removeImportFromFile (T.unpack f)
                             m
                             ((T.unpack *** fmap T.unpack) <$> t))

liftE :: Applicative m => Either e r -> ExceptT e m r
liftE = ExceptT . pure

main :: IO ()
main = getArgs >>= \case
  [e] -> runExceptT (liftE (parseExp (T.pack e)) >>= interpret)
         >>= either error putStrLn
  _ -> putStrLn showHelp
