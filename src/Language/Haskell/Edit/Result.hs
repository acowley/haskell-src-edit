{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Language.Haskell.Edit.Result where
import Data.SCargot
import Data.SCargot.Repr
import Data.Text (Text)
import qualified Data.Text as T

data Tag = ReplaceLinesTag | AddLineTag | RemoveLinesTag
  deriving (Eq,Ord,Show)

data Result = ReplaceLines (Int,Int) Text
            | AddLine Int Text
            | RemoveLines (Int,Int)

data Atom
  = Reserved Tag
  | TextAtom Text
  | IntAtom Int
  | KeyWord Text
  deriving (Eq, Ord, Show)

showT :: Atom -> Text
showT (Reserved t) = reservedWord t
showT (TextAtom t) = T.concat ["\"", t, "\""]
showT (IntAtom i) = T.pack (show i)
showT (KeyWord t) = T.cons ':' t

-- * Pattern Synonyms

pattern WFResult :: Tag -> WellFormedSExpr Atom
pattern WFResult c = WFSAtom (Reserved c)

pattern WFText :: Text -> WellFormedSExpr Atom
pattern WFText t = WFSAtom (TextAtom t)

pattern WFInt :: Int -> WellFormedSExpr Atom
pattern WFInt t = WFSAtom (IntAtom t)

pattern WFKeyWord :: Text -> WellFormedSExpr Atom
pattern WFKeyWord t = WFSAtom (KeyWord t)

-- * Printing

reservedWord :: Tag -> Text
reservedWord ReplaceLinesTag = "replace-lines"
reservedWord AddLineTag = "add-line"
reservedWord RemoveLinesTag = "remove-lines"

sexpPrinter :: SExprPrinter Atom (WellFormedSExpr Atom)
sexpPrinter = setFromCarrier fromWellFormed (basicPrint showT)

toSexp :: Result -> WellFormedSExpr Atom
toSexp (ReplaceLines (n, m) t) =
  WFSList [WFResult ReplaceLinesTag, WFInt n, WFInt m, WFText t]
toSexp (AddLine n t) =
  WFSList [WFResult AddLineTag, WFInt n, WFText t]
toSexp (RemoveLines (n, m)) =
  WFSList [WFResult RemoveLinesTag, WFInt n, WFInt m]

printResult :: Result -> Text
printResult = encodeOne sexpPrinter . toSexp
