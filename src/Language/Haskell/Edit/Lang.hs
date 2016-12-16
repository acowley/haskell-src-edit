{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Language.Haskell.Edit.Lang where

import Data.Char (digitToInt)
import Data.SCargot
import Data.SCargot.Repr
import Data.Foldable (foldl')
import Data.Text (Text, pack, unpack)
import Text.Parsec
import Text.Parsec.Text

newtype ModuleName = ModuleName { moduleName :: Text } deriving (Eq, Show)

data Reserved = AddImportTag | SrcSpanTag
  deriving (Eq, Ord, Show)

parseCommandName :: Parser Reserved
parseCommandName = choice [ AddImportTag <$ string "add-import" 
                          , SrcSpanTag <$ string "span" ]

data Atom
  = Reserved Reserved
  | TextAtom Text
  | IntAtom Int
  | KeyWord Text
  deriving (Eq, Ord)
  
instance Show Atom where
  show (Reserved c) = show c
  show (TextAtom t) = '"' : unpack t ++ "\""
  show (IntAtom i) = show i
  show (KeyWord t) = ':' : unpack t

parseAtom :: Parser Atom
parseAtom =
  choice
    [ Reserved <$> parseCommandName
    , TextAtom . pack <$>
      between
        (char '"')
        (char '"')
        (many (noneOf ['"', '\\'] <|> ('\\' <$ string "\\\\")))
    , IntAtom <$> (option id (negate <$ char '-') <*> natural)
    , KeyWord . pack <$> (char ':' *> many (alphaNum <|> oneOf ['-','_','\'']))
    ]
  where
    natural = foldl' (\s d -> s * 10 + digitToInt d) 0 <$> many1 digit

data Exp = AddImport ModuleName Text 
         | SrcSpan Int (Maybe Int) Int (Maybe Int)
  deriving (Eq, Show)
  
pattern WFCommand :: Reserved -> WellFormedSExpr Atom
pattern WFCommand c <- WFSAtom (Reserved c)

pattern WFText :: Text -> WellFormedSExpr Atom
pattern WFText t <- WFSAtom (TextAtom t)

pattern WFInt :: Int -> WellFormedSExpr Atom
pattern WFInt t <- WFSAtom (IntAtom t)

pattern WFKeyWord :: Text -> WellFormedSExpr Atom
pattern WFKeyWord t <- WFSAtom (KeyWord t)

-- | Pick out any of the start line, start column, end line, and end
-- column values given as keyword arguments.
srcSpanKeywordArgs :: [WellFormedSExpr Atom] -> Maybe Exp
srcSpanKeywordArgs = go (Nothing,Nothing,Nothing,Nothing)
  where go (Just sl,sc, Just el,ec) [] = Just (SrcSpan sl sc el ec)
        go (sl, sc, el, ec) xs =
          case xs of
            (WFKeyWord "start-line" : WFInt i : xs') -> go (Just i, sc, el, ec) xs'
            (WFKeyWord "start-col" : WFInt j : xs') -> go (sl,Just j,el,ec) xs'
            (WFKeyWord "end-line" : WFInt k : xs') -> go (sl,sc,Just k,ec) xs'
            (WFKeyWord "end-col" : WFInt l : xs') -> go (sl,sc,el,Just l) xs'
            _ -> Nothing

parseSexp :: WellFormedSExpr Atom -> Either String Exp
parseSexp (WFSList [WFCommand AddImportTag, WFText a1, WFText a2]) =
  Right $ AddImport (ModuleName a1) a2
parseSexp (WFSList [WFCommand SrcSpanTag, WFInt i, WFInt j]) =
  Right $ SrcSpan i Nothing j Nothing          
parseSexp (WFSList [WFCommand SrcSpanTag, WFInt i, WFInt j, WFInt k, WFInt l]) =
  Right $ SrcSpan i (Just j) k (Just l)
parseSexp (WFSList (WFCommand SrcSpanTag : (srcSpanKeywordArgs -> Just s))) = Right s
parseSexp s = Left ("Invalid expression: " ++ unpack (encodeOne sexpPrinter s))

sexpPrinter :: SExprPrinter Atom (WellFormedSExpr Atom)
sexpPrinter = setFromCarrier fromWellFormed (basicPrint (pack . show))

langParser :: SExprParser Atom (WellFormedSExpr Atom)
langParser = asWellFormed (mkParser parseAtom)

test :: Either String (WellFormedSExpr Atom)
test = decodeOne langParser "(add-import \"Pipes\" \"Consumer\")"
