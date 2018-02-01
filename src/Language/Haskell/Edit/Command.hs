{-# LANGUAGE OverloadedStrings, PatternSynonyms, TupleSections, ViewPatterns #-}
module Language.Haskell.Edit.Command where
import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.SCargot
import Data.SCargot.Repr
import Data.Foldable (foldl')
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- * S-expression Components

data Reserved = AddImportTag | RemoveImportTag | SrcSpanTag
  deriving (Eq, Ord, Show)

data Atom
  = Reserved Reserved
  | TextAtom Text
  | IntAtom Int
  | KeyWord Text
  deriving (Eq, Ord, Show)

showT :: Atom -> Text
showT (Reserved c) = reservedWord c
showT (TextAtom t) = T.concat ["\"", t, "\""]
showT (IntAtom i) = T.pack (show i)
showT (KeyWord t) = T.cons ':' t

-- * The Command Expression Language

newtype ModuleName = ModuleName { moduleName :: Text } deriving (Eq, Show)
newtype FileName = FileName { fileName :: Text } deriving (Eq, Show)

data Command = AddImport FileName ModuleName Text
             | RemoveImport FileName ModuleName (Maybe (Text, Maybe Text))
             | SrcSpan Int (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Eq, Show)

-- * Pattern Synonyms

pattern WFCommand :: Reserved -> WellFormedSExpr Atom
pattern WFCommand c = WFSAtom (Reserved c)

pattern WFText :: Text -> WellFormedSExpr Atom
pattern WFText t = WFSAtom (TextAtom t)

pattern WFInt :: Int -> WellFormedSExpr Atom
pattern WFInt t = WFSAtom (IntAtom t)

pattern WFKeyWord :: Text -> WellFormedSExpr Atom
pattern WFKeyWord t = WFSAtom (KeyWord t)

-- * Parsing

parseCommandName :: Parser Reserved
parseCommandName = choice [ AddImportTag <$ string "add-import"
                          , RemoveImportTag <$ string "remove-import"
                          , SrcSpanTag <$ string "span" ]

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

-- * KeyWord Argument Parsers

addImportKW :: FileName -> [WellFormedSExpr Atom] -> Maybe Command
addImportKW f = go Nothing Nothing
    where go (Just m) (Just thing) [] = Just (AddImport f (ModuleName m) thing)
          go m thing xs =
            case xs of
              (WFKeyWord "module" : WFText m' : xs') -> go (Just m') thing xs'
              (WFKeyWord "thing" : WFText t : xs') -> go m (Just t) xs'
              _ -> Nothing

removeImportKW :: FileName -> [WellFormedSExpr Atom] -> Maybe Command
removeImportKW f = go Nothing Nothing Nothing
  where go (Just m) thing thingWith [] =
          Just (RemoveImport f (ModuleName m) (fmap (,thingWith) thing))
        go m thing thingWith xs =
          case xs of
            (WFKeyWord "module" : WFText m' : xs') ->
              go (Just m') thing thingWith xs'
            (WFKeyWord "thing" : WFText t : xs') ->
              go m (Just t) thingWith xs'
            (WFKeyWord "thingWith" : WFText w : xs') ->
              go m thing (Just w) xs'
            _ -> Nothing

-- | Pick out any of the start line, start column, end line, and end
-- column values given as keyword arguments.
srcSpanKeywordArgs :: [WellFormedSExpr Atom] -> Maybe Command
srcSpanKeywordArgs = go (Nothing,Nothing,Nothing,Nothing)
  where go (Just sl,sc, el,ec) [] = Just (SrcSpan sl sc el ec)
        go (sl, sc, el, ec) xs =
          case xs of
            (WFKeyWord "start-line" : WFInt i : xs') -> go (Just i, sc, el, ec) xs'
            (WFKeyWord "start-col" : WFInt j : xs') -> go (sl,Just j,el,ec) xs'
            (WFKeyWord "end-line" : WFInt k : xs') -> go (sl,sc,Just k,ec) xs'
            (WFKeyWord "end-col" : WFInt l : xs') -> go (sl,sc,el,Just l) xs'
            _ -> Nothing

-- | We support several versions of source span specification: just
-- the start line, the start line and the end line, or a version that
-- uses keywords to identify the span components: @:start-line@,
-- @:start-col@, @:end-line@, @:end-col@.
parseSexp :: WellFormedSExpr Atom -> Either String Command
parseSexp (WFSList [WFCommand AddImportTag, WFText f, WFText m, WFText t]) =
  Right $ AddImport (FileName f) (ModuleName m) t
parseSexp (WFSList ( WFCommand AddImportTag
                   : WFText f
                   : (addImportKW (FileName f) -> Just a))) = Right a

parseSexp (WFSList [WFCommand RemoveImportTag, WFText f, WFText m]) =
  Right $ RemoveImport (FileName f) (ModuleName m) Nothing
parseSexp (WFSList [WFCommand RemoveImportTag, WFText f, WFText m, WFText t]) =
  Right $ RemoveImport (FileName f) (ModuleName m) (Just (t, Nothing))
parseSexp (WFSList [WFCommand RemoveImportTag, WFText f, WFText m, WFText t, WFText w]) =
  Right $ RemoveImport (FileName f) (ModuleName m) (Just (t, Just w))
parseSexp (WFSList ( WFCommand RemoveImportTag
                   : WFText f
                   : (removeImportKW (FileName f) -> Just r))) = Right r

parseSexp (WFSList [WFCommand SrcSpanTag, WFInt i]) =
  Right $ SrcSpan i Nothing Nothing Nothing
parseSexp (WFSList [WFCommand SrcSpanTag, WFInt i, WFInt j]) =
  Right $ SrcSpan i Nothing (Just j) Nothing
parseSexp (WFSList [WFCommand SrcSpanTag, WFInt i, WFInt j, WFInt k, WFInt l]) =
  Right $ SrcSpan i (Just j) (Just k) (Just l)
parseSexp (WFSList (WFCommand SrcSpanTag : (srcSpanKeywordArgs -> Just s))) =
  Right s

parseSexp s = Left ("Invalid expression: " ++ unpack (encodeOne sexpPrinter s))

langParser :: SExprParser Atom (WellFormedSExpr Atom)
langParser = asWellFormed (mkParser parseAtom)

-- * Printing

reservedWord :: Reserved -> Text
reservedWord AddImportTag = "add-import"
reservedWord RemoveImportTag = "remove-import"
reservedWord SrcSpanTag = "span"

sexpPrinter :: SExprPrinter Atom (WellFormedSExpr Atom)
sexpPrinter = setFromCarrier fromWellFormed (basicPrint showT)

toSexp :: Command -> WellFormedSExpr Atom
toSexp (AddImport (FileName f) (ModuleName m) thing) =
  WFSList [ WFCommand AddImportTag, WFKeyWord "file", WFText f
          , WFKeyWord "module", WFText m, WFKeyWord "thing", WFText thing ]
toSexp (RemoveImport (FileName f) (ModuleName m) thing) =
  WFSList $ [ WFCommand RemoveImportTag, WFKeyWord "file", WFText f
            , WFKeyWord "module", WFText m ]
            ++ case thing of
                 Nothing -> []
                 (Just (t, Nothing)) -> [WFKeyWord "thing",  WFText t]
                 (Just (t, Just w)) -> [ WFKeyWord "thing", WFText t
                                       , WFKeyWord "thingWith", WFText w ]
toSexp (SrcSpan sl sc el ec) =
  WFSList $ [ WFCommand SrcSpanTag, WFKeyWord "start-line", WFInt sl ]
            ++ maybe [] (\sc' -> [WFKeyWord "start-col", WFInt sc']) sc
            ++ maybe [] (\el' -> [WFKeyWord "end-line", WFInt el']) el
            ++ maybe [] (\ec' -> [WFKeyWord "end-col", WFInt ec']) ec

parseExp :: Text -> Either String Command
parseExp = decodeOne langParser >=> parseSexp

printCommand :: Command -> Text
printCommand = encodeOne sexpPrinter . toSexp
