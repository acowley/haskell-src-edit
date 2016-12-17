{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Edit (addImportToFile, C.ModuleName(..)) where
import Prelude hiding (mod)
import Control.Monad ((>=>))
import Data.Char (isUpper)
import Data.Foldable (foldl')
import Data.List (nub, sortOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Haskell.Edit.Lang as C
import Language.Haskell.Edit.Result (Result(..))
import Language.Haskell.Exts

testMod :: IO (ParseResult (Module SrcSpanInfo, [Comment]))
testMod = parseFileWithComments
            defaultParseMode
            "/Users/acowley/Documents/Projects/VinylRecords/Frames/demo/MissingData.hs"

getEndLine :: SrcSpanInfo -> Int
getEndLine (SrcSpanInfo (SrcSpan _ _ _ ln _) _) = ln

getStartLine :: SrcSpanInfo -> Int
getStartLine (SrcSpanInfo (SrcSpan _ ln _ _ _) _) = ln

-- | Return the ending line of a pragma
pragmaLine :: ModulePragma SrcSpanInfo -> Int
pragmaLine = getEndLine . ann

-- | Return the line number after any pragmas and module header lines.
lineBeforeImports' :: Module SrcSpanInfo -> Int
lineBeforeImports' (Module _ (Just mh) _ _ _) = getEndLine (ann mh) + 1
lineBeforeImports' (Module _ Nothing [] _ _) = 1
lineBeforeImports' (Module _ Nothing pragmas _ _) =
  maximum (map pragmaLine pragmas) + 1
lineBeforeImports' _ = error "lineBeforeImports': only Modules are handled"

-- | Return the stat and end line numbers of a 'Comment'
commentLines :: Comment -> (Int,Int)
commentLines (Comment _ (SrcSpan _ start _ end _) _) = (start,end)

-- | Find the line number after any pragmas, any module header, and
-- any module-level comment lines.
lineBeforeImports :: (Module SrcSpanInfo, [Comment]) -> Int
lineBeforeImports (m,cs) = go cs
  where ln = lineBeforeImports' m
        go [] = ln
        go (Comment _ (SrcSpan _ start _ end _) _ : cs')
          | ln >= start && ln <= end = afterComments (end+1) cs'
          | ln >= end = ln
          | otherwise = go cs'
        afterComments ln' [] = ln'
        afterComments ln' (Comment _ (SrcSpan _ start _ end _) _ : cs')
          | ln' < start = ln'
          | otherwise = afterComments (end+1) cs'

findLineForImport :: (Module SrcSpanInfo, [Comment])
                  -> String
                  -> (Int, Maybe (ImportDecl ()))
findLineForImport mod@((Module _ _ _ imp _), _) m = go imp
  where go [] = (lineBeforeImports mod, Nothing)
        go (d@(ImportDecl ss (ModuleName _ m') _ _ _ _ _ _) : imps)
          | m == m' = (getStartLine ss, Just (() <$ d))
          | m < m' = (getStartLine ss, Nothing)
          | otherwise = go imps
findLineForImport _ _ = error "findLineForImport: only Modules are handled"

onImport :: Module l -> String -> (Maybe (ImportDecl l) -> r) -> r
onImport (Module _ _ _ imp _) m f = go imp
  where go [] = f Nothing
        go (decl@(ImportDecl _ (ModuleName _ m') _ _ _ _ _ _) : imps)
          | m == m' = f (Just decl)
          | otherwise = go imps
onImport _ _ _ = error "onImport: only Modules are handled"

data NestedImport = ImportAll | ImportSome [CName ()]

importSpecName :: ImportSpec l -> Name l
importSpecName (IVar _ n) = n
importSpecName (IAbs _ _ n) = n
importSpecName (IThingAll _ n) = n
importSpecName (IThingWith _ n _ ) = n

cnameName :: CName l -> Name l
cnameName (VarName _ n) = n
cnameName (ConName _ n) = n

-- | Compact multiple 'IThingWith' 'ImportSpec's for the same data
-- type or class. For example, if the 'ImportSpec' list is @(Foo(A),
-- Foo(B))@, this function will compress that to @(Foo(A,B))@.
compactImport :: [ImportSpec ()] -> [ImportSpec ()]
compactImport is = sortOn importSpecName (foldr compact [] (dedup mempty is))
  where nestedImports = foldl' collect mempty is
        compact i@(IThingAll _ _) xs = i : xs
        compact (IThingWith _ n _) xs =
          maybe xs ((:xs) . aux n) (M.lookup n nestedImports)
        compact i xs = i : xs
        aux n ImportAll = IThingAll () n
        aux n (ImportSome cs) = IThingWith () n (sortOn cnameName $ nub cs)
        collect m (IThingAll _ n) = M.insert n ImportAll m
        collect m (IThingWith _ n cs') =
          case M.lookup n m of
            Nothing -> M.insert n (ImportSome cs') m
            Just ImportAll -> m
            Just (ImportSome cs) ->
              M.insert n (ImportSome (cs++cs')) m
        collect m _ = m
        dedup _ [] = []
        dedup s (i@(IThingAll _ n) : is')
          | S.member n s = dedup s is'
          | otherwise = i : dedup (S.insert n s) is'
        dedup s (i@ (IThingWith _ n _) : is')
          | S.member n s = dedup s is'
          | otherwise = i : dedup (S.insert n s) is'
        dedup s (i:is') = i : dedup s is'

-- | Try to take apart a name like @Foo(Bar)@ into @(Foo,Bar)@.
decomposeNested :: String -> Maybe (Name (), Maybe (CName ()))
decomposeNested s = case break (== '(') s of
                      (_,[]) -> Nothing
                      (t,c) -> Just (Ident () t, cn (init (tail c)))
  where cn n@(c:_)
          | isUpper c = Just (ConName () (Ident () n))
          | otherwise = Just (VarName () (Ident () n))
        cn [] = Nothing

addToImport :: ImportSpec () -> ImportSpecList () -> ImportSpecList ()
addToImport spec (ImportSpecList _ b ss) =
  ImportSpecList () b (compactImport $ spec:ss)

-- | @addImport module thing@ returns an @import@ for @thing@ from
-- @module@ and the line number at which it should be inserted. Note
-- that importing a data constructor requires that @thing@ be
-- @Type(Constuctor)@.
addImport :: C.ModuleName
          -> String
          -> (Module SrcSpanInfo, [Comment])
          -> Either String Result
addImport (C.ModuleName (T.unpack -> m)) thing parsed =
  case impThing of
    Left e -> Left e
    Right th ->  case imp of
      Nothing -> Right $ AddLine ln (T.pack (prettyPrint (impStmt th)))
      Just existingImport ->
        let specs = maybe (ImportSpecList () False [th])
                          (addToImport th)
                          (importSpecs existingImport)
            import' = existingImport {importSpecs = Just specs}
        in Right $ ReplaceLine ln (T.pack (prettyPrint import'))
  where (ln, imp) = findLineForImport parsed m
        impStmt th = ImportDecl () (ModuleName () m) False False False Nothing Nothing (Just (ImportSpecList () False [th]))
        impThing =
          case thing of
            [] -> Left "Illegal identifier for import"
            (c:_) -> Right $
              if isUpper c
              then case decomposeNested thing of
                     Nothing -> IAbs () (NoNamespace ()) (Ident () thing)
                     Just (ty,con) -> IThingWith () ty (maybeToList con)
              else IVar () (Ident () thing)

addImportToFile :: FilePath
                -> C.ModuleName
                -> String
                -> IO (Either String Result)
addImportToFile f m t = (parseResult >=> addImport m t)
                        <$> parseFileWithComments defaultParseMode f
  where parseResult (ParseOk x) = Right x
        parseResult (ParseFailed sloc msg) =
          Left $ "Haskell parse failed at "++show sloc++": "++msg
