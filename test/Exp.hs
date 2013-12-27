{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Exp  where


import GHC.Generics
import Language.Haskell.Exts.Annotated
-- import Control.Monad
import Control.Applicative
import Control.Arrow
import Text.PrettyPrint.GenericPretty
import Data.List
import Data.Char
import Debug.Trace


try :: IO ()
try = do
    putStrLn "begin"
    s <-readFile "./test/Data/Blah.hs"
    let ParseOk (m, cms) = parseFileContentsWithComments defaultParseMode{parseFilename = "Blah.hs"}  s
        hl = sort $ map hlComment cms ++ hlModule m
    putStrLn . pr hl $ s
--    putStrLn (prty . show $ hl)
    putStrLn "done"

pr :: [Highlight] -> String -> String
pr hl = drop 1 . reverse . fst . foldl _scan ("1", (hl, 1,0)) 
    where
    _scan (s@(chp:_), st) ch = case ch of
            '\n' -> _ignore (s, if chp == '\r' then st else  _incL st) ch
            '\r' -> _ignore (s, if chp == '\n' then st else  _incL st) ch 
            '\t' -> _ignore (s, head . drop 8 . iterate _incC $ st) ch
            _    -> if isSpace ch then _ignore (s, _incC st) ch else   _proc (s, _discard . _incC $ st) $ ch
    _scan x _ = error $ "_scan mis match " ++ show x


    _incL (hs, l, _) = (hs, l + 1, 0)
    _incC (hs, l, c) = (hs, l, c + 1)
    _discard x@([],_,_) = x
    _discard (h:hs, l, c) | hlEnd h <= (l,c) =  _discard (hs, l, c) 
                          | otherwise = (h:hs, l, c)
    _discard' (s, st) = (s, _discard st)

    _ignore (s, st) ch = (ch:s, st)

    _proc (s, st@([],_,_)) ch = (ch : s, st)
    _proc (s, st@(h:_, l, c)) ch  = (_end (ch : _start s h l c) h l c, st)
    _start s h l c | hlStart h == (l,c) = foldl (flip (:)) s (_hlO . hlType $ h)
                   | otherwise = s
    _end s h l c | hlEnd h == (l,c+1) =  foldl (flip (:)) s (_hlC . hlType $ h)
                 | otherwise = s

    _hlO  = (++"|") . ("<"++) . _hlId
    _hlC  = (++">") . ("|"++) . _hlId
    _hlId hlt = case hlt of
                HlComment       -> "co"
                HlModuleName    -> "mn"
                HlKeyword       -> "kw"
                HlImport        -> "im"
                HlPragma        -> "pr"
                HlBrace         -> "br"
                HlComma         -> "cm"
                HlElipse        -> "el"
                HlIdentType     -> "it"
                HlIdentFunc     -> "if"
                HlSymbolType    -> "st"
                HlSymbolFunc    -> "sf"
                HlSpecialCon    -> "sc"
                HlOpType        -> "ot"
                HlOpFunc        -> "of"
                HlOther         -> "__"


type LnCol = (Int, Int)
data Highlight = Highlight  { hlStart :: LnCol
                            , hlEnd   :: LnCol
                            , hlType  :: HighlightType
                            }
                            deriving (Show, Eq, Ord, Generic)
instance Out Highlight

defaultHighlight :: Highlight
defaultHighlight = Highlight (0,0) (0,0) HlOther

data HighlightType  = HlComment 
                    | HlModuleName
                    | HlKeyword
                    | HlImport
                    | HlPragma
                    | HlBrace
                    | HlComma
                    | HlElipse
                    | HlIdentType
                    | HlIdentFunc
                    | HlSymbolType
                    | HlSymbolFunc
                    | HlSpecialCon
                    | HlOpType
                    | HlOpFunc
                    | HlOther
                    deriving (Show, Eq, Ord, Generic)
instance Out HighlightType


prty :: String -> String
prty = fst . foldl f ("", "")
    where
        f (s, pfx) c 
                    | c `elem` "{([" = let pfx' = pfx ++ "  " in (s ++ "\n" ++ pfx ++ [c], pfx')
                    | c `elem` "})]" = let pfx' = drop 2 pfx  in (s ++ "\n" ++ pfx' ++ [c], pfx')
                    | c `elem` "," =  (s ++ "\n" ++ pfx ++ [c], pfx)
                    | otherwise = (s ++ [c], pfx)

hlSrcSpan :: HighlightType -> SrcSpan -> Highlight
hlSrcSpan t SrcSpan {..} = defaultHighlight { hlStart =  (srcSpanStartLine, srcSpanStartColumn) 
                                            , hlEnd =  (srcSpanEndLine, srcSpanEndColumn) 
                                            , hlType = t
                                            }

hlSrcSpanInfo :: HighlightType -> SrcSpanInfo -> Highlight
hlSrcSpanInfo t = hlSrcSpan t . srcInfoSpan 

hlComment :: Comment -> Highlight
hlComment (Comment ml sp c) = hlSrcSpan HlComment sp

type SPI = SrcSpanInfo

hlModule :: Module SPI -> [Highlight]
hlModule (XmlPage _ _ _ _ _ _ _) = error "not supporting XmlPage"
hlModule (XmlHybrid _ _ _ _ _ _ _ _ _) = error "not supporting XmlHybrid"
hlModule (Module _ mHead mPragmas mImport decls) = hlModuleHead  mHead
                                                ++ map hlModulePragma mPragmas
                                                ++ concatMap hlImportDecl mImport

hlModuleHead :: Maybe (ModuleHead SPI) -> [Highlight]
hlModuleHead Nothing = []
hlModuleHead (Just (ModuleHead l mName mWarning mExpList)) = [mImport, hlModuleName mName, mWhere] 
                                                           ++ hlWarningText mWarning
                                                           ++ hlExportSpecList mExpList
    where
        [mImport, mWhere] = map (hlSrcSpan HlKeyword) . srcInfoPoints $ l

hlModuleName :: ModuleName SPI -> Highlight
hlModuleName (ModuleName i _) = hlSrcSpanInfo HlModuleName i

hlWarningText :: Maybe (WarningText SPI) -> [Highlight]
hlWarningText x = case x of
    Nothing                 -> []
    Just (DeprText i s) -> [hlSrcSpanInfo HlPragma i]
    Just (WarnText i s) -> [hlSrcSpanInfo HlPragma i]


hlExportSpecList :: Maybe (ExportSpecList SPI) -> [Highlight]
hlExportSpecList x = case x of
    Nothing -> []
    Just (ExportSpecList i es) -> hlBracedListPunc i ++ concatMap hlExportSpec es

hlBracedExpr_ :: ([SrcSpan] -> ([Highlight], [SrcSpan])) -> SPI -> [Highlight]
hlBracedExpr_ inner i = ob : cb : cs
    where
        (ph:ps) = srcInfoPoints $ i
        ob = hlSrcSpan HlBrace ph
        (cs, [pl]) = inner ps
        cb = hlSrcSpan HlBrace pl

hlBracedListPunc :: SPI -> [Highlight]
hlBracedListPunc  =  hlBracedExpr_ cms
    where cms ps = foldl f ([],ps) ps
            where f (cs', ps') p = case drop 1 ps' of
                                        []   -> (cs', ps')
                                        ps'' -> (hlSrcSpan HlComma p : cs', ps'') 

hlBracedElipse :: SPI -> [Highlight]
hlBracedElipse  = hlBracedExpr_ cms
    where cms (p:ps) = ([hlSrcSpan HlElipse p], ps) 

hlExportSpec :: ExportSpec SPI -> [Highlight]
hlExportSpec x = case x of
    EVar _ n            -> hlQName False n
    EAbs _ n            -> hlQName True n
    EThingAll i n       -> hlBracedElipse i ++ hlQName True n
    EThingWith i n cs   -> hlBracedListPunc i ++ hlQName True n ++ map hlCName cs
    EModuleContents i n -> trace (("EModuleContents"++) . prty . show $ i) $ [hlModuleName n]

hlQName :: Bool -> QName SPI -> [Highlight]
hlQName typeLevel x = case x of
    Qual _ mn n -> hlModuleName mn : hlName typeLevel n : []
    UnQual _ n  -> [hlName typeLevel n]
    Special _ n  -> [hlSpecialCon n]

hlName :: Bool -> Name SPI -> Highlight
hlName True     (Ident i _)     = hlSrcSpanInfo HlIdentType i
hlName False    (Ident i _)     = hlSrcSpanInfo HlIdentFunc i
hlName True     (Symbol i _)    = hlSrcSpanInfo HlSymbolType i
hlName False    (Symbol i _)    = hlSrcSpanInfo HlSymbolFunc i

hlSpecialCon :: SpecialCon SPI -> Highlight
hlSpecialCon x = case x of
    UnitCon i           -> trace (("UnitCon" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i
    ListCon i           -> trace (("ListCon" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i
    FunCon i            -> trace (("FunCon" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i
    TupleCon i _ _      -> trace (("TupleCon" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i
    Cons i              -> trace (("Cons" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i
    UnboxedSingleCon i  -> trace (("UnboxedSingleCon" ++) . prty . show $ i) hlSrcSpanInfo HlSpecialCon i

hlCName :: CName SPI -> Highlight
hlCName x = case x of
    VarName _ n -> hlName False n
    ConName _ n -> hlName True n


hlModulePragma :: ModulePragma SPI -> Highlight
hlModulePragma x = case x of
        LanguagePragma i _  -> hlSrcSpanInfo HlPragma i
        OptionsPragma i _ _ -> hlSrcSpanInfo HlPragma i
        AnnModulePragma i _ -> hlSrcSpanInfo HlPragma i

hlImportDecl :: ImportDecl SPI -> [Highlight]
hlImportDecl ImportDecl {..} =  [hlModuleName importModule] ++ _hlImprt ++ _hlSrc ++ _hlQual ++ _hlPkg ++ _hlAs ++ _hlSpec
    where
        mk t = (:[]) . hlSrcSpan t . head &&& drop 1
        (_hlImprt, ps)      = mk HlImport . srcInfoPoints $ importAnn
        (_hlSrc, ps')       = case importSrc of 
                                True  -> let ([b], _ps)  = mk HlPragma ps
                                             ([e], _ps') = mk HlOther _ps
                                         in  ([b{hlEnd = hlEnd e}], _ps')
                                False -> ([], ps)
        (_hlQual, ps'')     = case importQualified of
                                True  -> mk HlImport ps'
                                False -> ([], ps')
        (_hlPkg, ps''')     = case importPkg of
                                Just s  -> mk HlImport ps''
                                Nothing -> ([], ps'')
        _hlAs               = case importAs of
                                Just mn  -> let (cs, _ps) =  mk HlImport ps'''
                                            in hlModuleName mn : cs
                                Nothing -> []
        _hlSpec             = case importSpecs of
                                Nothing -> []
                                Just (ImportSpecList i hid imps) -> _hlSpecPunc i hid ++ concatMap _hlImpSpec imps
        _hlImpSpec x        = case x of
                                IVar _ n            -> [hlName False n]
                                IAbs _ n            -> [hlName True n]
                                IThingAll i n       -> hlName True n : hlBracedElipse i 
                                IThingWith i n cns  -> hlName True n : hlBracedListPunc i ++ map hlCName cns
        _hlSpecPunc i hid  = case hid of
                                False -> hlBracedListPunc i
                                True  -> uncurry (:) .
                                         ( hlSrcSpan HlImport . head 
                                         &&& hlBracedListPunc . (\p->i{srcInfoPoints =  p}) . drop 1
                                         ) . srcInfoPoints $ i


hlDecl :: Decl SPI -> [Highlight]
hlDecl x = case x of
    TypeDecl i hd tp                 -> undefined
    TypeFamDecl i hd knd             -> undefined
    DataDecl i dn ctx hd qs dr       -> undefined
    GDataDecl i dn ctx hd knd gds dr -> undefined
    DataFamDecl i ctx hd knd         -> undefined
    TypeInsDecl i tp1 tp2            -> undefined
    DataInsDecl i dn tp qs dr        -> undefined
    GDataInsDecl i dn tp knd gds dr  -> undefined
    ClassDecl i ctx hd fds cds       -> undefined
    InstDecl i ctx ihd  ids          -> undefined
    DerivDecl i ctx ihd              -> undefined
    InfixDecl i ass l ops            -> undefined
    DefaultDecl i tp                 -> undefined
    SpliceDecl i exp                 -> undefined
    TypeSig i ns tp                  -> undefined
    FunBind i ms                     -> undefined
    PatBind i p mtp rhs bnds         -> undefined
    ForImp i cv sfty s nm tp         -> undefined
    ForExp i cv s nm tp              -> undefined
    RulePragmaDecl i r               -> undefined
    DeprPragmaDecl i ds              -> undefined
    WarnPragmaDecl i ds              -> undefined
    InlineSig i b act qnm            -> undefined
    InlineConlikeSig i act qnm       -> undefined
    SpecSig i act qnm tp             -> undefined
    SpecInlineSig i b act qnm tp     -> undefined
    InstSig i ctx ihd                -> undefined
    AnnPragma i ann                  -> undefined

hlDeclHead :: DeclHead SPI -> [Highlight]
hlDeclHead x = case x of
    DHead i n tvs       -> hlName True n : concatMap hlTyVarBind tvs
    DHInfix i tvl n tvr -> hlTyVarBind tvl ++ [hlName True n] ++ hlTyVarBind tvr
    DHParen i dh        -> hlDeclHead dh

hlTyVarBind :: TyVarBind SPI -> [Highlight]
hlTyVarBind x = case x of
    KindedVar i nm kd   -> [hlName True nm] ++ hlKind kd 
    UnkindedVar i nm    -> [hlName True nm]

hlKind :: Kind SPI -> [Highlight]
hlKind x = case x of
    KindStar i          -> [hlSrcSpanInfo HlOpType i]
    KindBang i          -> [hlSrcSpanInfo HlOpType i]
    KindFn i k1 k2      -> trace (("KindFn\n" ++ ) . prty . show $ i) $ (hlKind k1 ++ hlKind k2)
    KindParen i k       -> trace (("KindParen\n" ++ ) . prty . show $ i) $ hlKind k 
    KindVar i n         -> [hlName True n]

