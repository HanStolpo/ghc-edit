{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Exp  where


import GHC.Generics
import Language.Haskell.Exts.Annotated
-- import Control.Monad
import Control.Applicative
import Control.Arrow
import Text.PrettyPrint.GenericPretty
import Data.List
import Debug.Trace


try :: IO ()
try = do
    putStrLn "begin"
    ParseOk (m, cms) <- parseFileContentsWithComments defaultParseMode{parseFilename = "Blah.hs"} <$> readFile "./test/Data/Blah.hs"
    pp . sort $ map hlComment cms ++ hlModule m
    putStrLn "done"

type LnCol = (Int, Int)
data Highlight = Highlight  { hlStart :: LnCol
                            , hlEnd   :: LnCol
                            , hlType  :: HighlightType
                            , hlCnt   :: String
                            }
                            deriving (Show, Eq, Ord, Generic)
instance Out Highlight

defaultHighlight :: Highlight
defaultHighlight = Highlight (0,0) (0,0) HlOther ""

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

hlSrcSpan :: SrcSpan -> Highlight
hlSrcSpan SrcSpan {..} = defaultHighlight { hlStart =  (srcSpanStartLine, srcSpanStartColumn) 
                                          , hlEnd =  (srcSpanEndLine, srcSpanEndColumn) }

hlSrcSpanInfo :: SrcSpanInfo -> Highlight
hlSrcSpanInfo = hlSrcSpan . srcInfoSpan 

hlComment :: Comment -> Highlight
hlComment (Comment ml sp c) = (hlSrcSpan sp) {hlType = HlComment, hlCnt = if ml then "{-" ++ c ++ "-}" else "--" ++ c}

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
        [mImport', mWhere'] = map ((\x -> x{hlType = HlKeyword}) . hlSrcSpan) . srcInfoPoints $ l
        mImport = mImport'{hlCnt = "import"}
        mWhere = mWhere'{hlCnt = "where"}

hlModuleName :: ModuleName SPI -> Highlight
hlModuleName (ModuleName i n) = (hlSrcSpanInfo i){hlType = HlModuleName, hlCnt = n}

hlWarningText :: Maybe (WarningText SPI) -> [Highlight]
hlWarningText x = case x of
    Nothing                 -> []
    Just (DeprText i s) -> [mk "DEPRECATED" i s]
    Just (WarnText i s) -> [mk "WARNING" i s]
    where
        -- TODO: Fill in correctly
        mk pfx i s = (hlSrcSpanInfo i){hlType = HlPragma, hlCnt = "{-#" ++ pfx ++ "\"" ++ s ++ "\"#-}"}


hlExportSpecList :: Maybe (ExportSpecList SPI) -> [Highlight]
hlExportSpecList x = case x of
    Nothing -> []
    Just (ExportSpecList i es) -> hlBracedListPunc i ++ concatMap hlExportSpec es

hlBracedExpr_ :: ([SrcSpan] -> ([Highlight], [SrcSpan])) -> SPI -> [Highlight]
hlBracedExpr_ inner i = ob : cb : cs
    where
        (ph:ps) = srcInfoPoints $ i
        ob = (hlSrcSpan ph){hlType = HlBrace, hlCnt = "("}
        (cs, [pl]) = inner ps
        cb = (hlSrcSpan pl){hlType = HlBrace, hlCnt =")"}

hlBracedListPunc :: SPI -> [Highlight]
hlBracedListPunc  = hlBracedExpr_ cms
    where cms ps = foldl f ([],ps) ps
            where f (cs', ps') p = case drop 1 ps' of
                                        []   -> (cs', ps')
                                        ps'' -> ((hlSrcSpan p){hlType = HlComma, hlCnt =","} : cs', ps'') 

hlBracedElipse :: SPI -> [Highlight]
hlBracedElipse  = hlBracedExpr_ cms
    where cms (p:ps) = ([(hlSrcSpan p){hlType = HlElipse, hlCnt =".."}], ps) 

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
hlName True     (Ident i s)     = (hlSrcSpanInfo i){hlType = HlIdentType, hlCnt = s}
hlName False    (Ident i s)     = (hlSrcSpanInfo i){hlType = HlIdentFunc, hlCnt = s}
hlName True     (Symbol i s)    = (hlSrcSpanInfo i){hlType = HlSymbolType, hlCnt = s}
hlName False    (Symbol i s)    = (hlSrcSpanInfo i){hlType = HlSymbolFunc, hlCnt = s}

hlSpecialCon :: SpecialCon SPI -> Highlight
hlSpecialCon x = case x of
    UnitCon i           -> trace (("UnitCon" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}
    ListCon i           -> trace (("ListCon" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}
    FunCon i            -> trace (("FunCon" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}
    TupleCon i _ _      -> trace (("TupleCon" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}
    Cons i              -> trace (("Cons" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}
    UnboxedSingleCon i  -> trace (("UnboxedSingleCon" ++) . prty . show $ i) (hlSrcSpanInfo i){hlType = HlSpecialCon}

hlCName :: CName SPI -> Highlight
hlCName x = case x of
    VarName _ n -> hlName False n
    ConName _ n -> hlName True n


hlModulePragma :: ModulePragma SPI -> Highlight
hlModulePragma x = case x of
        LanguagePragma i _  -> (hlSrcSpanInfo i){hlType = HlPragma}
        OptionsPragma i _ _ -> (hlSrcSpanInfo i){hlType = HlPragma}
        AnnModulePragma i _ -> (hlSrcSpanInfo i){hlType = HlPragma}

hlImportDecl :: ImportDecl SPI -> [Highlight]
hlImportDecl ImportDecl {..} =  [hlModuleName importModule] ++ _hlImprt ++ _hlSrc ++ _hlQual ++ _hlPkg ++ _hlAs ++ _hlSpec
    where
        mk m = (:[]) . m . hlSrcSpan . head &&& drop 1
        (_hlImprt, ps)      = mk (\i -> i{hlType = HlImport, hlCnt = "import"}) . srcInfoPoints $ importAnn
        (_hlSrc, ps')       = case importSrc of 
                                True  -> let ([b], _ps)  = mk (\i -> i{hlType = HlPragma}) ps
                                             ([e], _ps') = mk id _ps
                                         in  ([b{hlEnd = hlEnd e}], _ps')
                                False -> ([], ps)
        (_hlQual, ps'')     = case importQualified of
                                True  -> mk (\i -> i{hlType = HlImport, hlCnt = "qualified"}) ps'
                                False -> ([], ps')
        (_hlPkg, ps''')     = case importPkg of
                                Just s  -> mk (\i -> i{hlType = HlImport, hlCnt = s}) ps''
                                Nothing -> ([], ps'')
        _hlAs               = case importAs of
                                Just mn  -> let (cs, _ps) =  mk (\i -> i{hlType = HlImport, hlCnt = "as"}) ps'''
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
                                         ( (\h -> h{hlType = HlImport, hlCnt = "hiding"}) . hlSrcSpan . head 
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
    KindStar i          -> [(hlSrcSpanInfo i){hlType = HlOpType, hlCnt = "*"}]
    KindBang i          -> [(hlSrcSpanInfo i){hlType = HlOpType, hlCnt = "!"}]
    KindFn i k1 k2      -> trace (("KindFn\n" ++ ) . prty . show $ i) $ (hlKind k1 ++ hlKind k2)
    KindParen i k       -> trace (("KindParen\n" ++ ) . prty . show $ i) $ hlKind k 
    KindVar i n         -> [hlName True n]

