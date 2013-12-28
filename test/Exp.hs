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
import Data.Maybe
import Control.Conditional
import Safe
import Debug.Trace


try :: IO ()
try = do
    putStrLn "begin"
    s <-readFile "./test/Data/Blah.hs"
    let (hl, e) = case parseFileContentsWithComments defaultParseMode{parseFilename = "Blah.hs"}  s of
             ParseOk (m, cms) -> (sort $ map hlComment cms ++ hlModule m, [])
             err -> ([], prty . show $ err)
    putStrLn e
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

tracePrtyMsg :: Show a => String -> a -> b -> b
tracePrtyMsg s a = trace ((s++) . prty . show $ a) 

hlSrcSpan :: HighlightType -> SrcSpan -> Highlight
hlSrcSpan t SrcSpan {..} = defaultHighlight { hlStart =  (srcSpanStartLine, srcSpanStartColumn) 
                                            , hlEnd =  (srcSpanEndLine, srcSpanEndColumn) 
                                            , hlType = t
                                            }

hlSrcSpanInfo :: HighlightType -> SrcSpanInfo -> Highlight
hlSrcSpanInfo t = hlSrcSpan t . srcInfoSpan 

hlComment :: Comment -> Highlight
hlComment (Comment _ sp _) = hlSrcSpan HlComment sp

type SPI = SrcSpanInfo

hlModule :: Module SPI -> [Highlight]
hlModule (XmlPage _ _ _ _ _ _ _) = error "not supporting XmlPage"
hlModule (XmlHybrid _ _ _ _ _ _ _ _ _) = error "not supporting XmlHybrid"
hlModule (Module _ mHead mPragmas mImport decls) = hlModuleHead  mHead
                                                ++ map hlModulePragma mPragmas
                                                ++ concatMap hlImportDecl mImport
                                                ++ concatMap hlDecl decls

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

hlBracedExpr_ :: ([SrcSpan] -> ([Highlight], [SrcSpan])) -> [SrcSpan] -> [Highlight]
hlBracedExpr_ inner (ph:ps) = ob : cb : cs
    where
        ob = hlSrcSpan HlBrace ph
        (cs, pl:_) = inner ps
        cb = hlSrcSpan HlBrace pl

hlBracedListPunc :: SPI -> [Highlight]
hlBracedListPunc  =  hlBracedListPunc'  . srcInfoPoints

hlBracedListPunc' :: [SrcSpan] -> [Highlight]
hlBracedListPunc' = hlBracedExpr_ cms
    where cms ps = foldl f ([],ps) ps
            where f (cs', ps') p = case drop 1 ps' of
                                        []   -> (cs', ps')
                                        ps'' -> (hlSrcSpan HlComma p : cs', ps'') 

hlBracedElipse :: SPI -> [Highlight]
hlBracedElipse  = hlBracedExpr_ cms . srcInfoPoints
    where cms (p:ps) = ([hlSrcSpan HlElipse p], ps) 

hlExportSpec :: ExportSpec SPI -> [Highlight]
hlExportSpec x = case x of
    EVar _ n            -> hlQName False n
    EAbs _ n            -> hlQName True n
    EThingAll i n       -> hlBracedElipse i ++ hlQName True n
    EThingWith i n cs   -> hlBracedListPunc i ++ hlQName True n ++ map hlCName cs
    EModuleContents i n -> tracePrtyMsg "EModuleContents" i $ [hlModuleName n]

hlQName :: Bool -> QName SPI -> [Highlight]
hlQName typeLevel x = case x of
    Qual _ mn n -> _correct mn (hlModuleName mn) (hlName typeLevel n)
    UnQual _ n  -> [hlName typeLevel n]
    Special _ n  -> [hlSpecialCon n]
    where 
        _correct (ModuleName _ s) m n = m {hlEnd = (fst . hlEnd $ m, (snd . hlStart $ m) + length s + 1)} 
                                      : n {hlStart = (fst . hlStart $ n, (snd . hlStart $ n) + length s + 1)} 
                                      : []

hlName :: Bool -> Name SPI -> Highlight
hlName True     (Ident i _)     = hlSrcSpanInfo HlIdentType i
hlName False    (Ident i _)     = hlSrcSpanInfo HlIdentFunc i
hlName True     (Symbol i _)    = hlSrcSpanInfo HlSymbolType i
hlName False    (Symbol i _)    = hlSrcSpanInfo HlSymbolFunc i

hlSpecialCon :: SpecialCon SPI -> Highlight
hlSpecialCon x = case x of
    UnitCon i           -> tracePrtyMsg "UnitCon" i hlSrcSpanInfo HlSpecialCon i
    ListCon i           -> tracePrtyMsg "ListCon" i hlSrcSpanInfo HlSpecialCon i
    FunCon i            -> tracePrtyMsg "FunCon" i hlSrcSpanInfo HlSpecialCon i
    TupleCon i _ _      -> tracePrtyMsg "TupleCon" i hlSrcSpanInfo HlSpecialCon i
    Cons i              -> tracePrtyMsg "Cons" i hlSrcSpanInfo HlSpecialCon i
    UnboxedSingleCon i  -> tracePrtyMsg "UnboxedSingleCon" i hlSrcSpanInfo HlSpecialCon i

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
    TypeDecl i hd tp                 -> let hl = hlSrcSpan HlKeyword
                                            sps = srcInfoPoints i 
                                            in (hl . head) sps : (hl . last) sps : hlDeclHead hd ++ hlTypE tp
    TypeFamDecl i hd knd             -> (map (hlSrcSpan HlKeyword) . srcInfoPoints $ i) 
                                     ++ hlDeclHead hd 
                                     ++ maybe [] hlKind knd 
    DataDecl i dn ctx hd qs dr       -> hlDataOrNew dn
                                        : (map (hlSrcSpan HlKeyword) . srcInfoPoints $ i) 
                                        ++ hlContext ctx
                                        ++ hlDeclHead hd
                                        ++ concatMap hlQualConDecl qs
                                        ++ hlDeriving dr
    GDataDecl i dn ctx hd knd gds dr -> hlDataOrNew dn
                                        : (map (hlSrcSpan HlKeyword) . srcInfoPoints $ i) 
                                        ++ hlContext ctx
                                        ++ hlDeclHead hd
                                        ++ maybe [] hlKind knd 
                                        ++ concatMap hlGadtDecl gds 
                                        ++ hlDeriving dr
    DataFamDecl i ctx hd knd         -> tracePrtyMsg "DataFamDecl" i hlContext ctx ++ hlDeclHead hd ++ maybe [] hlKind knd
    TypeInsDecl i tp1 tp2            -> tracePrtyMsg "TypeInstDecl" i hlTypE tp1 ++ hlTypE tp2
    DataInsDecl i dn tp qs dr        -> []
    GDataInsDecl i dn tp knd gds dr  -> []
    ClassDecl i ctx hd fds cds       -> []
    InstDecl i ctx ihd  ids          -> []
    DerivDecl i ctx ihd              -> []
    InfixDecl i ass l ops            -> []
    DefaultDecl i tp                 -> []
    SpliceDecl i exp                 -> []
    TypeSig i ns tp                  -> []
    FunBind i ms                     -> []
    PatBind i p mtp rhs bnds         -> []
    ForImp i cv sfty s nm tp         -> []
    ForExp i cv s nm tp              -> []
    RulePragmaDecl i r               -> []
    DeprPragmaDecl i ds              -> []
    WarnPragmaDecl i ds              -> []
    InlineSig i b act qnm            -> []
    InlineConlikeSig i act qnm       -> []
    SpecSig i act qnm tp             -> []
    SpecInlineSig i b act qnm tp     -> []
    InstSig i ctx ihd                -> []
    AnnPragma i ann                  -> []

hlDeclHead :: DeclHead SPI -> [Highlight]
hlDeclHead x = case x of
    DHead i n tvs       -> hlName True n : concatMap hlTyVarBind tvs
    DHInfix i tvl n tvr -> hlTyVarBind tvl ++ [hlName True n] ++ hlTyVarBind tvr
    DHParen i dh        -> hlDeclHead dh

hlTyVarBind :: TyVarBind SPI -> [Highlight]
hlTyVarBind x = case x of
    KindedVar i nm kd   -> zipWith ($) (zipWith ($) (repeat hlSrcSpan) [HlBrace, HlKeyword, HlBrace]) (srcInfoPoints i) ++ [hlName True nm] ++ hlKind kd 
    UnkindedVar _ nm    -> [hlName True nm]


hlKind :: Kind SPI -> [Highlight]
hlKind x = case x of
    KindStar i          -> [hlSrcSpanInfo HlOpType i]
    KindBang i          -> [hlSrcSpanInfo HlOpType i]
    KindFn i k1 k2      -> (hlSrcSpan HlKeyword . head . srcInfoPoints $ i) : (hlKind k1 ++ hlKind k2)
    KindParen i k       -> hlBracedListPunc i ++  hlKind k 
    KindVar i n         -> [hlName True n]

hlTypE :: Type SPI -> [Highlight]
hlTypE x = case x of
    TyForall i tvb ctx tp -> (map (hlSrcSpan HlKeyword) . srcInfoPoints $ i)  
                          ++ maybe [] (concatMap hlTyVarBind) tvb 
                          ++ hlContext ctx 
                          ++ hlTypE tp
    TyFun i tp1 tp2       -> (hlSrcSpan HlKeyword . head . srcInfoPoints $ i) 
                          :  hlTypE tp1 
                          ++ hlTypE tp2
    TyTuple i _ tps       -> hlBracedListPunc i ++ concatMap hlTypE tps
    TyList i tp           -> hlBracedListPunc i ++ (hlTypE tp)
    TyApp _ tp1 tp2       -> hlTypE tp1 ++ hlTypE tp2
    TyVar _ nm            -> [hlName True nm]
    TyCon _ qn            -> hlQName True qn
    TyParen i tp          -> hlBracedListPunc i ++ (hlTypE tp)
    TyInfix i tp1 qn tp2  -> trace (("TyInfix - "++) . prty . show $ i) (hlTypE tp1 ++ hlQName True qn ++ hlTypE tp2)
    TyKind i tp kd        -> trace (("TyKind - "++) . prty . show $ i) (hlTypE tp ++ hlKind kd)


hlContext :: Maybe (Context SPI) -> [Highlight]
hlContext x = case x of
    Just (CxSingle i ass) -> _punc i ++ hlAsst ass
    Just (CxTuple i ass)  -> _punc i ++ concatMap hlAsst ass
    Just (CxParen i ctx)  -> _punc i ++ hlContext (Just ctx)
    Just (CxEmpty i)      -> trace (("CxEmpty - " ++ ) . prty . show $ i) []
    _                     -> []
    where _punc  = uncurry (:) . (hlSrcSpan HlKeyword . last &&& select null (const []) hlBracedListPunc' . init) . srcInfoPoints 
   
hlAsst :: Asst SPI -> [Highlight]
hlAsst x = case x of
    ClassA i qn  tps -> hlQName True qn ++ concatMap hlTypE tps
    InfixA i tp1 qn tp2 -> hlTypE tp1 ++ hlQName True qn ++ hlTypE tp2
    IParam i ipn tp -> hlIPName ipn : hlTypE tp
    EqualP i tp1 tp2 -> hlTypE tp1 ++ hlTypE tp2

hlIPName :: IPName SPI -> Highlight
hlIPName x = case x of
    IPDup i s -> trace (("IPDup - " ++ ) . prty . show $ i) $ hlSrcSpanInfo HlIdentType i
    IPLin i s -> trace (("IPLin - " ++ ) . prty . show $ i) $ hlSrcSpanInfo HlIdentType i

hlDataOrNew :: DataOrNew SPI -> Highlight
hlDataOrNew x = case x of
    DataType i -> hlSrcSpanInfo HlKeyword i
    NewType i -> hlSrcSpanInfo HlKeyword i

hlQualConDecl :: QualConDecl SPI -> [Highlight]
hlQualConDecl (QualConDecl i tvb ctx cdecl) = -- tracePrtyMsg "hlQualConDecl" i 
                                            maybe [] (concatMap hlTyVarBind) tvb 
                                            ++ hlContext ctx 
                                            ++ hlConDecl cdecl
                                            ++ if isJust tvb  then map (hlSrcSpan HlKeyword) . srcInfoPoints $ i else []
                                            -- ++ (select null (const []) hlBracedListPunc' . srcInfoPoints $ i)

hlDeriving :: Maybe (Deriving SPI) -> [Highlight]
hlDeriving x = case x of
    Just (Deriving i ihs) -> (uncurry (:) . (hlSrcSpan HlKeyword . head &&& select null (const []) hlBracedListPunc' . drop 1) . srcInfoPoints $ i)
                          ++ concatMap hlInstanceHead ihs
    _ -> []

hlInstanceHead :: InstHead SPI -> [Highlight]
hlInstanceHead x = case x of
    IHead i qn tps          -> {-tracePrtyMsg "IHead"   i -}hlQName True qn ++ concatMap hlTypE tps
    IHInfix i tp1 qn tp2    -> {-tracePrtyMsg "IHInfix" i -}hlTypE tp1 ++ hlQName True qn ++ hlTypE tp2
    IHParen i ih            -> {-tracePrtyMsg "IHParen" i -}hlBracedListPunc i ++ hlInstanceHead ih

hlConDecl :: ConDecl SPI -> [Highlight]
hlConDecl x = case x of 
    ConDecl i nm bgts           -> -- tracePrtyMsg "ConDecl" i 
                                   hlName True nm 
                                   : concatMap hlBangType bgts
    InfixConDecl i bgt1 nm bgt2 -> -- tracePrtyMsg "InfixConDecl" i
                                   hlName True nm 
                                   : hlBangType bgt1
                                   ++ hlBangType bgt2
    RecDecl i nm flds           -> -- tracePrtyMsg "RecDecl" i 
                                   hlName True nm 
                                   : hlBracedListPunc i 
                                   ++ concatMap hlFieldDecl flds

hlFieldDecl :: FieldDecl SPI -> [Highlight]
hlFieldDecl (FieldDecl i nms bgt) = -- tracePrtyMsg "FieldDecl" i 
                                    (hlSrcSpan HlKeyword . last . srcInfoPoints $ i)
                                    : (map (hlSrcSpan HlComma) . init . srcInfoPoints $ i)
                                    ++ map (hlName True) nms ++ hlBangType bgt

hlBangType :: BangType SPI -> [Highlight]
hlBangType x = case x of
    BangedTy i tp   -> (hlSrcSpan HlKeyword . head . srcInfoPoints $ i) : hlTypE tp
    UnBangedTy _ tp -> hlTypE tp
    UnpackedTy i tp -> tracePrtyMsg "UnpackedTy" i  hlTypE tp

hlGadtDecl :: GadtDecl SPI -> [Highlight]
hlGadtDecl (GadtDecl i nm tp) = (hlSrcSpan HlKeyword . head . srcInfoPoints $ i) : hlName True nm : hlTypE tp
