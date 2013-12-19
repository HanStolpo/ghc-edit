{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Exp  where


import GHC.Generics
import Language.Haskell.Exts.Annotated
-- import Control.Monad
import Control.Applicative
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
                    | HlPragma
                    | HlBrace
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

hlModuleHead :: Maybe (ModuleHead SPI) -> [Highlight]
hlModuleHead Nothing = []
hlModuleHead (Just (ModuleHead l mName mWarning mExpList)) = [mImport, hlModuleName mName, mWhere] ++ hlWarningText mWarning
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


{-hlExporSpecList :: Maybe (ExportSpecList SPI) -> [Hightlight]-}
{-hlExportSpecList Nothing = []-}
{-hlExportSpecList (Just (ExportSpecList i es)) = trace (prty . show $ i) (concatMap hlExportSpec es)-}

{-hlExportSpec :: ExportSpec SPI -> [Hightlight]-}
{-hlExportSpec = undefined-}


