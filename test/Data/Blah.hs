{-# LANGUAGE DeriveGeneric, PackageImports #-}
{- This is my one line multiline comment -}
{- This
 - is 
 - a multiline comments-}
module Blah {-#WARNING"not a real module" #-} ( BlahClass(blahShow)
                                              , BlahClass2(..)
                                              , Blah
                                              , BlahRec(blhRecI,blhRecF)
                                              , blah
                                              , bleh
                                              )  where

import GHC.Generics
import Data.List as L hiding (intersperse)
import {-# SOURCE #-} qualified Data.List as L (intersperse)
import qualified "containers" Data.Map as M

data BlahRec = BlahRec {blhRecI :: Int, blhRecF :: Float} deriving (Show)

data Blah a b = Blah [a] (M.Map a b) deriving (Show, Generic)

makeBlah :: M.Map a b -> Blah a b
makeBlah aTOb = Blah (M.keys aTOb) aTOb

class BlahClass a where
    blahShow :: a -> String

class BlahClass2 a where
    blahShow2 :: a -> String

instance (Show a, Show b) => BlahClass (Blah a b) where
    blahShow (Blah as aTOb) = "blahShow" ++ show as ++ show aTOb
    

blah :: Blah Int Float
blah = makeBlah . M.fromList $ [(1, 3.4), (2, 3.5), (3, 1e10)]

bleh :: BlahRec -> String
bleh _ = "Bleh"
