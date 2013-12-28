{-# LANGUAGE DeriveGeneric, PackageImports, RankNTypes, TypeFamilies, ExistentialQuantification, GADTs #-}
-- GADTs implies ExistentialQuantification but haskell-exts package does support the implication at the moment
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

data BlahS = BlahS deriving Show

data BlahRec = BlahRec {blhRecI :: Int, blhRecF :: Float} deriving ((Show))

data Blah a b = Blah1 [a] (M.Map a b) | Blah2 ![a] !(M.Map a b) deriving (Show, Generic)

data BlahOr a b = BlahL a | BlahR b

data BlahRecE  = forall a (b :: *). Show a => BlahRecE {blahRecE :: a} 

data BlahOrRec a b = forall . BL a | BR {br, br' :: !b, bl :: a} deriving Show

data (Show a, Show b) => Ifx a b = a `Ifx` b | b :---: !a deriving Show

data Gadt a where
    G1 :: Int -> Gadt Int
    G2 :: b -> b -> Gadt b
    (:-^-:) :: Show a => Gadt a -> Gadt a -> Gadt a
    -- GR {gr1 :: Int, gr2, gr3 :: Float} :: Gadt (Int,Float)  -- not supported by haskell-exts package yet

data Gadt2 :: * -> * where
    Gadt2 :: Int -> Gadt2 Int


makeBlah :: M.Map a b -> Blah a b
makeBlah aTOb = Blah1 (M.keys aTOb) aTOb

type TMakeBlah a b = M.Map a b -> Blah a b
type TK0 = forall a. (Num a, Show a) => a 
type TK1 = forall a. Num a => a 
type TK2 = forall a. a -> a
type TK3 = (Int, Float)
type TK4 = [Int]
type family TF1 a
type instance TF1 Int = [Int]

type family (TF2 a) :: * -> *
type family TF3 a :: (* -> *) -> *

class BlahClass a where
    blahShow :: a -> String

class BlahClass2 a where
    blahShow2 :: a -> String

instance (Show a, Show b) => BlahClass (Blah a b) where
    blahShow (Blah1 as aTOb) = "blahShow" ++ show as ++ show aTOb
    

blah :: Blah Int Float
blah = makeBlah . M.fromList $ [(1, 3.4), (2, 3.5), (3, 1e10)]

bleh :: BlahRec -> String
bleh _ = "Bleh"
