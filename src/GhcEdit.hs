{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import qualified Language.Haskell.GhcEdit as G

testAdder :: Int -> Int -> IO Int
testAdder x y = return $ G.testAdder x y


foreign export stdcall testAdder :: Int -> Int -> IO Int


main :: IO ()
main = undefined



