{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import qualified Language.Haskell.GhcEdit as G

foreign export ccall testAdder :: Int -> Int -> IO Int
testAdder :: Int -> Int -> IO Int
testAdder x y = return $ G.testAdder x y




main :: IO ()
main = undefined



