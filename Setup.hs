{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.System
import Distribution.ModuleName hiding (main)
import Data.List


prty :: String -> String
prty = fst . foldl f ("", "")
    where
        f (s, pfx) c 
                    | c `elem` "{([" = let pfx' = pfx ++ "  " in (s ++ "\n" ++ pfx ++ [c], pfx')
                    | c `elem` "})]" = let pfx' = drop 2 pfx  in (s ++ "\n" ++ pfx' ++ [c], pfx')
                    | c `elem` "," =  (s ++ "\n" ++ pfx ++ [c], pfx)
                    | otherwise = (s ++ [c], pfx)

main =  defaultMainWithHooks  hks {confHook = confHookNew, postConf = postConfNew} 
    where
        hks  = simpleUserHooks
        confHookOrig = confHook hks
        confHookNew (pkgDesc', hookedBI) flgs = do
            let pkgDesc = exeToDll "ghc-edit" pkgDesc'
            {-putStrLn $ "pkDesc = " ++ prty (show pkgDesc)-}
            {-putStrLn $ "hookedBI = " ++ prty (show hookedBI)-}
            {-putStrLn $ "flgs = " ++ show flgs-}
            confHookOrig (pkgDesc, hookedBI) flgs
        postConfOrig = postConf hks
        postConfNew args flgs desc bi = do
            putStrLn $ "args = " ++ prty (show args)
            putStrLn $ "flgs = " ++ prty (show flgs)
            putStrLn $ "desc = " ++ prty (show desc)
            putStrLn $ "bi = " ++ (show bi)
            postConfOrig args flgs desc bi



exeToDll :: String -> GenericPackageDescription -> GenericPackageDescription
exeToDll s pkg = pkg {condExecutables = map f . condExecutables $ pkg}
    where
        f (n, cnd@CondNode {condTreeData = ex@Executable {exeName = exNm}})
                    | n == s    = (s, cnd{condTreeData = ex{exeName = s ++ ".dll"}})
                    | otherwise = (s, cnd)
