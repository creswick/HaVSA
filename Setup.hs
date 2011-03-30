import Distribution.Simple
main = defaultMain

-- import Distribution.Simple
-- import System.Cmd (system)
-- import System.Exit
-- import System

-- ghcCmd =  "ghc --make -odir dist/build -hidir dist/build -idist/build:src src/Unit.hs -main-is Unit.runTests -o unit"

-- main :: IO ()
-- main = defaultMainWithHooks (simpleUserHooks { runTests = quickCheck } )
--     where
--       quickCheck _ _ _ _ = do ec <- system $ ghcCmd
--                               case ec of
--                                 ExitSuccess -> system "./unit"
--                                 _           -> return ec
--                               return () 