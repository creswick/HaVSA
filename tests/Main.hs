module Main where

import qualified AI.Tests as LogicHelpers
import qualified AI.VersionSpaceTests as VersionSpaces

import Test.Framework ( defaultMain )

main :: IO ()
main = defaultMain [ LogicHelpers.tests
                   , VersionSpaces.tests
                   ]
