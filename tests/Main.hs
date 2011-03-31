module Main where

import AI.Tests       ( tests )

import Test.Framework ( defaultMain )



main :: IO ()
main = defaultMain tests
