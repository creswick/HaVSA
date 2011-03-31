{-# LANGUAGE RankNTypes #-}
module AI.VersionSpaceTests where

import AI.VersionSpaces
import AI.Examples

import Test.Framework                      (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck (testProperty)

-- import Test.QuickCheck
import Test.HUnit

tests = testGroup "VersionSpace tests" [
          testCase "Union empties" test_emptyUnion1
        , testCase "Union Empty foo == id" test_emptyUnion2
        , testCase "Union foo Empty == id" test_emptyUnion3
        , testCase "Join Empty foo == Empty" test_emptyJoin1
        , testCase "Join foo Empty == Empty" test_emptyJoin2
        ]

-- | Check that the union operator on Empty version spaces behaves as expected.
test_emptyUnion1 = length []                     @=? length (hypotheses $ union Empty Empty)
test_emptyUnion2 = length (hypotheses constIdVS) @=? length (hypotheses $ union Empty constIdVS)
test_emptyUnion3 = length (hypotheses constIdVS) @=? length (hypotheses $ union constIdVS Empty)

-- | Check that the join operator on Empty version spaces behaves as expected.
test_emptyJoin1 = length [] @=? length (hypotheses $ union Empty constIdVS)
test_emptyJoin2 = length [] @=? length (hypotheses $ union constIdVS Empty)


-- | Version space that always returns @id@
constIdVS :: VersionSpace Int Int
constIdVS = VS $ BSR { storage = undefined
                     , narrow = \bsr _ _ -> bsr
                     , hypos = \_ -> [id]
                     }