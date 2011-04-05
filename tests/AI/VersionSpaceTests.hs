{-# LANGUAGE RankNTypes #-}
module AI.VersionSpaceTests where

import AI.VersionSpaces
import AI.Examples

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

-- import Test.QuickCheck
import Test.HUnit ( (@=?), Assertion )

tests :: Test
tests = testGroup "VersionSpace tests" [
          testCase "Union empties" test_emptyUnion1
        , testCase "Union Empty foo == id" test_emptyUnion2
        , testCase "Union foo Empty == id" test_emptyUnion3
        , testCase "Join Empty foo == Empty" test_emptyJoin1
        , testCase "Join foo Empty == Empty" test_emptyJoin2
        , testCase "Tr doesn't generate hypotheses" test_emptyTRisEmpty
        , testProperty "Tr id id id is 'id'" prop_IDtransform
        ]

-- | Check that the union operator on Empty version spaces behaves as expected.
test_emptyUnion1 :: Assertion
test_emptyUnion1 = length []                     @=? length (hypotheses $ union Empty Empty)

test_emptyUnion2 :: Assertion
test_emptyUnion2 = length (hypotheses constIdVS) @=? length (hypotheses $ union Empty constIdVS)

test_emptyUnion3 :: Assertion
test_emptyUnion3 = length (hypotheses constIdVS) @=? length (hypotheses $ union constIdVS Empty)

-- | Check that the join operator on Empty version spaces behaves as expected.
test_emptyJoin1 :: Assertion
test_emptyJoin1 = length [] @=? length (hypotheses $ join emptyVS constIdVS)

test_emptyJoin2 :: Assertion
test_emptyJoin2 = length [] @=? length (hypotheses $ join constIdVS emptyVS)

test_emptyTRisEmpty :: Assertion
test_emptyTRisEmpty = 0 @=? length (hypotheses $ tr id id id emptyVS)

prop_IDtransform :: Int -> Int -> Int -> Bool
prop_IDtransform x y z = let vs   = VS intHs
                             vsTr = tr id id id vs
                             -- | Train and execute a versionspace on the inputs:
                             eval vs = runVS (train vs x y) z
                             types = (x :: Int, y :: Int, z :: Int)
                         in eval vs == eval vsTr

-- | This is necessary to make the type checker happy in some cases.
emptyVS :: VersionSpace Int Int
emptyVS = Empty

-- | Version space that always returns @id@
constIdVS :: VersionSpace Int Int
constIdVS = VS $ BSR { storage = undefined
                     , narrow = \bsr _ _ -> bsr
                     , hypos = \_ -> [id]
                     }