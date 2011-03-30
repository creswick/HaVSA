module Tests where


import Control.Monad (liftM4, liftM2, liftM)
import Test.QuickCheck
import Data.List

import Control.Monad.Logic
import AI.VersionSpaces 
import AI.Examples
import AI.LogicHelpers

prop_absMinVal_sameSignGt x y = 
  x > 0 && y > 0 ==> absMinVal x y == min x y

prop_absMinVal_sameSignLt x y = 
  x < 0 && y < 0 ==> absMinVal x y == max x y

prop_absMinVal_span0 x y = 
  signum x /= signum y ==> absMinVal x y == 0

prop_fairInts_associate x y = 
  abs (x - y) < 10000 ==> -- stop the tests before they get huge
   (observeAll $ fairInts x y) == (observeAll $ fairInts y x)

prop_fairInts_unique x y = 
  abs (x - y) < 10000 ==> -- stop the tests before they get huge
  let ints = (observeAll $ fairInts x y) 
  in nub ints == ints
  


-- | threw an @*** Exception: Ratio.%: zero denominator@ initially.
checkSizes = let screen800 = Rect 0 0 800 600
                 example = (Rect 0 0 80 60)
                 rvs = train rectangleVS screen800 example
                 -- this is not yet demanded, because the condition
                 -- fails too soon:
                 results = runVS rvs screen800 
              in 
              -- Only 0,0,80x60 is valid, but it can be generated two ways:
              (length results == 2) && 
              (results!!0 == example) && 
              (results!!1 == example)

-- arbitraryBSR :: BSR a i o => Gen a
-- arbitraryBSR = oneof [AnyInt, AnyRat]

-- instance Arbitrary (VersionSpace i o) where
--   arbitrary = sized arbitraryVS
  
-- arbitraryVS :: Int -> Gen (VersionSpace i o)
-- arbitraryVS n | n <= 0    = liftM VS arbitraryBSR
--               | otherwise = oneof [ liftM2 join (arbitraryVS n/2) (arbitraryVS n/2) 
--                                   , liftM2 union (arbitraryVS n/2) (arbitraryVS n/2)
--                                     -- we should reduce the tr size a bit, but halving it may be excessive.
--                                   , liftM4 tr (return id) (return id) (return id) (arbitraryVS n/2)
--                                   ]


-- Quickcheck property ideas:
-- 
--   * every hypotheses is consistent with some training input, or no hypotheses exist:
--      case hypotheses (train v i o) of
--         Empty -> True
--         hs    -> map (\f -> f i) hs == take (length hs) $ repeat o
-- 
--   * Hypotheses sets shrink monotonically:
--      length $ hypotheses v >= length $ hypotheses $ train v i o
-- 
--   * Joining two version spaces results in hypotheses that are the cross product of the inputs. 
--     (even if some are Empty)
--       let l1 = length $ hypotheses v1
--           l2 = length $ hypotheses v2
--       in l1 * l2 == length $ hypotheses $ join v1 v2
-- 
--   * Unioning two version spaces is additive in the size of the hypotheses.  
--       let l1 = length $ hypotheses v1
--           l2 = length $ hypotheses v2
--       in l1 + l2 == length $ hypotheses $ union v1 v2
--
