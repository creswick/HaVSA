module AI.LogicHelpers (
     fairInts
   , choices
   , absMinVal
   , module Control.Monad.Logic
  ) where

import Control.Monad.Logic
import Data.Function (on)

choices :: MonadPlus m => [a] -> m a
choices = msum . map return

-- | Generates all @Int@ values in the specified range, inclusive,
-- steadily increasing in absolute value.
fairInts :: Int -> Int -> Logic Int
fairInts a b = return start `mplus` (choices (sTail $ mkList start (max a b)) `interleave`
                                     choices (sTail $ mkList start (min a b)))
  where start = absMinVal a b
        sTail []       = []
        sTail xs@(x:_) = tail xs
        mkList s e = if s > e
                     then [s, pred s..e]
                     else [s..e]

-- | Find the value with minimum absolute value in the range @[a b]@
absMinVal :: Int -> Int -> Int
absMinVal a b = case (compare `on` signum) a b of
  LT -> 0 -- signs are different, so 0 is in the range.
  EQ -> case (compare `on` abs) a b of
         LT -> a
         EQ -> a
         GT -> b
  GT -> 0 -- signs are different, so 0 is in the range.