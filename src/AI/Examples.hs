{-# LANGUAGE TypeSynonymInstances, FunctionalDependencies, MultiParamTypeClasses  #-}
module AI.Examples where


import AI.VersionSpaces
import AI.LogicHelpers (choices, fairInts, observeAll)

import GHC.Real (infinity)

-- | Version space that learns fixed or relative
-- offsets into an input region:
sizeVS :: VersionSpace Int Int
sizeVS = (VS intHs) `union` (intFromRatTr $ VS ratHs)

-- | Transform to adapt Rational VSs to Integral VSs
intFromRatTr :: VersionSpace Rational Rational -> VersionSpace Int Int
intFromRatTr = Tr fromIntegral fromIntegral round

-- | Define a rectangle type to simplify the syntax and add semantics:
data Rectangle = Rect {x_coord :: Int,
                       y_coord :: Int,
                       width   :: Int,
                       height  :: Int
                      } deriving (Show, Eq)
                                 
-- | Define a 2-D Region type, also to add semantics and simplify syntax.
type Region1D = (Int, Int)

-- | Rectangle VS learns rectangles contained in a rectangular region.
-- This is simply a join of two 1-D regions, wrapped in a transform.
rectangleVS :: VersionSpace Rectangle Rectangle
rectangleVS = rectTr $ region1d `join` region1d
              where 
                rectTr = Tr decompose decompose compose
                compose ((x, w), (y, h)) = Rect x y w h
                decompose (Rect x y w h) = ((x, w), (y, h))

-- | The core components of RectangleVS: (1-D regions)
region1d :: VersionSpace Region1D Region1D
region1d = sizeVS `join` sizeVS -- offset and width.

-- | Hypothesis space of constant int functions.  This is a bit
-- wastefull, since the bounds collapse to be equal on one
-- example. However, it serves as an example of a BSR representation
-- that may be instructive to others.
intHs :: BSR (Int, Int) i Int
intHs = BSR { storage = (minBound :: Int, maxBound :: Int)
            , narrow = narrowIntHs
            , hypos = hyposIntHs
            }
        
narrowIntHs :: BSR (Int, Int) i Int -> i -> Int -> BSR (Int, Int) i Int
narrowIntHs EmptyBSR _ _          = EmptyBSR
narrowIntHs (BSR (l, u) f g) _ exOut 
         | exOut < l || u < exOut = EmptyBSR
         | otherwise              = BSR (exOut, exOut) f g

hyposIntHs :: BSR (Int, Int) i Int -> [(i -> Int)]
hyposIntHs EmptyBSR        = []
hyposIntHs (BSR (l,u) _ _) = [\_-> y | y <- observeAll $ fairInts l u] 

-- | Hypothesis space of ratio functions.
ratHs :: BSR (Rational, Rational) Rational Rational
ratHs =  BSR { storage = (-infinity, infinity)
             , narrow = narrowRatHs
             , hypos = hyposRatHs
             }

narrowRatHs :: BSR (Rational, Rational) Rational Rational 
               -> Rational 
               -> Rational 
               -> BSR (Rational, Rational) Rational Rational
narrowRatHs EmptyBSR         _ _  = EmptyBSR
narrowRatHs bsr@(BSR (n, d) f g) exIn exOut 
  | d == infinity         = bsr { storage = (exOut, exIn) }
  | exOut / exIn == n / d = bsr
  | otherwise             = EmptyBSR
    
    
-- | exOut < l || u < exOut = EmptyBSR
         -- | otherwise              = BSR (exOut, exOut) f g

-- | TODO ERC: pull in the code that uses Logic to intercalate values from 0.
hyposRatHs :: BSR (Rational, Rational) Rational Rational -> [Rational -> Rational]
hyposRatHs EmptyBSR        = []
-- | TODO ERC: this is not correct..
hyposRatHs (BSR (n, d) _ _) | d == infinity = [\_-> y | y <- [n .. d]]
                            | n == 0        = [\_ -> 0]
                            | otherwise     = [\x -> x * (n / d)]
