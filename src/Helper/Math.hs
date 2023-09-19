module Helper.Math
where

import Data.Set as Set
import Data.MultiSet as MultiSet


multiSetSupport :: Num a => Ord a => MultiSet.MultiSet a -> Set.Set a
multiSetSupport = MultiSet.toSet


-- | Transforms a given int-value into its corresponding binary representation. Only works for positive values
--   It returns a list containing all powers of 2 that combined represent the given value x.
--   I.e. this function returns the set of positions at which the binary encoding has a 1.
intToBinary :: Int -> [Int]
intToBinary 0 = []
intToBinary 1 = [0]
intToBinary x = i : intToBinary (x - 2^i)
    where
        i = floor $ logBase 2 $ fromIntegral x


ceilBinLog :: Int -> Int
ceilBinLog = ceiling . logBase 2 . fromIntegral

