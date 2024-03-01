module Helper.List
where

import qualified Data.HashSet as HashSet
import Data.MultiSet (MultiSet)


-- | Maps the given list by applying the given function to the given list.
--   The function must take the index of the element as its first argument.
--   The first index is 1.
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex func list = helper func list 1
    where 
        helper _ [] _ = []
        helper func [x] ind = [func ind x]
        helper func (x:xs) ind = func ind x : helper func xs (ind+1)


-- | Maps two lists into one
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys


-- | Flattens a given list. All elements of all sublists are concatenated into a new list.
flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten [x] = x
flatten (x:xs) = x ++ flatten xs


combine :: [Int] -> [Int] -> [Int]
combine [] [] = []
combine [] (y:ys) = y : combine [] ys
combine (x:xs) [] = x : combine xs []
combine (x:xs) (y:ys) = x+y : combine xs ys

-- | Adds all elements with the same index of each sublists
combineLists :: [[Int]] -> [Int]
combineLists [] = []
combineLists [x] = x
combineLists [x, y] = combine x y
combineLists (x:y:xs) = combine (combine x y) $ combineLists xs


-- | Extracts the values at the given index from all sublists into a new list.
extractIndexFromAllLists :: Int -> [[a]] -> [a]
extractIndexFromAllLists index lists = map (!! index) $ filter (\list -> length list > index) lists
