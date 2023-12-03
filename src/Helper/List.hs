module Helper.List
where

import qualified Data.HashSet as HashSet
import Data.MultiSet (MultiSet)

-- | The first index is 1
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex func list = helper func list 1
    where 
        helper _ [] _ = []
        helper func [x] ind = [func ind x]
        helper func (x:xs) ind = func ind x : helper func xs (ind+1)

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys


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

combineLists :: [[Int]] -> [Int]
combineLists [] = []
combineLists [x] = x
combineLists [x, y] = combine x y
combineLists (x:y:xs) = combine (combine x y) $ combineLists xs


extractIndexFromAllLists :: Int -> [[a]] -> [a]
extractIndexFromAllLists index lists = map (!! index) $ filter (\list -> length list > index) lists


-- filterFromOtherList :: [(MultiSet String, MultiSet String)] -> [(MultiSet String, MultiSet String)] -> [(MultiSet String, MultiSet String)]
-- filterFromOtherList list filterList = filter (\x -> not $ HashSet.member (show $ fst x) hset) list
--     where
--         hset = HashSet.fromList $ map (show . fst) filterList

-- filterFromOtherLists :: (Show a) => [a] -> [[a]] -> [a]
-- filterFromOtherLists list filterLists = filter (\e -> not $ HashSet.member (show e) hset) list
--     where
--         buildHashSet [] = HashSet.empty
--         buildHashSet [l] = HashSet.fromList $ map show l
--         buildHashSet ([x]:ls) = HashSet.insert (show x) $ buildHashSet ls
--         buildHashSet ((x:xs):ls) = HashSet.insert (show x) $ buildHashSet (xs:ls)
--         hset = buildHashSet filterLists
