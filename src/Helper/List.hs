module Helper.List
where


-- | The first index is 1
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex func list = helper func list 1
    where 
        helper _ [] _ = []
        helper func [x] ind = [func ind x]
        helper func (x:xs) ind = func ind x : helper func xs (ind+1)


flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten [x] = x
flatten (x:xs) = x ++ flatten xs


maxFromList :: (Ord a) => [a] -> a
maxFromList [x] = x
maxFromList (x:xs)
    | maximum xs > x = maximum xs
    | otherwise = x
