-- for each line
--  split string into half
--  for each character in first half, find in second half
--  map to priority
-- sum

import Data.List.Split

rucksackPriority :: String -> Maybe(Int)
rucksackPriority s = head $ map(priority) $ findCommonChar (fst spl) (snd spl)
    where spl = tuplify2 (splitString s)

splitString :: String -> [String]
splitString s = [take i s, drop i s]
    where i = length s `div` 2

findCommonChar :: String -> String -> String
findCommonChar s1 s2 = [c | c <- s1, c `elem` s2]

priority :: Char -> Maybe (Int)
priority c = (elemIndex c (['a'..'z']++['A'..'Z']))

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

contents <- readFile "3.txt"
-- contents <- readFile "3a.txt"

sumPriorities :: [Maybe Int] -> Maybe Int
sumPriorities prios = fmap sum $ sequence prios

sumPriorities $ map (rucksackPriority) $ splitOn "\n" contents

