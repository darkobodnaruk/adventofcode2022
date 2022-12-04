import Data.List.Split

-- 1a

contents <- readFile "1a.txt"
strs = filter (/=[]) $ splitOn [""] $ splitOn "\n" contents
nums = map (map read) strs :: [[Integer]]
sums = map (foldr (+) 0) nums
foldr max 0 sums

-- 1b

top3 = take 3 (reverse (sort sums))
foldr (+) 0 top3