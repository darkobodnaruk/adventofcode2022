import Data.List.Split

-- 2a

contents <- readFile "2a.txt"
foldr (+) 0 $ map(score) $ splitOn "\n" contents

score :: String -> Integer
score s = scoreShape(resp) + scoreOutcome(op)(resp)
    where spl = splitOn " " s
          op = spl!!0
          resp = pickShape(spl!!1)

pickShape :: String -> String
pickShape "X" = "R"
pickShape "Y" = "P"
pickShape "Z" = "S"

scoreShape :: String -> Integer
scoreShape "R" = 1
scoreShape "P" = 2
scoreShape "S" = 3

scoreOutcome :: String -> String -> Integer
scoreOutcome ("A") ("R") = 3
scoreOutcome ("A") ("P") = 6
scoreOutcome ("A") ("S") = 0
scoreOutcome ("B") ("R") = 0
scoreOutcome ("B") ("P") = 3
scoreOutcome ("B") ("S") = 6
scoreOutcome ("C") ("R") = 6
scoreOutcome ("C") ("P") = 0
scoreOutcome ("C") ("S") = 3


-- 2b

score2 :: String -> Integer
score2 s = scoreShape(resp) + scoreOutcome(op)(resp)
    where spl = splitOn " " s
          op = spl!!0
          resp = pickShape2(op)(spl!!1)

pickShape2 :: String -> String -> String
pickShape2 ("A") ("X") = "S"
pickShape2 ("A") ("Y") = "R"
pickShape2 ("A") ("Z") = "P"
pickShape2 ("B") ("X") = "R"
pickShape2 ("B") ("Y") = "P"
pickShape2 ("B") ("Z") = "S"
pickShape2 ("C") ("X") = "P"
pickShape2 ("C") ("Y") = "S"
pickShape2 ("C") ("Z") = "R"


foldr (+) 0 $ map(score2) $ splitOn "\n" contents
