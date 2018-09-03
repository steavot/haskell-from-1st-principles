-- using take and drop write a fucntion called rvrs that reverses the sentance
-- "curry is awesome" to "awesome is curry"
module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 x) ++ (take 4 $ drop 5 $ x) ++ (take 5 x)

main :: IO ()
main = print $ rvrs "curry is awesome"

-- reverse any string
revAny :: String -> String
revAny xs = foldl (\acc x-> x:acc) "" xs

