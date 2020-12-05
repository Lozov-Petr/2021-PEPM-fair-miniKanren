module Util.Miscellaneous where

import Data.List

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) -> c
trd4 (_, _, x, _) = x

lst4 :: (a, b, c, d) -> d
lst4 (_, _, _, x) = x

generateSplits :: Eq a => [a] -> Int -> [([a], [a])]
generateSplits xs n =
  let sub = filter (\x -> n == length x) $ subsequences xs in
  [ (x, xs \\ x) | x <- sub ]

show' :: Show a => [a] -> String 
show' xs = 
  intercalate "\n" $ map show xs 
