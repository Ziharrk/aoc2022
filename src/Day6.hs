module Day6 where

import Data.List (sort)

-- | gets the number n of distinct characters to detect
-- and the stream of charaters.
-- Outputs the fist position where the previous n characters
-- are all distinct.
-- Assumes that the first n characters are not distinct.
checkStream :: Int -> String -> Int
checkStream n xs = go rest (firstThree, n')
  where
    n' = n-1
    (firstThree, rest) = splitAt n' xs
    go []     _        = error "end of stream"
    go (c:cs) (cur, m)
        | hasDuplicate (c:cur) = go cs (tail cur ++ [c], m + 1)
        | otherwise            = m + 1

hasDuplicate :: Ord a => [a] -> Bool
hasDuplicate = go . sort
  where
    go (x:y:xs) = x == y || go (y:xs)
    go _        = False

day6 :: IO ()
day6 = do
    xs <- readFile "input/day6"
    putStr "Part1: "
    print $ checkStream 4 xs
    putStr "Part2: "
    print $ checkStream 14 xs
