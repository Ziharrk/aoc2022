module Day13 where

import Data.Bifunctor (first)
import Data.List (groupBy, sort, findIndices)

data ListOrInt = ListOrInt [ListOrInt] | Int Int

instance Eq ListOrInt where
  (==) :: ListOrInt -> ListOrInt -> Bool
  x == y = compare x y == EQ

instance Ord ListOrInt where
  compare :: ListOrInt -> ListOrInt -> Ordering
  compare (ListOrInt xs) (ListOrInt ys) = compare xs ys
  compare (Int x) (Int y) = compare x y
  compare x@(ListOrInt _) y@(Int _) = compare x (ListOrInt [y])
  compare x@(Int _) y@(ListOrInt _) = compare (ListOrInt [x]) y

instance Read ListOrInt where
  readsPrec :: Int -> String -> [(ListOrInt, String)]
  readsPrec _ xs@('[':_) = map (first ListOrInt) (reads xs)
  readsPrec _ xs = map (first Int) (reads xs)

tuple :: [a] -> (a, a)
tuple [x, y] = (x, y)
tuple _ = error "tuple: invalid input"

untuple :: (a, a) -> [a]
untuple (x, y) = [x, y]

day13 :: IO ()
day13 = do
  input <- map (tuple . map read)
            . filter ((== 2) . length)
            . groupBy (\x y -> null x == null y)
            . lines
            <$> readFile "input/day13"
  putStr "Part 1: "
  print $ sum $ map snd $ filter (\((x, y), _) -> x < y) $ zip input [(1 :: Int) ..]
  putStr "Part 2: "
  let divider1 = ListOrInt [ListOrInt [Int 2]]
      divider2 = ListOrInt [ListOrInt [Int 6]]
      fullList = sort $ divider1 : divider2 : concatMap untuple input
      indices = findIndices (\x -> x == divider1 || x == divider2) fullList
  print $ product $ map (+1) indices
