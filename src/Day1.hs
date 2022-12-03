{-# LANGUAGE MultiWayIf #-}
module Day1 where

import Data.List (foldl')
import Data.Text (Text)
import Text.Parsec (digit, newline, manyTill, sepBy, many, Parsec)
import Text.Parsec.Text (parseFromFile)

newtype Inventory = Inventory [Int]

mkInventory :: [Int] -> Inventory
mkInventory = Inventory

type Parser a = Parsec Text () a

item :: Parser Int
item = read <$> ((:) <$> digit <*> manyTill digit newline)

inventory :: Parser Inventory
inventory = mkInventory <$> many item

rations :: Parser [Inventory]
rations = inventory `sepBy` newline

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

biggestInventory :: [Inventory] -> (Int, Int, Int)
biggestInventory = foldl' (\(acc1, acc2, acc3) (Inventory is) ->
    let x = sum is
    in if
    | x > acc1  -> (x,    acc1, acc2)
    | x > acc2  -> (acc1, x,    acc2)
    | x > acc3  -> (acc1, acc2, acc3)
    | otherwise -> (acc1, acc2, acc3)) (0, 0, 0)

day1 :: IO ()
day1 = do
    res <- parseFromFile rations "input/day1"
    case res of
        Left err -> print err
        Right xs -> do
            let (res1, res2, res3) = biggestInventory xs
            putStr "Part1: "
            print res1
            putStr "Part2: "
            print (res1 + res2 + res3)
