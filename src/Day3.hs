module Day3 where

import Data.Coerce (coerce)
import Data.Char (isUpper)
import Data.List (intersect, nub)
import Data.Text (Text)
import Text.Parsec (letter, newline, manyTill, many, Parsec)
import Text.Parsec.Text (parseFromFile)

newtype Item = Item { unItem :: Int }
  deriving Eq

mkItem :: Char -> Item
mkItem x | isUpper x = Item $ fromEnum x - 65 + 27
         | otherwise = Item $ fromEnum x - 97 + 1

data Rucksack = Rucksack [Item] [Item]

mkRucksack :: [Item] -> Rucksack
mkRucksack xs = uncurry Rucksack $ splitAt (length xs `div` 2) xs

type Parser = Parsec Text ()

parseItem :: Parser Item
parseItem = mkItem <$> letter

parseRucksack :: Parser Rucksack
parseRucksack = mkRucksack <$> manyTill parseItem newline

wrongItem :: Rucksack -> Item
wrongItem (Rucksack xs ys) = case nub $ xs `intersect` ys of
  [x] -> x
  _   -> error "Not a single or more than one duplicate found."

groupItems :: [Rucksack] -> [Item]
groupItems [] = []
groupItems (Rucksack a1 b1 : Rucksack a2 b2 : Rucksack a3 b3 : rest) =
  nub ((a1 ++ b1) `intersect` (a2 ++ b2) `intersect` (a3 ++ b3)) ++ groupItems rest
groupItems _ = error "Detected a Group with less than 3 elves."

day3 :: IO ()
day3 = do
  res <- parseFromFile (many parseRucksack) "input/day3"
  case res of
    Left err -> print err
    Right xs -> do
      putStr "Part1: "
      print (sum $ map (unItem . wrongItem) xs)
      putStr "Part2: "
      print (sum @[] @Int $ coerce $ groupItems xs)
