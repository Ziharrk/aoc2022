module Day4 where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Text.Parsec (newline, manyTill, many, Parsec)
import Text.Parsec.Char (char, digit)
import Text.Parsec.Text (parseFromFile)

data Section = Section Int Int

type Pair = (Section, Section)

type Parser = Parsec Text ()

parseSection :: Parser Section
parseSection = Section <$> fmap read (manyTill digit (char '-'))
                       <*> fmap read (manyTill digit (newline <|> char ','))

parsePair :: Parser Pair
parsePair = (,) <$> parseSection <*> parseSection

checkPairFull :: Pair -> Bool
checkPairFull (Section l1 u1, Section l2 u2) =
  (l1 <= l2 && u1 >= u2) ||
  (l2 <= l1 && u2 >= u1)

checkPairSemi :: Pair -> Bool
checkPairSemi (Section l1 u1, Section l2 u2) =
  if l1 >= l2
    then l1 <= u2
    else u1 >= l2

day4 :: IO ()
day4 = do
  res <- parseFromFile (many parsePair) "input/day4"
  case res of
    Left err -> print err
    Right xs -> do
      putStr "Part1: "
      print (length (filter checkPairFull xs))
      putStr "Part2: "
      print (length (filter checkPairSemi xs))
