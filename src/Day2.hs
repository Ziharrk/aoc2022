module Day2 where

import Data.Text (Text)
import Text.Parsec (char, newline, space, (<|>), many, Parsec)
import Text.Parsec.Text (parseFromFile)

data Move = Rock | Paper | Scissors

data Strategy = Strategy Move Move

type Parser = Parsec Text ()

strategy :: Parser Strategy
strategy = Strategy <$> enemy <* space <*> mine <* newline

enemy :: Parser Move
enemy = Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'

mine :: Parser Move
mine = Rock <$ char 'X' <|> Paper <$ char 'Y' <|> Scissors <$ char 'Z'

stratPoints1 :: Strategy -> Int
stratPoints1 (Strategy m1 m2) = case (m1, m2) of
  (Rock, Rock) -> 1 + 3
  (Rock, Paper) -> 2 + 6
  (Rock, Scissors) -> 3 + 0
  (Paper, Rock) -> 1 + 0
  (Paper, Paper) -> 2 + 3
  (Paper, Scissors) -> 3 + 6
  (Scissors, Rock) -> 1 + 6
  (Scissors, Paper) -> 2 + 0
  (Scissors, Scissors) -> 3 + 3

stratPoints2 :: Strategy -> Int
stratPoints2 (Strategy m1 m2) = case (m1, m2) of
  (Rock, Rock)     -> 0 + 3 -- loose
  (Rock, Paper)    -> 3 + 1 -- draw
  (Rock, Scissors) -> 6 + 2 -- win
  (Paper, Rock)     -> 0 + 1 -- loose
  (Paper, Paper)    -> 3 + 2 -- draw
  (Paper, Scissors) -> 6 + 3 -- win
  (Scissors, Rock)     -> 0 + 2 -- loose
  (Scissors, Paper)    -> 3 + 3 -- draw
  (Scissors, Scissors) -> 6 + 1 -- win

day2 :: IO ()
day2 = do
  res <- parseFromFile (many strategy) "input/day2"
  case res of
    Left err -> print err
    Right strats -> do
      putStr "Part1: "
      print (sum (map stratPoints1 strats))
      putStr "Part2: "
      print (sum (map stratPoints2 strats))
