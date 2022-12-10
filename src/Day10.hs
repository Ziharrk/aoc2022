module Day10 where

import Data.Text (Text)
import Text.Parsec (Parsec, digit, many, (<|>), optionMaybe)
import Text.Parsec.Char (newline, string, char)
import Text.Parsec.Text (parseFromFile)

type Parser = Parsec Text ()

data OP = NOOP | Add Int
  deriving Show

parseOP :: Parser OP
parseOP = NOOP <$ string "noop" <|> Add <$> (string "addx " *> parseInt)

parseInt :: Parser Int
parseInt = create <$> optionMaybe (char '-') <*> many digit
  where
    create Nothing  xs = read xs
    create (Just _) xs = negate (read xs)

execute :: forall a. (Int -> Int -> a -> a) -> a -> [OP] -> a
execute operator start xs = go xs 0 1 start
  where
    go [] _ _ = id
    go (op:ops) cycl reg = case op of
      NOOP  -> checkAndIncrement (go ops (cycl + 1) reg) (cycl + 1) reg
      Add n -> checkAndIncrement
                (checkAndIncrement
                  (go ops (cycl + 2) (reg + n))
                  (cycl + 2) reg)
                (cycl + 1) reg

    checkAndIncrement :: (a -> a) -> Int -> Int -> a -> a
    checkAndIncrement cont cycl reg = cont . operator cycl reg

day10 :: IO ()
day10 = do
    res <- parseFromFile (many (parseOP <* newline)) "input/day10"
    case res of
      Left err  -> print err
      Right ops -> do
        putStr "Part 1: "
        let strengthAdder cycl reg =
              if cycl `mod` 40 == 20
                then ((cycl * reg) +)
                else id
        print $ execute strengthAdder 0 ops
        putStrLn "Part 2: "
        let printer cycl reg =
              let xPos = (cycl - 1)  `mod` 40
              in (if xPos == 39
                    then (++ "\n")
                    else id)
                  .
                  if xPos == reg - 1 || xPos == reg || xPos == reg + 1
                    then (++ "#")
                    else (++ ".")
        putStr $ execute printer "" ops
