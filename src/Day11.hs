module Day11 where

import Control.Monad.ST (runST)
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Text (Text)
import Data.Vector (fromList)
import Data.Vector qualified as Vector (thaw)
import Data.Vector.Mutable qualified as Vector (read, write, modify)
import Text.Parsec (Parsec, digit, many1, (<|>), oneOf, sepBy)
import Text.Parsec.Char (newline, string, char, space)
import Text.Parsec.Text (parseFromFile)

type Parser = Parsec Text ()

-- Items, operation, test (Divisor, Target True, Target False, Test)
data Monkey a = MkMonkey [a] (a -> a) (Int, Int, Int, a -> Int)

type Item = Int

type FastItem = [Int]

monkeyBusiness :: (a -> a) -> Integer -> [(Monkey a)] -> Integer
monkeyBusiness worryModifier rounds mks =
  foldr (*) 1 $ take 2 $ sortOn Down $
  -- records all monkeys throw count
  runST (Vector.thaw (fromList mks) >>= go rounds)
  where
    numMonkeys = length mks
    giveItem i (MkMonkey items op test) = MkMonkey (items ++ [i]) op test
    incAt _ [] = []
    incAt n (x:xs) | n == 0 = (x+1):xs
                   | otherwise = x:incAt (n-1) xs
    go 0 _       = return $ replicate numMonkeys 0
    go r monkeys = step 0
      where
        step n | n >= numMonkeys = go (r-1) monkeys
               | otherwise = do
          MkMonkey items op (divis, t, f, test) <- Vector.read monkeys n
          case items of
            [] -> step (n+1)
            (lvl:rest) -> do
              let worry = worryModifier (op lvl)
              let target = test worry
              Vector.write monkeys n (MkMonkey rest op (divis, t, f, test))
              Vector.modify monkeys (giveItem worry) target
              incAt n <$> step n

-- Use modular arithmetic to accelerate computation on huge numbers.
-- FastItem is a list of remainders of the item when divided by their respective (idx + 1).
-- Some entries are not needed though. Depends on the monkeys.
accelerate :: [Monkey Item] -> [Monkey FastItem]
accelerate monkeys = map accelerate' monkeys
  where
    maxDivisor = maximum $ map (\(MkMonkey _ _ (d, _, _,_)) -> d) monkeys
    seqList [] = []
    seqList (x:xs) = seqList xs `seq` x `seq` x:xs
    accelerate' (MkMonkey items op (divisor, t, f, _)) =
      MkMonkey (map accelerateItem items)
        (seqList . zipWith (\idx1 n -> op n `mod` idx1) [1..])
        (divisor, t, f, \itm -> if itm !! (divisor - 1) == 0 then t else f)
    accelerateItem i = [i `mod` d | d <- [1..maxDivisor]]

parseItem :: Parser Item
parseItem = read <$> many1 digit

parseOperation :: Parser (Int -> Int)
parseOperation = do
  _ <- string "  Operation: new = old "
  opS <- oneOf "+*"
  let op = case opS of
        '+' -> (+)
        '*' -> (*)
        _   -> error "Invalid operation"
  _ <- space
  ((\xs -> \old -> old `op` read xs) <$> many1 digit)
    <|> (const (\old -> old `op` old) <$> string "old")

parseTest :: Parser (Int, Int, Int, Int -> Int)
parseTest = do
  _ <- string "  Test: divisible by "
  divisor <- read <$> many1 digit
  _ <- newline >> string "    If true: throw to monkey "
  targetT <- read <$> many1 digit
  _ <- newline >> string "    If false: throw to monkey "
  targetF <- read <$> many1 digit
  let test = \x -> if x `mod` divisor == 0 then targetT else targetF
  return (divisor, targetT, targetF, test)

parseMonkey :: Parser (Monkey Item)
parseMonkey = do
  _ <- string "Monkey " >> many1 digit >> char ':' >> newline >> string "  Starting items: "
  items <- parseItem `sepBy` string ", "
  _ <- newline
  op <- parseOperation
  _ <- newline
  test <- parseTest
  return $ MkMonkey items op test

day11 :: IO ()
day11 = do
    res <- parseFromFile (many1 (parseMonkey <* many1 newline)) "input/day11"
    case res of
      Left err  -> print err
      Right monkeys -> do
        putStr "Part 1: "
        print $ monkeyBusiness (`div` 3) 20 monkeys
        putStrLn "Warning: Part two takes ~50s and ~1.5GB to compute without optimizations."
        putStrLn "Or just ~13s and ~0.2GB with optimizations."
        putStr "Part 2: "
        print $ monkeyBusiness id 10000 (accelerate monkeys)
