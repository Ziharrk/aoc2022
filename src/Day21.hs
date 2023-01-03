{-# LANGUAGE LambdaCase #-}
module Day21 where

import qualified Data.Map as Map
import Data.Text (Text)
import Text.Parsec (Parsec, anyChar, newline, space, manyTill, many1, digit, (<|>), char)
import Text.Parsec.Text (parseFromFile)

type Parser = Parsec Text ()

data Monkey = Monkey String MonkeyOp

-- The three operation functions are the operator itself
-- and the two inverses, where the first or second argument is demanded (respectively).
data MonkeyOp = MkNum Int | MkOp String String (Int -> Int -> Int) (Int -> Int -> Int) (Int -> Int -> Int)

evalMonkey :: Map.Map String MonkeyOp -> String -> Maybe Int
evalMonkey mkMap nm = Map.lookup nm mkMap >>= \case
  MkNum n            -> return n
  MkOp nm1 nm2 op _ _ -> op <$> evalMonkey mkMap nm1 <*> evalMonkey mkMap nm2

solveRiddle :: Map.Map String MonkeyOp -> Int
solveRiddle mkMap = case Map.lookup "root" mkMap of
  Nothing                   -> error "root monkey not found"
  Just (MkNum _)            -> error "root monkey is yelling a number"
  Just (MkOp nm1 nm2 _ _ _) -> case (evalMonkey noHuman nm1, evalMonkey noHuman nm2) of
                              (Just _, Just _)   -> error "human not involved in solveRiddle"
                              (Just n, Nothing)  -> findAnswer n nm2
                              (Nothing, Just n)  -> findAnswer n nm1
                              (Nothing, Nothing) -> error "riddle too hard"
  where
    findAnswer n "humn" = n
    findAnswer n nm = case Map.lookup nm mkMap of
      Nothing -> error "monkey not found"
      Just (MkNum _) -> error "human not involved in findAnswer"
      Just (MkOp nm1 nm2 _ op1 op2) -> case (evalMonkey noHuman nm1, evalMonkey noHuman nm2) of
        (Just _, Just _)   -> error "human not involved"
        (Just n1, Nothing) -> findAnswer (op2 n n1) nm2
        (Nothing, Just n2) -> findAnswer (op1 n n2) nm1
        (Nothing, Nothing) -> error "riddle too hard"
    noHuman = Map.delete "humn" mkMap

checkRiddle :: Map.Map String MonkeyOp -> Int -> Bool
checkRiddle mkMap n = case Map.lookup "root" mkMap of
  Nothing                   -> False
  Just (MkNum _)            -> False
  Just (MkOp nm1 nm2 _ _ _) -> evalMonkey fullMap nm1 == evalMonkey fullMap nm2
  where
    fullMap = Map.insert "humn" (MkNum n) mkMap

parseMonkey :: Parser Monkey
parseMonkey = do
  name <- manyTill anyChar (char ':')
  _ <- space
  Monkey name <$> parseMonkeyOp

parseMonkeyOp :: Parser MonkeyOp
parseMonkeyOp = parseNum <|> parseOp
  where
    parseNum = MkNum . read <$> many1 digit <* newline
    parseOp = do
      nm1 <- manyTill anyChar space
      op <- anyChar
      _ <- space
      nm2 <- manyTill anyChar newline
      case op of
        '+' -> return $ MkOp nm1 nm2 (+) (-) (-)
        '-' -> return $ MkOp nm1 nm2 (-) (+) (flip (-))
        '/' -> return $ MkOp nm1 nm2 div (*) (flip div)
        '*' -> return $ MkOp nm1 nm2 (*) div div
        _   -> fail "unknown operator"

day21 :: IO ()
day21 = do
  res <- parseFromFile (many1 parseMonkey) "input/day21"
  case res of
    Left err  -> print err
    Right mks -> do
      let monkeyMap = Map.fromList $ map (\(Monkey nm op) -> (nm, op)) mks
      putStr "Part 1: "
      case evalMonkey monkeyMap "root" of
        Nothing -> print "root monkey not found"
        Just n  -> print n
      putStr "Part 2: "
      let sol = solveRiddle monkeyMap
      if checkRiddle monkeyMap sol
        then print sol
        else print "Riddle solution incorrect"
