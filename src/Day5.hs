module Day5 where

import Control.Applicative ((<|>))
import Data.List (transpose)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import Text.Parsec (try, newline, manyTill, many, Parsec)
import Text.Parsec.Char (char, anyChar, string, digit, space)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Text (parseFromFile)

type Crate = Char
type Stack = [Crate]
type CargoBay = [Stack]

-- # of cargo, from, to
data Action = Action Int Int Int

data Puzzle = Puzzle CargoBay [Action]

type Parser = Parsec Text ()

parseCrate :: Parser Crate
parseCrate = char '[' *> anyChar <* char ']'

parseBlob :: Parser (Maybe Crate)
parseBlob = (Just <$> parseCrate)
  <|> (Nothing <$ string (replicate 3 ' '))

parseCargoBay :: Parser CargoBay
parseCargoBay = map catMaybes . transpose <$> many (try line)
  where
    line = parseBlob `sepBy` char ' ' <* newline

parseAction :: Parser Action
parseAction = Action <$>
  (string "move " *> (read <$> manyTill digit space)) <*>
  (string "from " *> (read <$> manyTill digit space)) <*>
  (string "to "   *> (read <$> manyTill digit space))

parsePuzzle :: Parser Puzzle
parsePuzzle = Puzzle <$>
  parseCargoBay <* manyTill anyChar newline <* newline <*>
  many parseAction

execute :: (forall a. [a] -> [a]) -> Puzzle -> CargoBay
execute stackOrdering (Puzzle cb actions) = go cb actions
  where
    go cargoBay [] = cargoBay
    go cargoBay (Action num from to : rest) =
      go (fst $ applyAction cargoBay num (from-1) (to-1)) rest

    -- Returns the new configuration of the stacks in the back and
    -- a potential action to be performed in the previous cargo stacks.
    applyAction :: CargoBay -> Int -> Int -> Int -> (CargoBay, Maybe (Stack, Int))
    applyAction (this:further) num 0 to =
      if to < 0
        then (bot:further, Just (top, abs to - 1))
        else (carryAction (bot:further) top to, Nothing)
      where (top, bot) = splitAt num this
    applyAction (this:further) num n to =
      case applyAction further num (n-1) (to-1) of
        (further', Just (stack, 0)) -> ((stackOrdering stack ++ this) : further', Nothing)
        (further', mb) -> (this : further', fmap (fmap pred) mb)
    applyAction [] _ _ _ = error "Encountered end of depot in applyAction"

    -- Carry a stack for the given amount of steps.
    carryAction (this:further) stack 0 = (stackOrdering stack ++ this) : further
    carryAction (this:further) stack n = this : carryAction further stack (n-1)
    carryAction [] _ _ = error "Encountered end of depot in carryAction"

readSolution :: CargoBay -> String
readSolution = mapMaybe listToMaybe

day5 :: IO ()
day5 = do
  res <- parseFromFile parsePuzzle "input/day5"
  case res of
    Left err -> print err
    Right p  -> do
      putStr "Part1: "
      print (readSolution (execute reverse p))
      putStr "Part2: "
      print (readSolution (execute id p))

-- For debugging only.
-- Originally I appended the moved stacks instead of putting them on top.

prettyPuzzle :: Puzzle -> String
prettyPuzzle (Puzzle cargoBay act) =
  prettyCargoBay cargoBay ++ unlines (map prettyAction act)

-- sideways
prettyCargoBay :: CargoBay -> String
prettyCargoBay input = unlines (map printLine padded)
  where
    printLine = concatMap printItem
    printItem Nothing  = "    "
    printItem (Just c) = '[':c:"] "
    padded = fmap (\xs -> replicate (height - length xs) Nothing ++ xs) maybed
    maybed = fmap (fmap Just) input
    height = maximum (map length input)

prettyAction :: Action -> [Char]
prettyAction (Action num from to) =
  "move "  ++ show num  ++
  " from " ++ show from ++
  " to "   ++ show to
