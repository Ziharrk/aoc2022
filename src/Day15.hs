module Day15 where

import Control.Monad (zipWithM)
import Data.List (sort)
import Data.List.Extra (nubOrd)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.Parsec (digit, string, option, many, Parsec)
import Text.Parsec.Char (newline)
import Text.Parsec.Text (parseFromFile)

-- Unfortunately, creating the full matrix of covered positions is too slow
-- due to exploding vector and lists sizes.
-- However, the (unvovered) area is pretty sparsely spread out,
-- so we can just generate the intervals of covered positions.
-- Also returns the number of beacons and probes in the given row.
createCoverageInfo :: (Int -> Int) -> [SensorData] -> Int -> (Int, [(Int, Int)])
createCoverageInfo constrain dat row =
  ( length $ nubOrd $ concatMap equipmentPosInRow dat
  , addIntervals $ sort $ mapMaybe (fmap getRowInterval . isCloseToRow) dat)
  where
    isCloseToRow d@(MkData x1 y1 x2 y2) =
      let (d1, d2) = (manhattan (x1, y1) (x1, row), manhattan (x1, y1) (x2, y2))
      in if d1 < d2 then Just (d, d1, d2) else Nothing
    getRowInterval (MkData x1 _ _ _, d1, d2) =
      let delta = d2 - d1 + if d2 > d1 then 0 else 1
      in (constrain (x1-delta), constrain (x1+delta))
    addIntervals ((l1, h1):(l2, h2):is)
      | h1 >= l2 = addIntervals ((l1, max h1 h2):is)
      | otherwise = (l1, h1) : addIntervals ((l2, h2):is)
    addIntervals i = i
    equipmentPosInRow (MkData x1 y1 x2 y2) =
      [(x, row) | (x, y) <- [(x1, y1), (x2, y2)], y == row]

data SensorData = MkData Int Int Int Int

type Parser = Parsec Text ()

parseData :: Parser SensorData
parseData = MkData <$>
  (string "Sensor at x=" *> int) <*>
  (string ", y="  *> int) <*>
  (string ": closest beacon is at x=" *> int) <*>
  (string ", y=" *> int)
  where
    int = (. read) <$> option id neg <*> many digit
    neg = negate <$ string "-"

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

day15 :: IO ()
day15 = do
  res <- parseFromFile (many (parseData <* newline)) "input/day15"
  case res of
    Left err -> print err
    Right dat -> do
      putStr "Part 1: "
      let (equipment, intervals) = createCoverageInfo id dat 2000000
          occupied = foldr (\(l, h) -> (+ (1 + h - l))) 0 intervals - equipment
      print occupied
      let (lower, upper) = (0, 4000000)
          exec i r = do
            putStr ("\r" ++ show i ++ " of " ++ show upper)
            return (i, snd $ createCoverageInfo (max lower . min upper) dat r)
      intervals' <- zipWithM exec [0..] [lower .. upper]
      putStr "\nPart 2: "
      -- one interval before and one after, assuming the free space is in the middle.
      case filter ((== 2) . length . snd) intervals' of
        [(idx, [(_, h), _])] -> print (idx + upper * (h + 1))
        _-> print "No solution found"
