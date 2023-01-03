{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Day17 where

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Tuple.Extra (snd3)
import GHC.Generics (Generic)

data Direction = Lft | Rght deriving Show

data Rock = Horizontal | Plus | Angle | Vertical | Square
  deriving Enum

data RockField = Empty | Fixed | Falli
  deriving (Eq, Hashable, Generic)

instance Show RockField where
  show :: RockField -> String
  show = \case
    Empty -> "."
    Fixed -> "#"
    Falli -> "@"

type Layout = [[RockField]]

rockOrder :: [Rock]
rockOrder = cycle [Horizontal ..]

toDir :: Char -> Direction
toDir '<' = Lft
toDir '>' = Rght
toDir _ = error "Invalid direction"

mkRockShape :: Rock -> Layout
mkRockShape Horizontal = [[Empty, Empty, Falli, Falli, Falli, Falli, Empty]]
mkRockShape Plus       = [[Empty, Empty, Empty, Falli, Empty, Empty, Empty],
                          [Empty, Empty, Falli, Falli, Falli, Empty, Empty],
                          [Empty, Empty, Empty, Falli, Empty, Empty, Empty]]
mkRockShape Angle      = [[Empty, Empty, Falli, Falli, Falli, Empty, Empty],
                          [Empty, Empty, Empty, Empty, Falli, Empty, Empty],
                          [Empty, Empty, Empty, Empty, Falli, Empty, Empty]]
mkRockShape Vertical   = [[Empty, Empty, Falli, Empty, Empty, Empty, Empty],
                          [Empty, Empty, Falli, Empty, Empty, Empty, Empty],
                          [Empty, Empty, Falli, Empty, Empty, Empty, Empty],
                          [Empty, Empty, Falli, Empty, Empty, Empty, Empty]]
mkRockShape Square     = [[Empty, Empty, Falli, Falli, Empty, Empty, Empty],
                          [Empty, Empty, Falli, Falli, Empty, Empty, Empty]]

makeFixed :: Layout -> Layout
makeFixed = map (\case
  [a, b, c, d, e, f, g] -> [chg a, chg b, chg c, chg d, chg e, chg f, chg g]
  _                     -> error "Invalid layout")
  where
    chg x = if x == Falli then Fixed else x

emptyRow :: [RockField] -> Bool
emptyRow [Empty, Empty, Empty, Empty, Empty, Empty, Empty] = True
emptyRow _                                                 = False

trim, trimEnd :: [[RockField]] -> [[RockField]]
trim = dropWhile emptyRow
trimEnd = takeWhile (not . emptyRow)

simulateFor :: Int -> Layout -> [Direction] -> (Layout, Int, Int)
simulateFor n layoutStart = towerSize . go layoutStart (take n rockOrder) 0
  where
    emptyLayout = replicate 3 (replicate 7 Empty)
    towerSize (x, used) = (x, pred $ length x, used)

    go layout []     used  _  = (layout, used)
    go layout (r:rs) used ds = go layout' rs (used + used') (drop used' ds)
      where
        (layout', used') = simulateMove (emptyLayout ++ trim layout) (mkRockShape r) ds 0

    simulateMove _      _         []     _    = error "No more directions"
    simulateMove layout rockShape (d:ds) used = simulateFall layout (move d rockShape) ds (used + 1)
    simulateFall layout rockShape ds used = case fall layout rockShape of
      Nothing -> (reverse (makeFixed rockShape) ++ layout, used)
      Just (layout', rockShape') -> simulateMove layout' rockShape' ds used

    move d layout =
      if notElem Falli (map getEnd layout)
          && length allMoved == length layout
      then allMoved
      else layout
      where
        allMoved = mapMaybe moveRow layout
        moveRow row = merge (change d (getFalli row)) (getFixed row)
        getEnd = case d of
          Lft  -> head
          Rght -> last
        change Lft  = (++[Empty]) . tail
        change Rght = (Empty:) . init

    fall layout rockShape = case fallInLayout rockShape of
      Nothing -> Nothing
      Just rockShape' -> case merge (head layout) (head rockShape') of
        Nothing  -> Nothing
        Just row -> Just (tail layout, trimEnd (row:tail rockShape'))

    fallInLayout [] = return []
    fallInLayout [row] = return [getFalli row, getFixed row]
    fallInLayout (row1:row2:rs) = do
      rest <- fallInLayout (row2:rs)
      case rest of
        [] -> error "Falling produced empty layout"
        (row2':rs') -> do
          row1' <- merge row2' (getFixed row1)
          let row0 = getFalli row1
          return (getFalli row0:row1':rs')

    getFixed = map (\case
      Falli -> Empty
      x     -> x)
    getFalli = map (\case
      Falli -> Falli
      _     -> Empty)

    merge [] [] = return []
    merge (Empty:rs1) (r2:rs2) = (r2:) <$> merge rs1 rs2
    merge (r1:rs1) (Empty:rs2) = (r1:) <$> merge rs1 rs2
    merge _         _          = Nothing

type HeightProfile = Layout
type Memo = HashMap Int (HashMap HeightProfile (HeightProfile, Int, Int, Int))

simulateWithMemo :: Int -> Int -> [Direction] -> Memo -> HeightProfile -> Int -> Int -> Int
simulateWithMemo n cycleLength dat hmap profile used !acc
  | n < cycleLength = acc + snd3 (simulateFor n profile (drop used (cycle dat))) - pred (length profile)
  | otherwise       = case HashMap.lookup used hmap >>= HashMap.lookup profile of
    Nothing -> simulateWithMemo (n-cycleLength) cycleLength dat hmap' profile' realUsed (acc + realHeight)
      where
        hmap' = HashMap.alter (Just . HashMap.insert profile (profile', realHeight, realUsed, cycleLength)
                                    . fromMaybe HashMap.empty) used hmap
        realUsed = (used + used') `mod` length dat
        realHeight = height' - pred (length profile)
        (res, height', used') = simulateFor cycleLength profile (drop used (cycle dat))
        profile' = getProfile res
    Just (profile', height', used', reduced')
      | n < reduced'       -> acc + snd3 (simulateFor n profile (drop used (cycle dat))) - pred (length profile)
      | reduced' > 5000000 -> simulateWithMemo (n-reduced') cycleLength dat hmap profile' used' (acc + height')
      | otherwise          -> case HashMap.lookup used' hmap >>= HashMap.lookup profile' of
        Nothing -> simulateWithMemo (n-reduced') cycleLength dat hmap profile' used' (acc + height')
        Just (profile'', height'', used'', reduced'')
          | n < reduced' + reduced'' -> simulateWithMemo (n-reduced') cycleLength dat hmap profile' used' (acc + height')
          | otherwise                -> simulateWithMemo (n-reduced'-reduced'') cycleLength dat hmap'' profile'' used'' (acc + height' + height'')
            where
              hmap'' = HashMap.alter (Just . HashMap.insert profile (profile'', height' + height'', used'', reduced' + reduced'')
                                           . fromMaybe HashMap.empty) used hmap

getProfile :: Layout -> HeightProfile
getProfile = (++ [replicate 7 Fixed]) . trim . take 80 . map (map unfill) . takeWhile (elem Falli) . floodfill (replicate 7 Falli)
  where
    unfill Fixed = Fixed
    unfill _     = Empty
    floodfill :: [RockField] -> Layout -> Layout
    floodfill _    []     = []
    floodfill prev (x:xs) =
      let new = fillSide $ zipWith fill x prev
      in new:floodfill new xs

    fill Empty Falli = Falli
    fill x     _     = x
    fillSide xs       =
      let new = fillRow xs
      in if new == xs then xs else fillSide new
    fillRow []               = []
    fillRow [x]              = [x]
    fillRow (Falli:Empty:xs) = Falli:fillRow (Falli:xs)
    fillRow (Empty:Falli:xs) = Falli:fillRow (Falli:xs)
    fillRow (x:xs)           = x:fillRow xs

startSimulation :: Int -> [Direction] -> Int
startSimulation n dat = simulateWithMemo n (smallestCommonMultiple 5 (length dat)) dat HashMap.empty [replicate 7 Fixed] 0 0

smallestCommonMultiple :: Int -> Int -> Int
smallestCommonMultiple a b = a * b `div` gcd a b

day17 :: IO ()
day17 = do
  dat <- map toDir . takeWhile (/='\n') <$> readFile "input/day17"
  let !res1 = startSimulation 2022 dat
  putStr "Part 1: "
  print res1
  putStr "This will take approximately 5 minutes..."
  let !res2 = startSimulation 1000000000000 dat
  putStr "Part 2: "
  print res2

printLayout :: (Layout, Int) -> IO ()
printLayout (lay, size) = do
  mapM_ (putStrLn . concatMap show) lay
  putStr "Size:"
  print size
