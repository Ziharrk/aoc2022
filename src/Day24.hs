module Day24 where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Set qualified as S

data Tile = Blizzard [Direction] | Empty | Wall
  deriving Show

data Direction = U | D | L | R
  deriving Show

toTile :: Char -> Tile
toTile '.' = Empty
toTile '#' = Wall
toTile '>' = Blizzard [R]
toTile '<' = Blizzard [L]
toTile '^' = Blizzard [U]
toTile 'v' = Blizzard [D]
toTile _   = error "Invalid tile"

-- Assumes that it takes at most 999 minutes to the extraction point.
bfsSimulation :: [(Int, Int)] -> Vector (Vector Tile) -> [Int]
bfsSimulation waypoints input = go 0 waypoints (S.singleton (1, 0)) input []
  where
    maxX = V.length (V.head input) - 1
    maxY = V.length input - 1
    emptyGrid = V.generate (maxY+1) $ \y -> V.generate (maxX+1) $ \x ->
      if (x == 0 || y == 0 || x == maxX || y == maxY) && (x, y) /= (1, 0) && (x, y) /= (maxX-1, maxY)
        then Wall
        else Empty
    go _ [] _ _ = reverse
    go n (waypoint : rest) reachable grid
      | S.member waypoint reachable = go (n+1) rest (S.singleton waypoint) newGrid . (n :)
      | otherwise = go (n+1) (waypoint : rest) (S.unions $ map (\(x, y) -> S.fromList $ possible x y) $ S.toList reachable) newGrid
      where
        possible x y =
          filter (\(x', y') -> x' >= 0 && x' <= maxX && y' >= 0 && y' <= maxY && isEmpty (x', y') newGrid)
          [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        newGrid = foldr setIndexed emptyGrid $ V.ifoldr (flip . V.ifoldr . collect) [] grid
        collect y x (Blizzard dirs) = (++ map (check  . nextIndex (x, y)) dirs)
        collect _  _  _             = id
        check (x, y, dir) = case getIndexed grid (x, y) of
          Just Wall -> (x + offsetX dir, y + offsetY dir, dir)
          _         -> (x, y, dir)
        offsetX dir = case dir of
          L -> maxX - 1
          R -> negate (maxX -1)
          _ -> 0
        offsetY dir = case dir of
          U -> maxY - 1
          D -> negate (maxY - 1)
          _ -> 0
        nextIndex (x, y) dir = case dir of
          U -> (x, y - 1, dir)
          D -> (x, y + 1, dir)
          L -> (x - 1, y, dir)
          R -> (x + 1, y, dir)

setIndexed :: (Int, Int, Direction) -> Vector (Vector Tile) -> Vector (Vector Tile)
setIndexed (x, y, d) grid = case getIndexed grid (x, y) of
  Just Empty         -> V.update grid $ V.singleton (y, V.update (grid V.! y) $ V.singleton (x, Blizzard [d]))
  Just (Blizzard ds) -> V.update grid $ V.singleton (y, V.update (grid V.! y) $ V.singleton (x, Blizzard (d:ds)))
  _                  -> error "index out of bounds"

getIndexed:: Vector (Vector a) -> (Int, Int) -> Maybe a
getIndexed grid (x, y) = grid V.!? y >>= (V.!? x)

isEmpty :: (Int, Int) -> Vector (Vector Tile) -> Bool
isEmpty (x, y) grid = case getIndexed grid (x, y) of
  Just Empty -> True
  _          -> False

day24 :: IO ()
day24 = do
  input <- V.fromList . map (V.fromList . map toTile) . lines <$> readFile "input/day24"
  let maxX = V.length (V.head input) - 1
      maxY = V.length input - 1
      res  = bfsSimulation [(maxX-1, maxY), (1, 0), (maxX-1, maxY)] input
  putStr "Part 1: "
  print $ head res
  putStr "Part 2: "
  print $ last res
