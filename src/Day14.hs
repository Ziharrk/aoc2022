module Day14 where

import Data.List.Extra (splitOn)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (replicate, modify, (!))
import Data.Vector.Mutable qualified as MVector (modify, write)

newtype Path = MkPath [(Int, Int)]

toPath :: String -> Path
toPath = MkPath . map (toPoint . splitOn ",") . splitOn " -> "
  where
    toPoint [x, y] = (read x, read y)
    toPoint _      = error "Invalid path"

initializeCave :: Int -> Int -> [Path] -> Vector (Vector Bool)
initializeCave maxX maxY paths = foldr addPath clearCave paths
  where
    clearCave = Vector.replicate maxX $ Vector.replicate maxY False
    addPath (MkPath []) = id
    addPath (MkPath [p]) = addPoint p
    addPath (MkPath (p1:p2:ps))
      | p1 == p2  = addPath (MkPath (p2:ps)) . addPoint p1
      | otherwise = addPath (MkPath (moveTo p1 p2:p2:ps)) . addPoint p1
    moveTo (x1, y1) (x2, y2)
      | x1 == x2  = (x1, y1 + signum (y2 - y1))
      | otherwise = (x1 + signum (x2 - x1), y1)

addPoint :: (Int, Int) -> Vector (Vector Bool) -> Vector (Vector Bool)
addPoint (x, y) =
  Vector.modify $ \v1 -> flip (MVector.modify v1) x $
  Vector.modify $ \v2 -> MVector.write v2 y True

simulate :: Int -> Int -> [Path] -> Int
simulate maxX maxY paths = go 0 $ initializeCave maxX maxY paths
  where
    go n cave = maybe n (go (n+1)) $ dropSand (500, 0) cave
    dropSand (x, y) cave
      | outOfBounds (x,   y)         = Nothing
      | not (getPos (x,   y+1) cave) = dropSand (x,   y+1) cave
      | not (getPos (x-1, y+1) cave) = dropSand (x-1, y+1) cave
      | not (getPos (x+1, y+1) cave) = dropSand (x+1, y+1) cave
      | not (getPos (x,   y)   cave) = Just (addPoint (x, y) cave)
      | otherwise                    = Nothing
    getPos (x, y) cave
      | outOfBounds (x, y) = False
      | otherwise = cave Vector.! x Vector.! y
    outOfBounds (x, y) =
      x < 0 || x >= maxX ||
      y < 0 || y >= maxY

simulateWithBottom :: Int -> Int -> [Path] -> Int
simulateWithBottom maxX maxY paths = simulate newX newY (newPath:paths)
  where
    newPath = MkPath [(0, newY-1), (newX-1, newY-1)]
    -- assumes that maxX is at least wide enough to catch the spill
    -- to the left of the spawn point
    newX = maximum [maxX, maxX + newY]
    newY = maxY + 2

day14 :: IO ()
day14 = do
  paths <- map toPath . lines <$> readFile "input/day14"
  let allPoints = concatMap (\(MkPath p) -> p) paths
      maxX = maximum (map fst allPoints) + 1
      maxY = maximum (map snd allPoints) + 1
  putStr "Part 1: "
  print (simulate maxX maxY paths)
  putStr "Part 2: "
  print (simulateWithBottom maxX maxY paths)
