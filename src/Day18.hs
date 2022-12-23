{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Day18 where

import Data.Foldable (foldr')
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

data Point = Point {
    x :: !Int,
    y :: !Int,
    z :: !Int
  }

data VoxelMaterial = Air | Lava | Water
  deriving Eq

-- | First result: surface area,
--   Second result: surface area that is reachable by water
--   Assumes that (0, 0, 0) is a sutable point to start the floodfill from.
getDropletCharacterization :: [Point] -> (Int, Int)
getDropletCharacterization points = (allArea pointMap, allArea (foldr' buildMap pointMap airPoints) )
  where
    buildMap point = update (update (HashMap.insert point.z ()) point.y) point.x
    allArea pm = sum $ map (getFreeSides pm) points
    getFreeSides pm = (6 -) . countNeighbours pm
    countNeighbours pm point = length $ catMaybes
      [ getPoint pm (point { x = point.x - 1}), getPoint pm (point { x = point.x + 1})
      , getPoint pm (point { y = point.y - 1}), getPoint pm (point { y = point.y + 1})
      , getPoint pm (point { z = point.z - 1}), getPoint pm (point { z = point.z + 1})
      ]
    getPoint pm point = HashMap.lookup point.x pm >>= HashMap.lookup point.y >>= HashMap.lookup point.z
    pointMap = foldr' buildMap HashMap.empty points
    airPoints = Vector.ifoldr' (\x a1 b1 -> Vector.ifoldr'
                               (\y a2 b2 -> Vector.ifoldr'
                               (\z m xs -> if m == Air then Point x y z : xs else xs)
                               b2 a2) b1 a1) [] airfilled
    maxX = maximum (map x points) + 1
    maxY = maximum (map y points) + 1
    maxZ = maximum (map z points) + 1
    airfilled = floodfill (Point 0 0 0) startVolume
    emptyVolume = Vector.replicate maxX (Vector.replicate maxY (Vector.replicate maxZ Air))
    startVolume = setIndex (Point 0 0 0) Water $ foldr' (`setIndex` Lava) emptyVolume points
    floodfill point volume = foldr fill volume (xVariation ++ yVariation ++ zVariation)
      where
        xVariation = [ Point x point.y point.z | x <- [(point.x - 1) .. (point.x + 1)], x >= 0, x < maxX ]
        yVariation = [ Point point.x y point.z | y <- [(point.y - 1) .. (point.y + 1)], y >= 0, y < maxY ]
        zVariation = [ Point point.x point.y z | z <- [(point.z - 1) .. (point.z + 1)], z >= 0, z < maxZ ]
    fill point volume = case lookupIdx point volume of
      Just Air -> floodfill point (setIndex point Water volume)
      _        -> volume

lookupIdx :: Point -> Vector (Vector (Vector a)) -> Maybe a
lookupIdx point v = (v Vector.!? point.x) >>= (Vector.!? point.y) >>= (Vector.!? point.z)

setIndex :: Point -> a -> Vector (Vector (Vector a)) -> Vector (Vector (Vector a))
setIndex point value =
  \v0 -> (`Vector.modify` v0) $
  \v1 -> flip (MVector.modify v1) point.x $
  \v2 -> (`Vector.modify` v2) $
  \v3 -> flip (MVector.modify v3) point.y $
  \v4 -> (`Vector.modify` v4) $
  \v5 -> flip (MVector.modify v5) point.z $
  \_  -> value

update :: Hashable k => (HashMap k' a -> HashMap k' a) -> k
       -> HashMap k (HashMap k' a)  -> HashMap k (HashMap k' a)
update f k mp = HashMap.alter (Just . f . maybe HashMap.empty id) k mp

parsePoint :: String -> Point
parsePoint xs = case groupBy (\a b -> a /= ',' && b /= ',') xs of
    [x, _, y, _, z] -> Point (read x) (read y) (read z)
    _               -> error "Invalid input"

day18 :: IO ()
day18 = do
  points <- map parsePoint . lines <$> readFile "input/day18"
  let (surfaceArea, containedAir) = getDropletCharacterization points
  putStr "Part 1: "
  print surfaceArea
  putStr "Part 2: "
  print containedAir
