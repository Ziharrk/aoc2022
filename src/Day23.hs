module Day23 where

import Data.List.Extra ((!?))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

data Tile = Empty | Elf
  deriving Eq

simulateWithRound :: Int -> Int -> Vector (Vector Tile) -> (Vector (Vector Tile), Int)
simulateWithRound 0 rnd grid = (grid, rnd)
simulateWithRound n rnd grid = if change
    then simulateWithRound (n -1) (rnd + 1) newGrid
    else (grid, rnd)
  where
    (change, newGrid) = foldr execute (False, grid) considered
    considered = V.ifoldr go [] grid
    go y row acc = V.ifoldr (go' y) acc row
    go' y x tile acc = case tile of
      Empty -> acc
      Elf
        | allEmpty grid [(x' + x, y' + y) | x' <- [-1 .. 1], y' <- [-1 .. 1], x' /= 0 || y' /= 0]
                    -> acc
        | otherwise -> case getStrategy rnd grid x y of
            Nothing -> acc
            Just (x', y') -> (x, y, x', y') : acc
    execute (x, y, x', y') (b, grid') = case filter (\(_, _, x'', y'') -> x'' == x' && y'' == y') considered of
      (_:_:_) -> (b, grid')
      _       -> (True, setCoord x' y' Elf $ setCoord x y Empty grid')

setCoord :: Int -> Int -> a -> Vector (Vector a) -> Vector (Vector a)
setCoord x y value =
  \v0 -> (`V.modify` v0) $
  \v1 -> flip (MV.modify v1) y $
  \v2 -> (`V.modify` v2) $
  \v3 -> flip (MV.modify v3) x $
  \_  -> value

atCoords :: Vector (Vector Tile) -> Int -> Int -> Maybe Tile
atCoords grid x y = grid V.!? y >>= (V.!? x)

allEmpty :: Vector (Vector Tile) -> [(Int, Int)] -> Bool
allEmpty grid = all (maybe True (==Empty). uncurry (atCoords grid))

getStrategy :: Int -> Vector (Vector Tile) -> Int -> Int -> Maybe (Int, Int)
getStrategy n grid x y =
    let possible = filter (\coords -> allEmpty grid coords)
                        [strats !! i | off <- [0..3], let i = (off + n) `mod` 4]
    in  case possible of
            [] -> Nothing
            (res:_) -> Just (res !! 1)
  where
    strats =
        [ [(x-1, y-1), (x, y-1), (x+1, y-1)]
        , [(x-1, y+1), (x, y+1), (x+1, y+1)]
        , [(x-1, y-1), (x-1, y), (x-1, y+1)]
        , [(x+1, y-1), (x+1, y), (x+1, y+1)]
        ]

boundedBox :: Vector (Vector Tile) -> (Int, Int, Int, Int)
boundedBox grid = (minX, minY, maxX, maxY)
  where
    minY = V.length (V.takeWhile (V.all (==Empty)) grid)
    maxY = sizeY - V.length  (V.takeWhile (V.all (==Empty)) $ V.reverse grid) - 1
    minX = case [ x | x <- [0..(sizeX-1)], V.all ((== Just Empty) . (V.!? x)) grid] of
                [] -> 0
                xs -> lastAdjacient xs + 1
    maxX = case [ x | x <- [(sizeX-1), (sizeX-2) .. 0], V.all ((== Just Empty) . (V.!? x)) grid] of
                [] -> sizeX
                xs -> lastAdjacient xs - 1
    lastAdjacient xs = case xs of
        [] -> error "lastAdjacient: empty list"
        [x] -> x
        (x:y:xs') -> if abs (x - y) == 1 then lastAdjacient (y:xs') else x

sizeX, sizeY :: Int
sizeX = 200
sizeY = 200

simulate :: Int -> Vector (Vector Tile) -> Vector (Vector Tile)
simulate n grid = fst $ simulateWithRound n 0 grid

simulateUntilEnd :: Vector (Vector Tile) -> Int
simulateUntilEnd = go 0
  where
    go n grid = case simulateWithRound 2 n grid of
        (grid', n') | n == n'   -> n
                    | otherwise -> go n' grid'

day23 :: IO ()
day23 = do
    input <- lines <$> readFile "input/day23"
    let toTile '#' = Elf
        toTile _   = Empty
        fromCoord x y = case input !? y >>= (!? x) of
            Nothing -> Empty
            Just c  -> toTile c
        origX = length (head input)
        origY = length input
        halfDiffX = (sizeX - origX) `div` 2
        halfDiffY = (sizeY - origY) `div` 2
        grid = V.generate sizeY $ (\y -> V.generate sizeX (\x ->  fromCoord (x - halfDiffX) (y - halfDiffY)))
        grid' = simulate 10 grid
        (minX, minY, maxX, maxY) = boundedBox grid'
        emptyTiles = length $ [(x, y) | x <- [minX..maxX], y <- [minY..maxY], atCoords grid' x y == Just Empty]
    putStr "Part 1: "
    print emptyTiles
    putStr "Part 2: "
    print $ succ $ simulateUntilEnd grid
