module Day22 where

import Data.Char (isDigit)
import Data.List (findIndex)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Debug.Trace

data Move = MkMove Int Rotate

data Rotate = Clock | CounterClock | None

data Dir = R | D | L | U
  deriving (Enum, Show)

reverseDir :: Dir -> Dir
reverseDir U = D
reverseDir R = L
reverseDir D = U
reverseDir L = R

walk :: (Dir -> (Int, Int) -> Maybe (Int,Int)) -> [Move] -> Vector (Vector Char) -> (Dir, Int, Int) -> (Dir, Int, Int)
walk _ [] _ coords = coords
walk wrap (MkMove 0     r : moves) maze (d, x, y) = traceShow (d, x, y) $ walk wrap moves maze (rotate d r, x , y)
walk wrap (MkMove steps r : moves) maze (d, x, y) = walk wrap (MkMove (pred steps) r : moves) maze updatedIdx
    where
        updatedIdx =
            let (x', y') = nextIndex d (x, y)
            in case get maze (x', y') of
                    Just '.' -> (d, x', y')
                    Just '#' -> (d, x, y)
                    _        -> case wrap d (x, y) of
                        Just (x'', y'') -> (d, x'', y'')
                        Nothing         -> (d, x, y)

get :: Vector (Vector b) -> (Int, Int) -> Maybe b
get maze (x, y) = maze V.!? y >>= (V.!? x)

findWrapMap :: Vector (Vector Char) -> Dir -> (Int, Int) -> Maybe (Int, Int)
findWrapMap maze d (x, y) = case get maze (x, y) of
    Just '.' -> findWrapMap maze d (nextIndex (reverseDir d) (x, y))
    Just '#' -> findWrapMap maze d (nextIndex (reverseDir d) (x, y))
    _        -> let idx = nextIndex d (x, y)
                in case get maze idx of
                        Just '.' -> Just idx
                        _        -> Nothing

-- The following has to be adapted for the specific cube net.
-- This maps a side region of the cube net to a different side region.
-- Uses findWrapMap basically just to check if the wrap point is free.
-- (i.e. the "_" case in findWrapMap after just one recursive call with case one or two.)
findWrapCube :: Vector (Vector Char) -> Dir -> (Int, Int) -> Maybe (Int, Int)
findWrapCube maze U (x, y)
    | x <   50 && y == 100 = findWrapMap maze R (50, x + 1 * 50)
    | x <  100 && y ==   0 = findWrapMap maze R (0, x + 2 * 50)
    | x >= 100 && y ==   0 = findWrapMap maze U (x - 2 * 50, 200)
findWrapCube maze D (x, y)
    | x <   50 && y == 199 = findWrapMap maze D (x + 2 * 50, 0)
    | x <  100 && y == 149 = findWrapMap maze L (49, x + 2 * 50)
    | x >= 100 && y ==  49 = findWrapMap maze L (99, x - 1 * 50)
findWrapCube maze R (x, y)
    | x == 149 && y <   50 = findWrapMap maze L (99, y - 2 * 50)
    | x ==  99 && y <  100 = findWrapMap maze U (y + 1 * 50, 49)
    | x ==  99 && y <  150 = findWrapMap maze L (149, 3 * 50 - y)
    | x ==  49 && y >= 150 = findWrapMap maze U (x - 2 * 50, 149)
findWrapCube maze L (x, y)
    | x ==  50 && y <   50 = findWrapMap maze R (0, 3 * 50 - y)
    | x ==  50 && y <  100 = findWrapMap maze D (y - 1 * 50, 100)
    | x ==   0 && y <  150 = findWrapMap maze R (50, x - 2 * 50)
    | x ==   0 && y >= 150 = findWrapMap maze D (y - 1 * 50, 100)
findWrapCube maze d idx = findWrapMap maze d idx

nextIndex :: Dir -> (Int, Int) -> (Int, Int)
nextIndex U (x, y) = (x, pred y)
nextIndex R (x, y) = (succ x, y)
nextIndex D (x, y) = (x, succ y)
nextIndex L (x, y) = (pred x, y)

rotate :: Dir -> Rotate -> Dir
rotate dir None = dir
rotate R CounterClock = U
rotate U Clock = R
rotate dir Clock = succ dir
rotate dir CounterClock = pred dir

parsePath :: String -> [Move]
parsePath xs =
    let (n, rest) = span isDigit xs
    in case rest of
        [] -> [MkMove (if null n then 0 else read n) None]
        (x:further) -> MkMove (if null n then 0 else read n) (parseRotate x) : parsePath further
    where
        parseRotate 'R' = Clock
        parseRotate 'L' = CounterClock
        parseRotate _   = error "Invalid rotation"

day22 :: IO ()
day22 = do
    input <- lines <$> readFile "input/day22"
    let maze = V.map V.fromList $ V.fromList (take (length input - 2) input)
    let path = parsePath $ (last input)
    let initY = 0
    case findIndex (== '.') (head input) of
        Nothing    -> error "No start position found"
        Just initX -> do
            -- do  let (d, x, y) = walk (findWrapMap maze) path maze (R, initX, initY)
            --     putStr "Part 1: "
            --     print (fromEnum d + 4 * succ x + 1000 * succ y)
            do  putStr "Part 2: "
                let (d, x, y) = walk (findWrapCube maze) path maze (R, initX, initY)
                print (fromEnum d + 4 * succ x + 1000 * succ y)
