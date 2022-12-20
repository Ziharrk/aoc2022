module Day20 where

import Control.Monad.ST (ST)
import Data.List (findIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

mix :: Int -> [Int] -> [Int]
mix steps input = map snd $ V.toList $ foldr (\_ -> go 0) inV [1..steps]
  where
    inV = V.fromList (zip [(1::Int)..] input)
    size = length input
    go n vec
      | n >= size = vec
      | otherwise = case V.findIndex ((==ident) . fst) vec of
          Nothing  -> error "not found"
          Just idx -> go (n+1) (V.modify (\v -> move v size idx (signum mv) (idx + (mv `rem` (size - 1)))) vec)
        where
          (ident, mv) = inV V.! n

move :: MV.MVector s (Int, Int) -> Int -> Int -> Int -> Int -> ST s ()
move _ _    _   0   _ = return ()
move v size idx sig n
  | n == idx  = return ()
  | otherwise = do
    let newIdx = idx+sig
    let wrapAround i = (i + size) `rem` size
    MV.swap v (wrapAround idx) (wrapAround newIdx)
    move v size newIdx sig n

day20 :: IO ()
day20 = do
  input <- map read . lines <$> readFile "input/day20"
  putStr "Part 1: "
  let atModulusIndex xs i = xs !! (i `mod` length xs)
  let res1 = mix 1 input
  case findIndex (==0) res1 of
    Nothing  -> error "not found"
    Just idx -> do
      print (res1 `atModulusIndex` (idx + 1000) +
             res1 `atModulusIndex` (idx + 2000) +
             res1 `atModulusIndex` (idx + 3000))
  putStr "Part 2: "
  let res2 = mix 10 (map (*811589153) input)
  case findIndex (==0) res2 of
    Nothing  -> error "not found"
    Just idx -> do
      print (res2 `atModulusIndex` (idx + 1000) +
             res2 `atModulusIndex` (idx + 2000) +
             res2 `atModulusIndex` (idx + 3000))
