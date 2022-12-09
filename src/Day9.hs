{-# LANGUAGE LambdaCase #-}
module Day9 where

import Control.Monad.ST (runST, ST)
import Data.Text (Text)
import Data.Vector (MVector)
import Data.Vector qualified as Vector (replicate, thaw)
import Data.Vector.Mutable qualified as Vector (read, exchange)
import Text.Parsec (Parsec, digit, oneOf, many)
import Text.Parsec.Char (space, newline)
import Text.Parsec.Text (parseFromFile)

type Parser = Parsec Text ()

data Action = Act !Direction !Int

data Direction = U | D | L | R

parseDirection :: Parser Direction
parseDirection = oneOf "UDLR" >>= \case
    'U' -> return U
    'D' -> return D
    'L' -> return L
    'R' -> return R
    _   -> fail "Invalid direction"

parseAction :: Parser Action
parseAction = Act <$> parseDirection <* space <*> (read <$> many digit)

simulate :: Int -> Int -> [Action] -> Int
simulate halfSize tailLength acts = runST (mkMatrix >>= go acts 0 (replicate tailLength (0, 0)) 0 0)
  where
    mkMatrix :: ST s (MVector s (MVector s Bool))
    mkMatrix = do
      let size = halfSize * 2 + 1
      matrix <- mapM Vector.thaw (Vector.replicate size (Vector.replicate size False))
      Vector.thaw matrix

    moveTail t xh yh = case scanl movePart (xh, yh) t of
      []                           -> error "Empty rope"
      _:rest | (x, y) <- last rest -> (x, y, rest)

    movePart (xh, yh) (xt, yt) = case (abs (xh-xt), abs (yh-yt)) of
      (1, 1) -> (xt, yt)
      (1, 0) -> (xt, yt)
      (0, 1) -> (xt, yt)
      (_, _) -> (xt + signum (xh-xt), yt + signum (yh-yt))

    go [] !n !_ !_ !_ !_ = return n
    go (Act _   0   : rest) !n !t !xh !yh !m = go rest n t xh yh m
    go (Act dir num : rest) !n !t !xh !yh !m = do
      let (xh', yh') = case dir of
              U -> (xh, yh + 1)
              D -> (xh, yh - 1)
              L -> (xh - 1, yh)
              R -> (xh + 1, yh)
      let (xt', yt', t') = moveTail t xh' yh'
      row <- Vector.read m (xt' + halfSize)
      prev <- Vector.exchange row (yt' + halfSize) True
      go (Act dir (num - 1) : rest) (if prev then n else n + 1) t' xh' yh' m

day9 :: IO ()
day9 = do
    res <- parseFromFile (many (parseAction <* newline)) "input/day9"
    case res of
      Left err -> print err
      Right actions -> do
        putStr "Part 1: "
        -- Board size has to be at least 487x487
        print $ simulate 243 1 actions
        putStr "Part 2: "
        -- This board can be just 483x483, since the tail moves less
        print $ simulate 241 9 actions
