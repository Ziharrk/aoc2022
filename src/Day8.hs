module Day8 where

import Data.List (transpose, foldl')
import Data.List.Extra (nubOrd)

countVisible :: [[((Int, Int), Int)]] -> Int
countVisible xs = length $ nubOrd $
  getUpVisible xs ++
  getDownVisible xs ++
  getLeftVisible xs ++
  getRightVisible xs
  where
    getUpVisible = concatMap checkColumn
      where
        checkColumn [] = []
        checkColumn ((idx, h):rest) = fst (foldl' checkHeight ([idx], h) rest)
        checkHeight (acc, maxH) (idx, h) =
          if h > maxH
            then (idx:acc, h)
            else (acc, maxH)
    getDownVisible = getUpVisible . map reverse
    getLeftVisible = getUpVisible . transpose
    getRightVisible = getUpVisible . transpose . reverse

getScenicScore :: (Int, Int) -> [[((Int, Int), Int)]] -> Int
getScenicScore (maxX, maxY) xs =
  maximum $ map getFullScenicScore $ concat xs
  where
    getFullScenicScore coord =
      getUpScenicScore coord *
      getDownScenicScore coord *
      getLeftScenicScore coord *
      getRightScenicScore coord

    getGenericScenicScore f g ((x, y), height) = getScore 0
      [(xCoord, yCoord)| xCoord <- f x maxX, yCoord <- g y maxY]
      where
        getScore !n [] = n
        getScore !n ((xCoord, yCoord):rest)
          | (_, h) <- xs !! xCoord !! yCoord
          , h < height = getScore (n + 1) rest
          | otherwise  = n + 1
    constCoord a = const [a]
    negCoord   a = const $ enumFromThenTo (a - 1) (a - 2) 0
    posCoord   a = enumFromThenTo (a + 1) (a + 2)
    getUpScenicScore = getGenericScenicScore posCoord constCoord
    getDownScenicScore = getGenericScenicScore negCoord constCoord
    getLeftScenicScore = getGenericScenicScore constCoord posCoord
    getRightScenicScore = getGenericScenicScore constCoord negCoord

day8 :: IO ()
day8 = do
  xs <- map (map (read . pure)) . lines <$> readFile "input/day8"
  let indexed = zipWith (\i1 -> zip [(i1, i2) | i2 <- [0..]]) [0..] xs
  putStr "Part 1: "
  print (countVisible indexed)
  putStr "Part 2: "
  print (getScenicScore (length xs - 1, length (head xs) - 1) indexed)
