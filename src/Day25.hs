{-# LANGUAGE DerivingStrategies #-}
module Day25 where

{-# ANN module "HLINT: ignore Evaluate" #-}

newtype SNAFU = SNAFU Int
  deriving newtype (Eq, Ord, Bounded, Num, Enum, Real, Integral)

instance Read SNAFU where
    readsPrec :: Int -> ReadS SNAFU
    readsPrec _ str =
        let (snafuStr, rest) = span isSnafuChar str
        in [(SNAFU (stringToSNAFU $ reverse snafuStr), rest)]

isSnafuChar :: Char -> Bool
isSnafuChar c = c `elem` "210-="

stringToSNAFU :: String -> Int
stringToSNAFU = sum . zipWith charToSnafu [(0 :: Int) ..]
  where
    charToSnafu expo c = 5 ^ expo * case c of
        '2' -> 2
        '1' -> 1
        '0' -> 0
        '-' -> -1
        '=' -> -2
        _   -> error "Invalid SNAFU character"

instance Show SNAFU where
    show :: SNAFU -> String
    show (SNAFU 0) = "0"
    show (SNAFU s) = intToSNAFU s []
      where
        intToSNAFU 0 = id
        intToSNAFU n = case n `mod` 5 of
            4 -> intToSNAFU ((n + 1) `div` 5) . ('-' :)
            3 -> intToSNAFU ((n + 2) `div` 5) . ('=' :)
            2 -> intToSNAFU ((n - 2) `div` 5) . ('2' :)
            1 -> intToSNAFU ((n - 1) `div` 5) . ('1' :)
            0 -> intToSNAFU ((n - 0) `div` 5) . ('0' :)
            _ -> error "Invalid SNAFU number"

day25 :: IO ()
day25 = do
    input <- map read . lines <$> readFile "input/day25"
    print (sum input :: SNAFU)
