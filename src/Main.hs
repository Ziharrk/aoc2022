module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)

main :: IO ()
main = do
    putStrLn "Which day?"
    input <- getLine
    case input of
        "1" -> day1
        "2" -> day2
        "3" -> day3
        _   -> putStrLn "Invalid day"
