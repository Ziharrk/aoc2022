module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)

main :: IO ()
main = do
    putStrLn "Which day?"
    input <- getLine
    case input of
        "1" -> day1
        "2" -> day2
        "3" -> day3
        "4" -> day4
        "5" -> day5
        "6" -> day6
        _   -> putStrLn "Invalid day"
