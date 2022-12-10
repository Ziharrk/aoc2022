module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)

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
        "7" -> day7
        "8" -> day8
        "9" -> day9
        "10" -> day10
        _   -> putStrLn "Invalid day"
