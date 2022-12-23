module Main where

import Data.List.Extra ((!?))
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))

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
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import Day20 (day20)
import Day21 (day21)
import Day22 (day22)
import Day23 (day23)

allDays :: [IO ()]
allDays =
    [ day1, day2, day3, day4, day5
    , day6, day7, day8, day9, day10
    , day11, day12, day13, day14, day15
    , day16, day17, day18, day19, day20
    , day21, day22, day23
    ]

main :: IO ()
main = do
    putStrLn "Which day?"
    input <- getLine
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering
    case reads input of
        [(n, "")]
            | n >= 1 && n <= 24 ->
                case allDays !? (n - 1) of
                    Just day -> day
                    Nothing  -> putStrLn "Day not implemented yet"
        _   -> putStrLn "Invalid day! It has to be a number between 1 and 24."
