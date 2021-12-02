module Main where

import Advent.Of.Code
import Data.Text (Text)

import Days.Day1

main :: IO ()
main = runAdvent 2021 solve
  where
    solve 1 Part1 = Just day1part1
    solve 1 Part2 = Just day1part2
    solve _ _ = Nothing

