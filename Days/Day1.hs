module Days.Day1 (day1part1, day1part2) where

import Advent.Of.Code.Input (list)

import Data.Bool (bool)
import Data.Text (Text)
import TextShow


-- blackbird
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


--------------------------------------------------------------------
-- PART 1

day1part1' :: [Int] -> Int
day1part1' xs = sum $ zipWith (bool 0 1 .: (<)) xs (tail xs)

day1part1 :: Text -> Text
day1part1 = showt . day1part1' . list


--------------------------------------------------------------------
-- PART 2

day1part2' :: [Int] -> Int
day1part2' xs = sum $ zipWith (bool 0 1 .: (<)) xs (drop 3 xs)

day1part2 :: Text -> Text
day1part2 = showt . day1part2' . list