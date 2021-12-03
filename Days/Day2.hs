{-# LANGUAGE FlexibleContexts, TypeApplications, BlockArguments #-}
module Days.Day2 (day2part1, day2part2) where

import Advent.Of.Code.Input (wordsList, readt)

import qualified Data.Text as T
import Data.Text (Text, unpack)
import TextShow

import Data.Bifunctor (Bifunctor(..))
import Data.Monoid (Sum(..), Endo(..), Dual(..))
import Data.Coerce (coerce)
import Data.Type.Coercion (coerceWith)


--------------------------------------------------------------------
-- PART 1


day2part1' :: [(String, Int)] -> Int
day2part1' = uncurry (*) . bimap getSum getSum . foldMap convert
    where
        convert :: (String, Int) -> (Sum Int, Sum Int)
        convert ("forward", x) = (Sum x, mempty)
        convert ("down", x)    = (mempty, Sum x)
        convert ("up", x)      = (mempty, Sum (-x))
        convert _ = mempty

day2part1 :: Text -> Text
day2part1 = showt . day2part1' . wordsList (\[cmd,x] -> (unpack cmd, readt @Int x))


--------------------------------------------------------------------
-- PART 2


day2part2' :: [(String, Int)] -> Int
day2part2' = (\(p,d,_) -> p*d) . flip appEndo (0,0,0) . getDual . foldMap convert
    where
        coerceDE :: (a -> a) -> Dual (Endo a)
        coerceDE = coerce

        convert ("down", x)    = coerceDE \(p,d,a) -> (p,d,a+x)
        convert ("up", x)      = coerceDE \(p,d,a) -> (p,d,a-x)
        convert ("forward", x) = coerceDE \(p,d,a) -> (p+x,d+a*x,a)
        convert _ = mempty

day2part2 :: Text -> Text
day2part2 = showt . day2part2' . wordsList (\[cmd,x] -> (unpack cmd, readt @Int x))
