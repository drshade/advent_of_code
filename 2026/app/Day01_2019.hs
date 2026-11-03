module Day01_2019 where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: IO [Int]
input = parse' (p <* eof) <$> puzzle Main 2019 1
    where p = some $ num <* newline

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

fuel' :: Int -> Int -- sum of all positive fuel values after iteratively calling fuel (thanks lazy)
fuel' = sum . takeWhile (>0) . drop 1 . iterate fuel

part1, part2 :: IO Int
part1 = sum . fmap fuel  <$> input
part2 = sum . fmap fuel' <$> input
