module DayXX where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: IO [Int]
input = parse' (p <* eof) <$> puzzle Main 2026 XX
    where p = pure []

part1, part2 :: IO Int
part1 = sum <$> input
part2 = sum <$> input
