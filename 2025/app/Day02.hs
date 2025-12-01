
module Day02 where

import           Handy
import           Prelude              hiding (empty, insert, some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

input :: Parser' [Int]
input = pure []

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 1
    pure 0

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 1
    pure 0
