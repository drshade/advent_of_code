
module DayXX where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' ()
input = pure ()

part1 :: IO ()
part1 = do
    values <- parse' input <$> puzzle Main 2025 XX
    pure ()

part2 :: IO ()
part2 = do
    values <- parse' input <$> puzzle Main 2025 XX
    pure ()
