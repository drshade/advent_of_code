module Day11 where

import qualified Data.Map             as Map
import           Data.MemoTrie
import qualified Data.Set             as Set
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' (Map String [String])
input = Map.fromList <$> some ((,) <$> (some letterChar <* string ": ")
                                   <*> (some letterChar `sepBy` hspace <* optional newline))

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 11
    pure $ memoFix (\rec node -> if node == "out" then 1
                                 else sum $ rec <$> fromJust (Map.lookup node values)
                    ) "you"

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 11
    pure $ memoFix (\rec (fft, dac, node) ->
                        if node == "out" then if fft && dac then 1 else 0
                        else sum $ (\n -> rec (fft || node == "fft", dac || node == "dac", n))
                                    <$> fromJust (Map.lookup node values)
                    ) (False, False, "svr")
