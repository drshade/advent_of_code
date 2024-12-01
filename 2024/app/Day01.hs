module Day01 where
import           Data.List   (sort)
import qualified Data.Map    as Map
import           Data.Maybe  (fromMaybe)
import           Handy
import           Text.Parsec (digit, many1, newline, spaces)

parser :: Parser [(Int, Int)]
parser = many1 $ (,) <$> (read <$> many1 digit) <* spaces
                     <*> (read <$> many1 digit) <* newline

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 1
    let matched = zip (sort $ fst <$> input) (sort $ snd <$> input)
    pure $ sum $ (\(e1, e2) -> abs $ e1 - e2) <$> matched

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 1
    let freqmap = foldr (\e acc -> Map.insertWith (+) e 1 acc) Map.empty (snd <$> input)
    pure $ foldr (\e acc -> (fromMaybe 0 $ Map.lookup e freqmap) * e + acc) 0 (fst <$> input)
