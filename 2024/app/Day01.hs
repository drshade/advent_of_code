module Day01 where
import           Data.List   (sort)
import qualified Data.Map    as Map
import           Data.Maybe  (fromMaybe)
import           Handy
import           Text.Parsec (char, digit, many1, newline, optional)

parser :: Parser [(Int, Int)]
parser = many1 $ (,) <$> (read <$> many1 digit) <* (many1 $ char ' ')
                     <*> (read <$> many1 digit) <* optional newline

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 1
    let matched = zip (sort $ fst <$> input) (sort $ snd <$> input)
    pure $ sum $ (\(e1, e2) -> abs $ e1 - e2) <$> matched

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 1
    let simmap = foldr (\e acc -> Map.insertWith (+) e 1 acc) Map.empty (snd <$> input)
    pure $ foldr (\e acc -> (fromMaybe 0 $ Map.lookup e simmap) * e + acc) 0 (fst <$> input)
