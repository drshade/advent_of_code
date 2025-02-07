module Day07 where

import           Data.Bifunctor (second)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)
import           Parsing
import           Text.Parsec    (choice, digit, letter, many1, newline,
                                 optional, sepBy, space, string, try)

type Bags = Map.Map String [(Int, String)]

parser :: Parser Bags
parser = Map.fromList <$> many1 (entry <* optional newline)
    where colour = (<>) <$> word <* space <*> word where word = many1 letter
          bag    = (,) <$> read <$> many1 digit <* space
                       <*> colour <* (choice $ try <$> [string " bags", string " bag"])
          entry  = (,) <$> colour <* string " bags contain "
                       <*> (choice $ try <$> [ (string "no other bags" *> pure [])
                                             , (bag `sepBy` (string ", "))
                                             ]
                           ) <* string "."

-- Very naive recursive method for matching bags in sub-bags
matches :: String -> Bags -> [(Int, String)] -> Int
matches _ _ [] = 0
matches which allbags ((cnt, col):rest) =
    (if col == which then cnt else 0)
        + matches which allbags (rest ++ (fromMaybe [] $ Map.lookup col allbags))

part1 :: IO Int
part1 = do
    bags <- parse parser <$> getInput Main 2020 07
    -- Filter only the matches which have more than 0 matching bags
    pure $ length $ filter (( > 0) . snd) $ second (matches "shinygold" bags) <$> Map.toList bags

-- Walk graph, multiplying as we descend and summing each branch
count :: Bags -> [(Int, String)] -> Int
count _ [] = 1
count allbags ((cnt, col):rest) =
    (cnt * count allbags (fromMaybe [] $ Map.lookup col allbags)) + count allbags rest

part2 :: IO Int
part2 = do
    bags <- parse parser <$> getInput Main 2020 07
    let Just start = Map.lookup "shinygold" bags
    pure $ count bags start - 1
