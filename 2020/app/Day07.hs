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

get :: Bags -> String -> [(Int, String)]
get allbags name = fromMaybe (error "can't find bag!") $ Map.lookup name allbags

-- Very naive recursive dfs over the tree matching and counting bags
matches :: String -> Bags -> [(Int, String)] -> Int
matches which allbags = walk
    where walk [] = 0
          walk ((cnt, col):xs) = if col == which then cnt else 0
                                    + walk (get allbags col) -- Down
                                    + walk xs -- Across

part1 :: IO Int
part1 = do
    bags <- parse parser <$> getInput Main 2020 07
    -- Filter only the matches which have more than 0 matching bags
    pure $ length $ filter (( > 0) . snd) $ second (matches "shinygold" bags) <$> Map.toList bags

-- Walk graph, multiplying as we descend and summing each branch
count :: Bags -> [(Int, String)] -> Int
count allbags = walk
    where walk []              = 1
          walk ((cnt, col):xs) = cnt * walk (get allbags col) + walk xs
                            --   ^^ Multiply on descend         ^^ Sum across
part2 :: IO Int
part2 = do
    bags <- parse parser <$> getInput Main 2020 07
    pure $ count bags (get bags "shinygold") - 1
