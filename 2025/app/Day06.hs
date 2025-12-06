module Day06 where

import           Handy
import           Prelude              hiding (some, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Oper a = (a -> a -> a)

input :: Parser' [(Oper Int, [Int])]
input = do
    row <- some ((optional hspace *> num `sepBy1` hspace) <* newline)
    ops <- choice [char '*' $> ((*) @Int), char '+' $> ((+) @Int)] `sepEndBy` hspace
    pure $ zip ops $ transpose row

columninput :: Parser' [(Oper Int, [String])]
columninput = do
    rows <- some (some (digitChar <|> char ' ') <* newline)
    ops <- choice [char '*' $> ((*) @Int), char '+' $> ((+) @Int)] `sepEndBy` hspace
    pure $ zip ops (columnise ' ' rows)
    where
        -- where there is a common char in all inputs, thats the common index to split on
        columnise :: Eq a => a -> [[a]] -> [[[a]]]
        columnise delim rows = transpose $ split <$> rows
            where len = minimum $ length <$> rows
                  -- list of indexes which have a delim in all the rows
                  indexes = filter (\idx -> all ((== delim) . (!! idx)) rows) [0..len - 1]
                  -- Convert to ranges between delimiters, skipping the delim itself (+1)
                  ranges = zip (0 : fmap (+1) indexes) (indexes ++ [len])
                  split str = [take (e - s) (drop s str) | (s, e) <- ranges]

part1 :: IO Int
part1 = do
    cols <- parse' input <$> puzzle Main 2025 6
    pure $ sum $ uncurry foldr1 <$> cols

part2 :: IO Int
part2 = do
    cols <- parse' columninput <$> puzzle Main 2025 6
    -- horrible horrible horrible, but my brain is fried
    pure $ sum $ (\(oper, vals) -> foldr1 oper (read <$> transpose vals)) <$> cols
