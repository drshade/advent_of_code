module Day10 where
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import           Handy
import           Text.Parsec (digit, getPosition, many1, newline, optional,
                              sourceColumn, sourceLine)

type Pos = (Int, Int)
type Grid = (Map.Map Pos Int)

parser :: Parser Grid
parser = Map.fromList <$> many1 (block <* optional newline)
    where block = do
            pos <- (,) <$> (sourceColumn <$> getPosition)
                       <*> (sourceLine <$> getPosition)
            c <- read . (: []) <$> digit
            pure (pos, c)

around :: [Pos]
around = [(0,-1),(1,0),(0,1),(-1,0)]

walk :: Grid -> Int -> Pos -> [Pos]
walk grid 9 pos = if Map.lookup pos grid == Just 9 then [pos] else []
walk grid n pos@(x,y) =
    if Map.lookup pos grid == Just n
        then around >>= (\(dx,dy) -> walk grid (n+1) (x+dx,y+dy))
        else []

walk' :: Grid -> Int -> Pos -> Int
walk' grid 9 pos = if Map.lookup pos grid == Just 9 then 1 else 0
walk' grid n pos@(x,y) =
    if Map.lookup pos grid == Just n
        then sum $ (\(dx,dy) -> walk' grid (n+1) (x+dx,y+dy)) <$> around
        else 0

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 10
    pure $ sum $ length . uniq . walk input 0 <$> Map.keys input
    where uniq = Set.toList . Set.fromList

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 10
    pure $ sum $ walk' input 0 <$> Map.keys input
