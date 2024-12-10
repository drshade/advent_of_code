module Day10 where
import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Handy
import           Text.Parsec   (digit, getPosition, many1, newline, optional,
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

uniq :: [Pos] -> [Pos]
uniq = Set.toList . Set.fromList

around :: [Pos]
around = [(0,-1),(1,0),(0,1),(-1,0)]

walk :: Grid -> Pos -> Int -> [Pos]
walk grid pos 9 = if Map.lookup pos grid == Just 9 then [pos] else []
walk grid pos@(x,y) n =
    if Map.lookup pos grid == Just n
        then let z = join $ ((\(dx,dy) -> walk grid (x+dx,y+dy) (n+1)) <$> around)
              in z
        else []

walk' :: Grid -> Pos -> Int -> Int
walk' grid pos 9 = if Map.lookup pos grid == Just 9 then 1 else 0
walk' grid pos@(x,y) n =
    if Map.lookup pos grid == Just n
        then sum $ (\(dx,dy) -> walk' grid (x+dx,y+dy) (n+1)) <$> around
        else 0

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 10
    pure $ sum $ length . (\pos -> uniq $ walk input pos 0) <$> Map.keys input

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 10
    pure $ sum $ (\pos -> walk' input pos 0) <$> Map.keys input
