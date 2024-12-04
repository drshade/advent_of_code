module Day04 where
import qualified Data.Map    as Map
import           Handy
import           Text.Parsec (getPosition, letter, many1, newline, optional,
                              sourceColumn, sourceLine)

type Pos = (Int, Int)
type Grid = (Map.Map Pos Char)
type Dir = Pos

parser :: Parser (Map.Map (Int, Int) Char)
parser = Map.fromList <$> many1 (xmas <* optional newline)
    where xmas = do
            pos <- getPosition
            c <- letter
            pure ((sourceColumn pos, sourceLine pos), c)

peek :: Grid -> Pos -> Dir -> Maybe Char
peek grid (px, py) (dx, dy) = Map.lookup (px+dx, py+dy) grid

extract :: Grid -> Pos -> Dir -> String -> String
extract grid pos@(px,py) dir@(dx,dy) acc
    | length acc == 4 = reverse acc
    | otherwise =
        case Map.lookup pos grid of
            Just next -> extract grid (px+dx,py+dy) dir (next : acc)
            Nothing   -> reverse acc

around :: [Dir]
around = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

mas :: Grid -> Pos -> Bool
mas grid pos =
    -- Hack fest - all rotations of MAS in the grid
    let pat = [ [('A',(0,0)),('M',(-1,-1)),('S',(1,-1)),('M',(-1,1)),('S',(1,1))]
              , [('A',(0,0)),('M',(-1,-1)),('M',(1,-1)),('S',(-1,1)),('S',(1,1))]
              , [('A',(0,0)),('S',(-1,-1)),('M',(1,-1)),('S',(-1,1)),('M',(1,1))]
              , [('A',(0,0)),('S',(-1,-1)),('S',(1,-1)),('M',(-1,1)),('M',(1,1))]
              ]
        x = (\p -> all (== True) $ (\(pm,pd) -> Just pm == peek grid pos pd) <$> p) <$> pat
     in any (== True) x

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 4
    let options = (\(k, d) -> extract input k d "") <$> [(p, d) | p <- Map.keys input, d <- around]
    pure $ length $ filter (== "XMAS") options

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 4
    pure $ length $ filter (== True) $ mas input <$> Map.keys input

