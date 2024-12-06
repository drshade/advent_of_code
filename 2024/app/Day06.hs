module Day06 where
import           Control.Applicative ((<|>))
import qualified Data.Map            as Map
import           Data.Set            (Set, empty, insert, member, size)
import           Handy
import           Text.Parsec         (char, getPosition, many1, newline,
                                      optional, sourceColumn, sourceLine)

type Pos = (Int, Int)
type Grid = (Map.Map Pos Item)
data Item = Open | Obstacle | Guard deriving (Eq, Show)

parser :: Parser Grid
parser = Map.fromList <$> many1 (block <* optional newline)
    where block = do
            pos <- (,) <$> (sourceColumn <$> getPosition)
                       <*> (sourceLine <$> getPosition)
            c <- Open <$ char '.' <|> Obstacle <$ char '#' <|> Guard <$ char '^'
            pure (pos, c)

turn :: Pos -> Pos
turn (0,-1) = (1,0)
turn (1,0)  = (0,1)
turn (0,1)  = (-1,0)
turn (-1,0) = (0,-1)
turn _      = error "Invalid facing!"

walk :: Grid -> Pos -> Pos -> Set Pos -> Int
walk grid pos@(gx,gy) facing@(dx,dy) history =
    case Map.lookup (gx+dx, gy+dy) grid of
        Just Open     -> walk grid (gx+dx, gy+dy) facing (insert pos history)
        Just Obstacle -> walk grid pos (turn facing) (insert pos history)
        Just Guard    -> error "Found a guard??"
        Nothing       -> size (insert pos history)

guard :: Grid -> Pos
guard = fst . head . filter ((== Guard) . snd) . Map.toList

part1 :: IO ()
part1 = do
    grid <- parse parser <$> getInput Main 2024 6
    let grid' = Map.insert (guard grid) Open grid
    print $ walk grid' (guard grid) (0,-1) empty

walk' :: Grid -> Pos -> Pos -> Pos -> Set (Pos, Pos) -> Bool
walk' grid obstacle pos@(gx,gy) facing@(dx,dy) history =
    let grid' = Map.insert obstacle Obstacle grid
     in (member (pos, facing) history || (case Map.lookup (gx+dx, gy+dy) grid' of
        Just Open     -> walk' grid obstacle (gx+dx, gy+dy) facing (insert (pos, facing) history)
        Just Obstacle -> walk' grid obstacle pos (turn facing) (insert (pos, facing) history)
        Just Guard    -> error "Found a guard??"
        Nothing       -> False))

part2 :: IO ()
part2 = do
    grid <- parse parser <$> getInput Main 2024 6
    let grid' = Map.insert (guard grid) Open grid
    let opens = filter (\pos -> Map.lookup pos grid == Just Open) $ Map.keys grid
    let loops = (\obstacle -> walk' grid' obstacle (guard grid) (0,-1) empty) <$> opens
    print $ length $ filter id loops
