module Day08 where
import           Control.Applicative ((<|>))
import           Control.Monad       (join)
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes)
import qualified Data.Set            as Set
import           Handy
import           Text.Parsec         (Parsec, char, digit, getPosition,
                                      getState, letter, many1, modifyState,
                                      newline, optional, sourceColumn,
                                      sourceLine)

type Pos = (Int, Int)
type Grid = Map.Map Pos Char
type Boundary = Pos

-- Maybe a bit too fancy - parses the grid and also the boundary extents
parser :: Parsec String Boundary (Grid, Boundary)
parser = do
    grid <- Map.fromList . catMaybes <$> many1 (freq <* optional newline)
    boundary <- getState
    pure (grid, boundary)
    where freq = do
            pos <- (,) <$> (sourceColumn <$> getPosition) <*> (sourceLine <$> getPosition)
            _ <- modifyState (\(x, y) -> (max x (fst pos), max y (snd pos)))
            c <- (Just <$> letter) <|> (Just <$> digit) <|> (Nothing <$ char '.')
            case c of
                Nothing -> pure Nothing
                Just c' -> pure $ Just (pos, c')

bounded :: Boundary -> Pos -> Bool
bounded (w, h) (x, y) = x >= 1 && y >= 1 && x <= w && y <= h

uniq :: [Pos] -> [Pos]
uniq = Set.toList . Set.fromList

calc :: Pos -> Pos -> [Pos]
calc (ax,ay) (bx,by) =
    let (dx,dy) = (bx - ax, by - ay)
     in [(ax-dx, ay-dy), (ax+(dx*2), ay+(dy*2))]

antinodes :: Boundary -> Grid -> [Pos]
antinodes boundary grid =
    let nodes = Map.toList grid
        antis = join $ [calc (fst a) (fst b) | a <- nodes, b <- nodes, snd a == snd b, fst a /= fst b]
     in uniq $ filter (bounded boundary) antis

calc' :: Boundary -> Pos -> Pos -> [Pos]
calc' boundary (ax,ay) (bx,by) =
    let (dx,dy) = (bx - ax, by - ay)
     in [(ax,ay), (bx,by)] ++ go (dx, dy) (ax, ay) ++ go (-dx, -dy) (bx, by)
     where go (dx, dy) (x, y) =
            let (x', y') = (x-dx, y-dy)
             in if bounded boundary (x', y')
                    then (x', y') : go (dx, dy) (x', y')
                    else []

antinodes' :: Boundary -> Grid -> [Pos]
antinodes' boundary grid =
    let nodes = Map.toList grid
        antis = [calc' boundary (fst a) (fst b) | a <- nodes, b <- nodes, snd a == snd b, fst a /= fst b]
     in uniq $ join antis

part1 :: IO Int
part1 = do
    (grid, boundary) <- parseWithState parser (0,0) <$> getInput Main 2024 8
    pure $ length $ antinodes boundary grid

part2 :: IO Int
part2 = do
    (grid, boundary) <- parseWithState parser (0,0) <$> getInput Main 2024 8
    pure $ length $ antinodes' boundary grid
