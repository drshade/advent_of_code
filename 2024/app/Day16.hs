module Day16 where
import           Control.Monad    (join)
import           Data.Bifunctor   (bimap)
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HashSet
import           Data.List        (find)
import qualified Data.Map         as Map
import           Handy
import           Text.Parsec      (char, choice, getPosition, many1, newline,
                                   sourceColumn, sourceLine)

type Pos = (Int, Int)
type Facing = (Int, Int)
data Block = Open | Wall | Start | End deriving (Show, Eq)
type Grid = Map.Map Pos Block

parser :: Parser Grid
parser = grid
    where   grid = do
                blocks <- (many1 $ many1 block <* newline)
                pure (Map.fromList $ join blocks)
            block = do
                pos <- (,) <$> (pred <$> sourceColumn <$> getPosition)
                           <*> (pred <$> sourceLine <$> getPosition)
                c <- choice [ Open <$ char '.'
                            , Wall <$ char '#'
                            , Start <$ char 'S'
                            , End <$ char 'E'
                            ]
                pure (pos, c)

manhattan :: (Pos, Facing) -> (Pos, Facing) -> Int
manhattan ((ax,ay),_) ((bx,by),_) = abs (ax - bx) + abs (ay - by)

options :: Grid -> (Pos, Facing) -> [(Pos, Facing)]
options grid (pos@(px, py), _) =
    let valid =
            filter (\p -> Map.lookup p grid `elem` [Just Open, Just End])
                $ bimap (px +) (py +) <$> [(0, 1), (0, -1), (1, 0), (-1, 0)]
        facing (x, y) (fx, fy) = (fx - x, fy - y)
     in [ (p, facing pos p) | p <- valid ]

cost :: (Pos, Facing) -> (Pos, Facing) -> Int
cost (_, (xf, yf)) (_, (xf', yf')) =
    1 + if abs (xf' - xf) == 1 || abs (yf' - yf) == 1 then 1000 else 0

routecost :: [(Pos, Facing)] -> Int
routecost path = sum $ zipWith cost path (tail path)

part1 :: IO Int
part1 = do
    grid <- parse parser <$> getInput Main 2024 16

    let start = maybe (error "no start") fst (find (\(_, block) -> block == Start) (Map.toList grid))
    let end = maybe (error "no start") fst (find (\(_, block) -> block == End) (Map.toList grid))
    let facing :: Facing = (1, 0)

    let route = aStar
                    (HashSet.fromList . options grid)   -- neighbours
                    cost                                -- cost function
                    (manhattan (end, facing))           -- heuristic cost function
                    (\(p, _) -> p == end)               -- termination
                    (start, facing)                     -- start

    pure $ maybe (error "no route") routecost route
