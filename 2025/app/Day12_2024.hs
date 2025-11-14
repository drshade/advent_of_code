module Day12_2024 where

import           Control.Monad.State  (State, evalState, get, modify)
import           Data.List            (sort)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe, mapMaybe)
import qualified Data.Set             as Set
import           Handy
import           Text.Megaparsec      hiding (Pos, State)
import           Text.Megaparsec.Char

type Grid = Map.Map XY Char
type Visited = Set.Set XY

parser :: Parser' Grid
parser = Map.fromList <$> many ((,) <$> xy <*> (letterChar <* optional newline))

cardinals :: XY -> [XY]
cardinals (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

neighbours :: XY -> Grid -> [XY]
neighbours (x,y) grid =
    let typ = fromMaybe (error "BANG! Universe imploded.") $ Map.lookup (x,y) grid
     in mapMaybe (\p -> if Map.lookup p grid == Just typ then Just p else Nothing)
                 (cardinals (x,y))

flood :: Grid -> XY -> State Visited (Set.Set XY)
flood grid (x,y) = do
    -- Track that we have visited this position
    modify (Set.insert (x,y))
    -- Recursively call ourselves until we run out of neighbours, skipping visited
    cluster <- mapM   (\here -> do
                visited <- get
                if here `Set.notMember` visited
                    then flood grid here
                    else pure Set.empty
            )
            (neighbours (x,y) grid)
    -- Add ourselves to the cluster
    pure $ Set.insert (x,y) $ Set.unions cluster

groups :: Grid -> State Visited [Set.Set XY]
groups grid =
    -- Flood from every position, pruning whatever we have already visited
    mapM   (\here -> do
                visited <- get
                if here `Set.notMember` visited
                    then flood grid here
                    else pure Set.empty
            )
            (Map.keys grid)

perimeter :: Set.Set XY -> Int
perimeter cluster =
    -- For each point, get the cardinals which are outside (ie on the boundary) and count them
    sum $ length . filter (`Set.notMember` cluster) . cardinals <$> Set.toList cluster

area :: Set.Set XY -> Int
area = Set.size

sides :: Set.Set XY -> Int
sides cluster = sum $ straightLines <$> dirs
    where
        dirs = [ (( 0,-1), snd, fst)  -- North: check above, group by y, runs in x
               , (( 0, 1), snd, fst)  -- South: check below, group by y, runs in x
               , ((-1, 0), fst, snd)  -- West:  check left,  group by x, runs in y
               , (( 1, 0), fst, snd)  -- East:  check right, group by x, runs in y
               ]

        straightLines ((dx,dy), grouper, extracter) =
            let edges = filter (\(x,y) -> (x+dx,y+dy) `Set.notMember` cluster) (Set.toList cluster)
                grouped = Map.fromListWith (++) [(grouper p, [extracter p]) | p <- edges]
             -- count the number of straight lines in the group
             -- e.g. [1,2,4,6] => [[1,2],[4],[6]] is 3 straight lines
             in sum $ length . chunkBy (\a b -> b - a == 1) . sort <$> Map.elems grouped

part1 :: IO Int
part1 = do
    grid <- parse' parser <$> puzzle Main 2024 12
    let grps = evalState (groups grid) Set.empty
    pure . sum . fmap ((*) <$> area <*> perimeter) $ grps

part2 :: IO Int
part2 = do
    grid <- parse' parser <$> puzzle Main 2024 12
    let grps = evalState (groups grid) Set.empty
    pure . sum . fmap ((*) <$> area <*> sides) $ grps
