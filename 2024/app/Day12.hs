module Day12 where
import           Control.Monad       (join)
import           Control.Monad.State (State, get, gets, put, runState)
import           Data.Functor        (void)
import           Data.List           (partition)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Handy
import           Text.Parsec         (anyChar, getPosition, many1, newline,
                                      optional, sourceColumn, sourceLine)

type Pos = (Int, Int)
type Grid = (Map.Map Pos Char)

type Visited = Set.Set Pos

parser :: Parser Grid
parser = Map.fromList <$> many1 (block <* optional newline)
    where block = do
            pos <- (,) <$> (sourceColumn <$> getPosition)
                       <*> (sourceLine <$> getPosition)
            c <- anyChar
            pure (pos, c)

type Flood a = State (Grid, Visited) a

data Category a = Inside a | Outside a deriving (Show)

notVisited :: Visited -> Pos -> Bool
notVisited visited pos = Set.notMember pos visited

flood :: Char -> Pos -> Flood [Category Pos]
flood matching pos@(x,y) = do
    (grid, visited) <- get
    let neighbours = [(0,-1),(1,0),(0,1),(-1,0)]
                        >>= (\(dx,dy) ->
                                if notVisited visited (x+dx,y+dy)
                                    then case Map.lookup (x+dx,y+dy) grid of
                                        Nothing -> [Outside (x+dx,y+dy)]
                                        Just n  | n == matching -> [Inside (x+dx,y+dy)]
                                                | otherwise -> [Outside (x+dx,y+dy)]
                                    else []
                            )
    -- set neighbours and ourself as visited
    void $ put (grid, Set.union visited (Set.fromList $ pos : ((\case Inside a -> a; Outside a -> a) <$> neighbours)))
    pure neighbours

calc :: Pos -> Flood (Int, Int)
calc startpos = do
    grid <- gets fst
    let me = fromMaybe (error "uhh...?") $ Map.lookup startpos grid
    neighbours <- flood me startpos
    -- let area = length $ (\case Inside a -> a; Outside a -> a) <$> filter (\case Inside _ -> True; _ -> False) neighbours
    -- let circ = length $ (\case Inside a -> a; Outside a -> a) <$> filter (\case Inside _ -> False; _ -> True) neighbours

    let (inside, outside) = partition (\case Inside _ -> True; _ -> False) neighbours
    let z :: [Pos] = ((\case Inside a -> a; Outside a -> a) <$> inside)
    area :: [Category Pos] <- join <$> traverse (\p -> flood me p) z

    pure $ trace ("n=" <> show neighbours <> ";i=" <> show inside <> ";o=" <> show outside <> ";a=" <> show area) $ (length area, 0)

part1 :: IO ()
part1 = do
    grid <- parse parser <$> getInput (Example 1) 2024 12
    putStrLn $ show grid
    let x = fst $ runState (calc (1,1)) (grid, Set.empty)
    putStrLn $ show $ x
