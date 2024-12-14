module Day12 where
import           Control.Monad       (join)
import           Control.Monad.State (State, get, gets, put, runState)
import           Data.Functor        (void)
import           Data.IORef          (IORef, modifyIORef, readIORef)
import           Data.List           (insertBy, partition, unfoldr)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           GHC.IORef           (newIORef)
import           Handy
import           Text.Parsec         (anyChar, getPosition, many1, newline,
                                      optional, sourceColumn, sourceLine)

type Pos = (Int, Int)
type Grid = (Map.Map Pos Char)

parser :: Parser Grid
parser = Map.fromList <$> many1 (block <* optional newline)
    where block = do
            pos <- (,) <$> (sourceColumn <$> getPosition)
                       <*> (sourceLine <$> getPosition)
            c <- anyChar
            pure (pos, c)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c, b+d)

flood :: Grid -> Pos -> (Int, Int)
flood grid startpos =
    let x :: [(Int, Int)] = unfoldr (uncurry eat) (Map.toList grid, Set.singleton startpos)
        y = foldr add (0,0) $ x
     in y
    where
        eat :: [(Pos, Char)] -> Set.Set Pos -> Maybe ((Int, Int), ([(Pos, Char)], Set.Set Pos))
        eat [] _          = Nothing
        eat ((p@(px, py), c):ps) visited =  Just (
                                                    foldr add (0,0) ([ case Map.lookup (px+dx,py+dy) grid of
                                                        Nothing           -> (0,1)
                                                        Just c' | c' == c -> (1,0)
                                                        Just c'           -> (0,1)
                                                    | (dx, dy) <- alldirs
                                                    , (px+dx,py+dy) `Set.notMember` visited
                                                    ]), (ps, Set.insert p visited)
                                                )

        alldirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

bfs :: Grid -> Char -> Pos -> (Int, Int)
bfs grid match (px,py) =
    let r :: (Int, Int) = foldr (\(dx,dy) acc ->
                                    case Map.lookup (px+dx,py+dy) grid of
                                        Nothing -> acc `add` (0, 1)
                                        Just candidate | candidate == match -> acc `add` (1, 0) `add` bfs grid match (px+dx,py+dy)
                                        Just candidate                      -> acc `add` (0, 1) `add` bfs grid candidate (px+dx,py+dy)
                                ) (0,0) alldirs
     in r
    where
        alldirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

part1 :: IO ()
part1 = do
    grid <- parse parser <$> getInput (Example 2) 2024 12
    let start = fromMaybe (error "?") $ Map.lookup (1,1) grid
    putStrLn $ show $ flood grid (1,1)
