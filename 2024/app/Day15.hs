module Day15 where
import           Control.Monad   (join)
import           Data.List       (find, intercalate)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)
import           Handy
import           Text.Parsec     (anyChar, char, choice, count, getPosition,
                                  many1, newline, optional, sourceColumn,
                                  sourceLine)

type Pos = (Int, Int)
type Grid = Map.Map Pos Block

data Block = Open | Box | Wall | Robot deriving (Show, Eq)
type Dir = (Int, Int)

parser :: Parser ((Grid, (Int, Int)), [Dir])
parser = (,) <$> (grid <* newline) <*> (join <$> (many1 $ instr <* optional newline))
    where   grid = do
                blocks <- join <$> (many1 $ many1 block <* newline)
                pure (Map.fromList blocks, (maximum $ fst . fst <$> blocks, maximum $ snd . fst <$> blocks))
            block = do
                pos <- (,) <$> (pred <$> sourceColumn <$> getPosition)
                           <*> (pred <$> sourceLine <$> getPosition)
                c <- choice [ Open <$ char '.'
                            , Box <$ char 'O'
                            , Wall <$ char '#'
                            , Robot <$ char '@'
                            ]
                pure (pos, c)
            instr = many1 $ choice  [ ( 0,-1) <$ char '^'
                                    , ( 0, 1) <$ char 'v'
                                    , (-1, 0) <$ char '<'
                                    , ( 1, 0) <$ char '>'
                                    ]

add :: Pos -> Dir -> Pos
add (x,y) (dx,dy) = (x+dx,y+dy)

smoosh :: Grid -> Pos -> Dir -> (Bool, Grid)
smoosh grid pos dir =
    -- can I smoosh the block in that direction?
    let iam = fromMaybe (error "?") $ Map.lookup pos grid
        move grid' =
            -- Move the new block and open the current
            Map.insert pos Open $ Map.insert (pos `add` dir) iam grid'
     in case Map.lookup (pos `add` dir) grid of
            Just Open -> (True, move grid)
            Just Wall -> (False, grid)
            Just Box ->
                -- If it's a box, can i smoosh the box over to make space?
                let (pushed, newgrid) = smoosh grid (pos `add` dir) dir
                    -- if that worked, then we can move ourselves, otherwise we can't
                in if pushed then (True, move newgrid)
                             else (False, grid)
            Just Robot -> error "Found robot?"
            Nothing -> error ("Off the grid? " <> show (pos `add` dir))

sumgps :: Grid -> Int
sumgps grid =
    sum $ (\((x,y), block) -> case block of Box -> x + (y * 100); _ -> 0) <$> Map.toList grid

part1 :: IO Int
part1 = do
    ((grid, bounds), instrs) <- parse parser <$> getInput (Main) 2024 15
    let x = foldl
                (\grid' instr ->
                    let robot = fromMaybe (error "no robot?") $ fst <$> (find (\(pos, block) -> block == Robot) $ Map.toList grid')
                        (_, newgrid) = smoosh grid' robot instr
                     in newgrid
                ) grid instrs
    pure $ sumgps x

printG :: (Grid, (Int, Int)) -> String
printG (grid, (bx, by)) =
    intercalate "\n" $ chunksOf (bx+1) [ ch $ Map.lookup (x,y) grid | y <- [0..by], x <- [0..bx]  ]
    where
        ch (Just Wall)  = '#'
        ch (Just Box)   = 'O'
        ch (Just Open)  = '.'
        ch (Just Robot) = '@'
        ch _            = '?'
