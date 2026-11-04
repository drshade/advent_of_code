module Day03_2019 where

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

type Wire = [XY]

input :: IO (Wire, Wire)
input = parse' ((,) <$> wire <*> wire <* eof) <$> puzzle Main 2019 3
    where wire = unitify <$> (segment `sepBy` char ',' <* newline)
          segment = (,) <$> dir <*> num
          dir     = ( 1, 0) <$ char 'R'
                <|> (-1, 0) <$ char 'L'
                <|> ( 0,-1) <$ char 'U'
                <|> ( 0, 1) <$ char 'D'
          unitify = concatMap $ (uncurry . flip) replicate -- convert list of [(dir,count)] to [dir,dir,dir] units

part1, part2 :: IO Int
part1 = do
    (wire1, wire2) <- input
    -- scan the list of steps, laying the wire coordinates, then intersection them to find crossings
    let walk wire = Set.fromList $ scanl addXY (0,0) wire
    let crosses = Set.intersection (walk wire1) (walk wire2)
    -- minimum by manhattan distance, dropping origin point
    pure $ minimum $ Set.map (manhattan (0,0)) $ Set.delete (0,0) crosses

part2 = do
    (wire1, wire2) <- input
    -- scan the list of steps, counting the *minimum* number of steps we have taken to get there
    let walk wire = Map.fromListWith min $ zip (scanl addXY (0,0) wire) [0..]
    -- intersect the keys and add together steps
    let crosses = Map.intersectionWith (+) (walk wire1) (walk wire2)
    pure $ minimum $ Map.delete (0,0) crosses
