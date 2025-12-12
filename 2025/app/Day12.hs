module Day12 where

import           Handy
import           Prelude              hiding (some, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Piece = [XY]

input :: Parser' ([Piece], [(XY, [Int])])
input = (,) <$> some (try piece) <*> some grid -- try because it could be a piece or a grid (both start with num)
    where grid = (,) <$> ((,) <$> (num <* char 'x') <*> (num <* string ": "))
                     <*> (num `sepBy` char ' ' <* newline)
          piece = do
            _id <- num <* char ':' <* newline
            offset <- xy -- get the starting point as an offset, and then grab exactly 3 lines
            join <$> sequenceA [line offset, line offset, line offset] <* newline
          line offset = fmap (`subtractXY` offset) . catMaybes
                        <$> some (choice [ Just <$> xy <* char '#'
                                         , Nothing <$ char '.' ]) <* newline

part1 :: IO Int
part1 = do
    (pieces, trees) <- parse' input <$> puzzle Main 2025 12
    -- trivially reject all the trees which are too small to hold the total number of pieces,
    -- even with perfect compression
    let candidates = filter (\((sizex, sizey), targets) ->
                                sizex * sizey > sum ((\(p, t) -> length p * t) <$> zip pieces targets)
                            ) trees
    pure $ length candidates

part2 :: IO ()
part2 = pure () -- no part2!!
