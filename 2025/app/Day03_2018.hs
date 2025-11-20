{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day03_2018 where

import qualified Data.Map             as Map
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

type Size = (Int, Int)
type Claim = Int

data Swatch = Swatch Claim XY Size deriving (Show)

input :: Parser' [Swatch]
input = some $ Swatch <$> (char '#' *> num <* string " @ ")
                      <*> ((,) <$> num <* char ',' <*> num <* string ": ")
                      <*> ((,) <$> num <* char 'x' <*> num <* optional newline)

xys :: Swatch -> [XY]
xys (Swatch _ (x,y) (w,h)) = [(x',y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2018 3
    let fabric :: Map.Map XY Int
        fabric = foldr (\swatch fab ->
                    foldr (\p -> Map.insertWith (+) p 1) fab $ xys swatch
                 ) Map.empty values
    pure $ length $ Map.filter (>= 2) fabric

part2 :: IO (Either Claim ())
part2 = do
    values <- parse' input <$> puzzle Main 2018 3
    let fabric :: Map.Map XY Int
        fabric = foldr (\swatch fab ->
                    foldr (\p -> Map.insertWith (+) p 1) fab $ xys swatch
                 ) Map.empty values
    -- run through our swatches again, and see which one is entirely claim 1
    pure $ foldM (\_ sw@(Swatch claim _ _) ->
                if all (== 1) $ mapMaybe (`Map.lookup` fabric) (xys sw)
                    then Left claim
                    else Right ()
           ) () values
