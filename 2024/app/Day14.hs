{-# LANGUAGE TemplateHaskell #-}
module Day14 where
import           Handy
import           Raylib.Core        (clearBackground, isKeyDown)
import           Raylib.Core.Shapes (drawRectangle)
import           Raylib.Core.Text   (drawText)
import           Raylib.Types       (KeyboardKey (..))
import           Raylib.Util        (drawing, whileWindowOpen_, withWindow)
import           Raylib.Util.Colors (black, rayWhite)
import           Text.Parsec        (char, digit, many1, newline, optionMaybe,
                                     optional, string)

type Pos = (Int, Int)
type Vel = (Int, Int)

parser :: Parser [(Pos, Vel)]
parser = many1 $ (,) <$> pos <*> vel <* optional newline
    where pos = string "p=" *> coord <* char ' '
          vel = string "v=" *> coord <* newline
          coord = (,) <$> val <* char ',' <*> val
          val = do
            negative <- optionMaybe $ char '-'
            v <- read <$> many1 digit
            case negative of
                Nothing -> pure v
                Just _  -> pure $ v * (-1)

step :: (Int, Int) -> Int -> [(Pos, Vel)] -> [(Pos, Vel)]
step (bx,by) n state =
    (\((px,py), vel@(vx,vy)) -> (((px+(vx*n)) `mod` bx, (py+(vy*n)) `mod` by), vel)) <$> state

quadrants :: (Int, Int) -> [Pos] -> [Int]
quadrants (bx, by) ps =
    [ length $ filter (\(x,y) -> x < mx && y < my) ps
    , length $ filter (\(x,y) -> x > mx && y < my) ps
    , length $ filter (\(x,y) -> x < mx && y > my) ps
    , length $ filter (\(x,y) -> x > mx && y > my) ps
    ]
    where (mx, my) = (bx `div` 2, by `div` 2)

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 14
    let bounds = (101,103)
    pure $ product $ quadrants bounds $ fst <$> step bounds 100 input

-- Can't run this in the repl, but rather you should launch it via the main function in Main.hs
part2 :: IO ()
part2 = do
    withWindow 800 600 "Day14 Solution Seeker" 15
        ( \_window -> do
            state <- initialState
            whileWindowOpen_
                ( \(bounds, ps, n) ->
                    drawing
                        ( do
                            forward <- isKeyDown KeyD
                            backward <- isKeyDown KeyA
                            forward' <- isKeyDown KeyE
                            backward' <- isKeyDown KeyQ

                            let n'
                                    | forward = n + 1
                                    | backward = n - 1
                                    | forward' = n + fst bounds
                                    | backward' = n - fst bounds
                                    | otherwise = n

                            let nstep = step bounds n' ps

                            clearBackground rayWhite
                            drawText "Use 'A' and 'D' to search for a trace of a vertical signal" 0 450 24 black
                            drawText "Once locked, then use 'Q' and 'E' to tune" 0 480 24 black
                            drawText ("Answer is -> " <> show n) 0 510 24 black

                            drawPs 4 nstep
                            pure (bounds, ps, n')
                        )
                ) state
        )
    where
        initialState = do
            input <- parse parser <$> getInput Main 2024 14
            let bounds = (101,103)
            pure (bounds, step bounds 0 input, 0)

        drawPs :: Int -> [(Pos, Vel)] -> IO ()
        drawPs _ [] = pure ()
        drawPs scale (((px,py),_):ps) = do
            drawRectangle (px * scale) (py * scale) scale scale black
            drawPs scale ps

