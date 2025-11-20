module Play where

play :: Int
play = do
    let x = runCont (foldM
                (\e acc -> cont $ \k ->
                    if even e then k e
                              else e
                ) 1 [1..10]) id
     in x

