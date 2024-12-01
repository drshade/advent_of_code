module Main where

import           AoC
import           AoCTypes
import           Control.Concurrent (threadDelay)
import           Control.Exception  (catch)
import           Data.Map           (empty)
import           DiscordWebhook

ownerId :: Int
ownerId = 2306561

year :: Int
year = 2024

main :: IO ()
main = do
    current <- catch (parseJson <$> readFile "current.json") (\(_ :: IOError) -> pure $ Leaderboard ownerId (show year) empty)
    newestRaw <- getPrivateLeaderboard year ownerId
    let newest = parseJson $ newestRaw

    let updates = getUpdates current newest

    mapM_
        (\update -> do
            send $ show update
            threadDelay 1000
            pure ()
        ) updates

    writeFile "current.json" newestRaw
