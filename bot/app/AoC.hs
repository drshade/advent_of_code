{-# LANGUAGE OverloadedStrings #-}

module AoC where

import           AoCTypes
import           Control.Lens
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Char8      as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (pack, unpack)
import           Data.List                  (sort, (\\))
import           Data.Map                   (assocs, keys)
import           Data.Maybe                 (fromMaybe)
import           Data.String.Interpolate    (i)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             requestHeaders, responseBody,
                                             responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (statusCode)

import           Prelude                    hiding (id)
import           Utils                      (stripNewlines)

data Update
    = JoinedLeaderboard Member
    | LeftLeaderboard Member
    | EarnedStar Member Day Part

memberName :: Member -> String
memberName m = m ^. name . non ("Anonymous User (#" <> (show $ m ^. id) <> ")")

instance Show Update where
    show :: Update -> String
    show (JoinedLeaderboard m) = [i|#{m & memberName} joined the leaderboard|]
    show (LeftLeaderboard m) = [i|#{m & memberName} left the leaderboard|]
    show (EarnedStar m d 1) = [i|#{m & memberName} unlocked ★ for day #{d}!|]
    show (EarnedStar m d 2) = [i|#{m & memberName} unlocked ⭐ for day #{d}!|]
    show (EarnedStar m _ _) = [i|#{m & memberName} strangely earned a non-existent star??|]

getLeaverJoinerUpdates :: Leaderboard -> Leaderboard -> [Update]
getLeaverJoinerUpdates old new =
    -- Get lists of members in both leaderboards and then difference them
    let oldMembers = old ^. members & keys & sort
        newMembers = new ^. members & keys & sort
        idsJoined = newMembers \\ oldMembers
        idsLeft = oldMembers \\ newMembers
        joiners = JoinedLeaderboard <$> (idsJoined <&> \id' -> new ^. members . at id' . non (error "member not found"))
        leavers = LeftLeaderboard <$> (idsLeft <&> \id' -> old ^. members . at id' . non (error "member not found"))
     in leavers ++ joiners

getEarnedStarUpdates :: Leaderboard -> Leaderboard -> [Update]
getEarnedStarUpdates old new =
    let oldMembers = old ^. members
        newMembers = new ^. members
        updates = do
            (id', newMember) <- newMembers & assocs
            let oldMember = oldMembers ^. at id'
            (day, dayCompletion) <- newMember ^. completion_day_level & assocs
            (part, partResult) <- dayCompletion & assocs
            let oldPartResult = oldMember >>= \m -> m ^. completion_day_level . at day >>= \d -> d ^. at part
            if oldPartResult /= Just partResult
            then pure $ EarnedStar newMember day part
            else []
     in updates

getUpdates :: Leaderboard -> Leaderboard -> [Update]
getUpdates old new = getLeaverJoinerUpdates old new ++ getEarnedStarUpdates old new


getPrivateLeaderboard :: Year -> LeaderboardId -> IO String
getPrivateLeaderboard year boardid = do
    let download_url = [i|https://adventofcode.com/#{year}/leaderboard/private/view/#{boardid}.json|]
    putStrLn
        [i|Downloading leaderboard for year #{year}|]
    cookie <- readFile "cookie.txt"
    req <- parseRequest download_url
    let req0 = req{requestHeaders = [(hCookie, Char8.pack (stripNewlines cookie))]}
    manager <- newManager tlsManagerSettings
    resp <- httpLbs req0 manager
    if statusCode (responseStatus resp) /= 200
    then do
        let body :: String = LChar8.unpack $ responseBody resp
        error
            [i|Failed to download leaderboard #{boardid} for year #{year} => #{body}|]
    else pure $ LChar8.unpack $ responseBody resp

parseJson :: String -> Leaderboard
parseJson contents = fromMaybe (error $ "Unable to decode leaderboard: \n" <> contents) (decode $ LChar8.pack $ contents)

getTemp1 :: IO String
getTemp1 = readFile "temp1.json"

getTemp2 :: IO String
getTemp2 = readFile "temp2.json"
