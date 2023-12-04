module AoC where

import           Data.String.Interpolate    (i)
import           Text.Parsec (Parsec, char, digit, many1, newline, optional,
                              runParser)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             requestHeaders, responseBody,
                                             responseStatus)
import           Control.Monad              (join)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8      as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (unpack)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             requestHeaders, responseBody,
                                             responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (createDirectory,
                                             doesDirectoryExist, doesFileExist)
import           System.IO                  (IOMode (ReadMode), hGetContents,
                                             openFile)

-- Run parser or die!
run_parser :: Parsec String () a -> String -> a
run_parser parser input =
  case runParser parser () "(input)" input of
    Left err -> error $ "A terribly unfortunate parsing error: " ++ (show err)
    Right a  -> a

-- Make parameters nicer
type Year = Int

type Day = Int

data WhichPuzzleInput = Example1 | Example2 | Mine

get_puzzle_input :: WhichPuzzleInput -> Year -> Day -> IO String
get_puzzle_input which_input year day = do
  let local_path = "data/"
      local_file = case which_input of
        Example1 -> [i|input_#{year}_#{day}_example_1|]
        Example2 -> [i|input_#{year}_#{day}_example_2|]
        Mine -> [i|input_#{year}_#{day}|]
      download_url = [i|https://adventofcode.com/#{year}/day/#{day}/input|]
      downloadFile :: IO ()
      downloadFile = do
        putStrLn $
          [i|Downloading input for year #{year} day #{day} (will be cached)|]
        cookie <- readFile "cookie.txt"
        req <- parseRequest download_url
        let req0 = req {requestHeaders = [(hCookie, Char8.pack cookie)]}
        manager <- newManager tlsManagerSettings
        resp <- httpLbs req0 manager
        if statusCode (responseStatus resp) /= 200
          then do
              let body :: String = LChar8.unpack $ responseBody resp
              error $
                [i|Failed to download input for year #{year} day #{day} => #{body}|]
          else do
            let body :: String = LChar8.unpack $ responseBody resp
            writeFile (local_path <> local_file) body
            pure ()
  -- Ensure the directory exists
  _ <-
    do exists <- doesDirectoryExist local_path
       if not exists
         then createDirectory local_path
         else pure ()
  -- Does the file exist?
  _ <-
    do exists <- doesFileExist (local_path <> local_file)
       if not exists
         then downloadFile
         else pure ()
  openFile (local_path <> local_file) ReadMode >>= hGetContents

