
{-# LANGUAGE OverloadedStrings #-}

module DiscordWebhook where

import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Utils                      (stripNewlines)

send :: String -> IO ()
send content = do
    url <- readFile "webhook.txt"
    manager <- newManager tlsManagerSettings
    let message = object ["content" .= content]
        request = (parseRequest_ (stripNewlines url))
                    { method = "POST"
                    , requestBody = RequestBodyLBS (encode message)
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
    response <- httpLbs request manager
    putStrLn $ "Discord Webhook Response: " ++ show (responseStatus response)
    L8.putStrLn $ responseBody response
