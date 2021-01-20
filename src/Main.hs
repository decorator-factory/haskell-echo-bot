{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple ( httpBS, httpJSON, getResponseBody, Request, Query, addToRequestQueryString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ( (&) )
import Data.String ( IsString(..) )

import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String )
import Data.Aeson ( Value )

import Data.Aeson.Encode.Pretty ( encodePretty )

botToken :: String
botToken = "" -- insert bot token here

buildRequestURL :: String -> String -> String -> String
buildRequestURL method token endpoint =
  method ++ " https://api.telegram.org/bot" ++ token ++ "/" ++ endpoint

buildGetRequest :: String -> String -> Request
buildGetRequest token endpoint = fromString $ buildRequestURL "GET" token endpoint

withParam :: BS.ByteString -> BS.ByteString  -> Request -> Request
withParam name value = addToRequestQueryString [(name, Just value)]

queryEndpoint :: Request  -> IO Value
queryEndpoint req = do
  res <- httpJSON req
  return $ getResponseBody res

main :: IO ()
main = do
  -- json1 <- queryEndpoint $ buildRequest botToken "getMe"
  -- LBS.putStrLn  $ encodePretty json1

  let updateRequest =
        buildGetRequest botToken "getUpdates"
        & withParam "limit" "5"
        & withParam "offset" "513423730"
  json2 <- queryEndpoint $ updateRequest
  LBS.putStrLn  $ encodePretty json2
