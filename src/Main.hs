{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.HTTP.Simple ( httpBS, httpJSON, getResponseBody, Request, Query, addToRequestQueryString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ( (&) )
import Data.String ( IsString(..) )

import Control.Concurrent (threadDelay)

import Control.Lens (Ixed(ix),  preview )
import Data.Aeson.Lens ( key, _String, _Integer, _Array )
import Data.Aeson ( Value )
import Text.Read ( readMaybe )
import qualified Data.Text as T

import Data.Aeson.Encode.Pretty ( encodePretty )
import Control.Monad (when)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Foldable (Foldable(toList))


botToken :: String
botToken = "" -- insert bot token here


buildRequestURL :: String -> String -> String -> String
buildRequestURL method token endpoint =
  method ++ " https://api.telegram.org/bot" ++ token ++ "/" ++ endpoint


buildGetRequest :: String -> String -> Request
buildGetRequest token endpoint = fromString $ buildRequestURL "GET" token endpoint


buildPostRequest :: String -> String -> Request
buildPostRequest token endpoint = fromString $ buildRequestURL "POST" token endpoint


withParam :: String -> String -> Request -> Request
withParam name value = addToRequestQueryString [(BS.pack name, Just $ BS.pack value)]


queryEndpoint :: Request  -> IO Value
queryEndpoint req = do
  res <- httpJSON req
  return $ getResponseBody res


data TgUpdate = TgUpdate
  { updateId :: Integer
  , message :: T.Text
  , author :: T.Text
  }
  deriving Show


parseTgUpdate :: Value -> Maybe TgUpdate
parseTgUpdate value = do
  updateId <- preview (key "update_id" . _Integer) value
  message <- preview (key "message" . key "text" . _String) value
  author <- preview (key "message" . key "from" . key "username" . _String) value
  return TgUpdate { updateId, message, author }


applyUpdates :: [TgUpdate] -> BotState -> BotState
applyUpdates updates state = foldr applyUpdate state updates


applyUpdate :: TgUpdate -> BotState -> BotState
applyUpdate TgUpdate{updateId} state@BotState{latestUpdateId} =
  state
    { latestUpdateId = max updateId (latestUpdateId + 1)
    }


pollForever :: Int -> BotState -> IO ()
pollForever timeout state = do

  putStrLn $ "Given state" ++ show state

  let updateRequest =
        buildPostRequest botToken "getUpdates"
        & withParam "limit" "10"
        & withParam "offset" (show $ latestUpdateId state)
        & withParam "timeout" (show timeout)

  putStrLn "Getting updates..."

  json <- queryEndpoint updateRequest

  let updatesM = do
      updateJsons <- preview (key "result" . _Array) json
      return $ mapMaybe parseTgUpdate $ toList updateJsons

  let newState = case updatesM of
        Just updates -> applyUpdates updates state
        Nothing -> state
  print updatesM

  threadDelay 500000
  pollForever timeout newState


newtype BotState = BotState
  { latestUpdateId :: Integer
  }
  deriving Show


initialBotState :: BotState
initialBotState = BotState
  { latestUpdateId = -100
  }


main :: IO ()
main = pollForever 2 initialBotState
