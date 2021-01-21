{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.HTTP.Simple (httpJSON, getResponseBody, Request, Query, addToRequestQueryString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ( (&) )
import Data.String ( IsString(..) )

import Control.Concurrent (threadDelay)

import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String, _Integer, _Array )
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Maybe (mapMaybe)
import Data.Foldable (Foldable(toList))
import System.Environment (lookupEnv)
import System.IO (hGetContents, IOMode(ReadMode), withFile)
import Data.Aeson.Types (parseMaybe)


-- Request building stuff

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


-- Telegram updates stuff:

data TgUpdate = TgUpdate
  { updateId :: Integer
  , message :: TgMessage
  }
  deriving Show


data TgMessage = TgMessage
  { author :: TgUser
  , text :: Maybe T.Text
  }
  deriving (Show, Eq)


data TgUserKind = Human | Bot
  deriving (Show, Eq, Ord)


data TgUser = TgUser
  { username :: Maybe T.Text
  , firstName :: T.Text
  , lastName :: Maybe T.Text
  , userId :: Integer
  , kind :: TgUserKind
  }
  deriving (Show, Eq)


instance FromJSON TgUser where
  parseJSON = withObject "TgUser" $ \o -> do
    username <- o .:? "username"
    firstName <- o .: "first_name"
    lastName <- o .:? "last_name"
    userId <- o .: "id"
    isBot <- o .: "is_bot"
    let kind = if isBot then Bot else Human
    return TgUser{..}


instance FromJSON TgMessage where
  parseJSON = withObject "TgMessage" $ \o -> do
    author <- o .: "from"
    text <- o .:? "text"
    return TgMessage{..}


instance FromJSON TgUpdate where
  parseJSON = withObject "TgUpdate" $ \o -> do
    updateId <- o .: "update_id"
    message <- o .: "message"
    return TgUpdate{..}


applyUpdates :: [TgUpdate] -> BotState -> BotState
applyUpdates updates state = foldr applyUpdate state updates


applyUpdates2 :: [TgUpdate] -> BotState -> ([Action], BotState)
applyUpdates2 updates state = foldr applyUpdate2 ([], state) updates


applyUpdate2 :: TgUpdate -> ([Action], BotState) -> ([Action], BotState)
applyUpdate2
  TgUpdate{updateId, message=TgMessage{text, author=TgUser{userId}}}
  (actions, state@BotState{latestUpdateId}) =
    let newState = state { latestUpdateId = max (updateId + 1) latestUpdateId }
        newActions = case text of
            Just text -> [ SendMessage userId text ]
            Nothing -> []
    in (newActions ++ actions, newState)


applyUpdate :: TgUpdate -> BotState -> BotState
applyUpdate TgUpdate{updateId} state@BotState{latestUpdateId} =
  state
    { latestUpdateId = max (updateId + 1) latestUpdateId
    }

parseUpdate :: Value -> Maybe TgUpdate
parseUpdate = parseMaybe parseJSON


-- Bot state stuff:

data Action
  = SendMessage { sendChatId :: Integer, messageToSend :: T.Text }


performAction :: Config -> Action -> IO ()
performAction
  Config{botToken}
  SendMessage{sendChatId, messageToSend} = do
    let req =
          buildGetRequest botToken "sendMessage"
          & withParam "chat_id" (show sendChatId)
          & withParam "text" (T.unpack messageToSend)
    queryEndpoint req
    return ()


performActions :: Config -> [Action] -> IO ()
performActions config = mapM_ (performAction config)


newtype BotState = BotState
  { latestUpdateId :: Integer
  }
  deriving Show


initialBotState :: BotState
initialBotState = BotState
  { latestUpdateId = -100
  }


-- Main loop:

pollForever :: Config -> BotState -> IO ()
pollForever config state = do

  putStrLn $ "Given state" ++ show state

  let updateRequest =
        buildPostRequest (botToken config) "getUpdates"
        & withParam "limit" "10"
        & withParam "offset" (show $ latestUpdateId state)
        & withParam "timeout" (show $ timeoutSeconds config)

  putStrLn "Getting updates..."

  json <- queryEndpoint updateRequest

  LBS.putStrLn $ encodePretty json

  let updatesM = do
        updateJsons <- preview (key "result" . _Array) json
        return $ mapMaybe parseUpdate $ toList updateJsons

  let (actions, newState) = case updatesM of
        Just updates -> applyUpdates2 updates state
        Nothing -> ([], state)

  performActions config actions

  threadDelay 500000
  pollForever config newState


-- Program configuration

data Config = Config
  { botToken :: String
  , timeoutSeconds :: Integer
  }
  deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    botToken <- o .: "token"
    timeoutSeconds <- o .:? "timeout" .!= 2
    return Config{..}


-- entry point

main :: IO ()
main = do
  configPathM <- lookupEnv "CONFIG_PATH"

  configPath <- case configPathM of
    Just configPath -> return configPath
    _               -> fail "Environment variable CONFIG_PATH not found"

  print configPath

  configE <- eitherDecodeFileStrict configPath

  config :: Config <- case configE of
    Left e -> fail e
    Right config -> return config

  print config

  pollForever config initialBotState
