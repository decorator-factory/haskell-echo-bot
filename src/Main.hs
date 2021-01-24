{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Network.HTTP.Simple (setRequestBodyJSON, httpJSON, getResponseBody, Request, addToRequestQueryString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ( (&) )
import Data.String ( IsString(..) )

import Control.Concurrent (threadDelay)

import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _Array )
import Data.Aeson hiding ( json )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Maybe (maybeToList, mapMaybe)
import Data.Foldable (Foldable(toList))
import System.Environment (lookupEnv)
import Data.Aeson.Types (parseMaybe)
import Control.Monad.Writer.Lazy

import qualified Telegram.User as TgUser
import qualified Telegram.Message as TgMessage
import qualified Telegram.Chat as TgChat


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


-- Actions:

data Action
  = SendMessage
    { sendChatId :: Integer
    , messageText :: T.Text
    , replyId :: Maybe Integer
    }
  | SendSticker
    { sendChatId :: Integer
    , fileId :: T.Text
    , replyId :: Maybe Integer
    }
  | Log T.Text
    deriving Show

ifPresent :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> [kv]
ifPresent fieldName = maybeToList . fmap (fieldName .=)

performAction :: Config -> Action -> IO ()
performAction
  Config{botToken} SendMessage{..} = do
    let json = object $
          [ "chat_id" .= sendChatId
          , "text"    .= messageText
          ]
          ++ ifPresent "reply_to_message_id" replyId
    _ <- queryEndpoint $
      buildPostRequest botToken "sendMessage"
      & setRequestBodyJSON json
    return ()

performAction
  Config{botToken} SendSticker{..} = do
    let json = object $
          [ "chat_id" .= sendChatId
          , "sticker" .= fileId
          ]
          ++ ifPresent "reply_to_message_id" replyId
    _ <- queryEndpoint $
      buildPostRequest botToken "sendSticker"
      & setRequestBodyJSON json
    return ()

performAction
  Config{} (Log text) = do
    TIO.putStrLn $ "LOG | " <> text

performActions :: Config -> [Action] -> IO ()
performActions config = mapM_ (performAction config)


-- Update type:

data TgUpdate = TgUpdate
  { updateId :: Integer
  , message :: TgMessage.Message
  }
  deriving Show

instance FromJSON TgUpdate where
  parseJSON = withObject "TgUpdate" $ \o -> do
    updateId <- o .: "update_id"
    message <- o .: "message"
    return TgUpdate{..}

parseUpdate :: Value -> Maybe TgUpdate
parseUpdate = parseMaybe parseJSON


-- Update logic

applyUpdates :: [TgUpdate] -> BotState -> ([Action], BotState)
applyUpdates updates state = foldr applyUpdate ([], state) updates

applyUpdate :: TgUpdate -> ([Action], BotState) -> ([Action], BotState)
applyUpdate update (actions, state) =
  let (newActions, newState) =  computeActions update state
  in (actions ++ newActions, newState)

computeActions :: TgUpdate -> BotState -> ([Action], BotState)
computeActions TgUpdate{updateId, message} state@BotState{latestUpdateId} = do
    let (actions, newState) = processMessage message state
    tell actions
    return $ newState { latestUpdateId = max (updateId + 1) latestUpdateId }

processMessage :: TgMessage.Message  -> BotState -> ([Action], BotState)
processMessage (TgMessage.Message _ TgChat.Chat{chatId} replyId (TgMessage.Text text)) state = do
  tell
    [ SendMessage chatId text replyId
    , Log $ "Sending " <> text <> " to chat " <> T.pack (show chatId)
    ]
  return state

processMessage (TgMessage.Message _ TgChat.Chat{chatId} replyId (TgMessage.Sticker fileId)) state = do
  tell
    [ SendSticker chatId fileId replyId
    , Log $ "Sending a sticker back to chat " <> T.pack (show chatId)
    ]
  return state

processMessage (TgMessage.Message author chat _ _) state = do
  tell
    [ Log $ TgUser.format author <> " sent an unsupported message in " <> T.pack (show chat)
    ]
  return state


-- Bot state:

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
  putStrLn "Getting updates..."

  json <- queryEndpoint $
    buildGetRequest (botToken config) "getUpdates"
    & withParam "limit" "10"
    & withParam "offset" (show $ latestUpdateId state)
    & withParam "timeout" (show $ timeoutSeconds config)

  LBS.putStrLn $ encodePretty json

  let updatesM = do
        updateJsons <- preview (key "result" . _Array) json
        return $ mapMaybe parseUpdate $ toList updateJsons

  print updatesM

  let (actions, newState) = case updatesM of
        Just updates -> applyUpdates updates state
        Nothing -> ([], state)

  performActions config actions

  print actions

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
