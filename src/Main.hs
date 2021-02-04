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

import qualified Data.Map as Map
import Data.Map ( Map )

import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Maybe (maybeToList, mapMaybe, fromMaybe)
import Data.Foldable (Foldable(toList))
import System.Environment (lookupEnv)
import Data.Aeson.Types (parseMaybe)
import Control.Monad.Writer.Lazy

import qualified Telegram.User as TgUser
import qualified Telegram.Message as TgMessage
import qualified Telegram.Chat as TgChat

import qualified Commands
import Control.Monad.Reader
import Control.Monad.State.Strict

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
  | SetRepeatCount
    { userId :: Integer
    , newRepeatCount :: Integer
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

performAction
  Config{} SetRepeatCount{..} = do
    putStrLn $ "Setting repeat count of " <> fromString (show userId) <> " to " <> fromString (show newRepeatCount)

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

applyUpdates :: [TgUpdate] -> App [Action]
applyUpdates updates = do
  actions <- mapM computeActions updates
  return $ concat actions


computeActions :: TgUpdate -> App [Action]
computeActions TgUpdate{updateId, message} = do
  BotState{latestUpdateId} <- get
  actions <- processMessage message
  modify $ \s -> s { latestUpdateId = max (updateId + 1) latestUpdateId }
  return actions

processMessage :: TgMessage.Message -> App [Action]
processMessage (TgMessage.Message _ TgChat.Chat{chatId} replyId (TgMessage.Text text)) = do
  state <- get

  let foo = Commands.executeCommand "/" text

  let rc = fromMaybe 1 $ Map.lookup chatId (repeatCount state)

  return $ case foo of
    Left Commands.NotACommand ->
      replicate (fromInteger rc) (SendMessage chatId text replyId)
      ++
      [ Log $ "Sending " <> text <> " to chat " <> T.pack (show chatId)
      ]
    Left (Commands.CommandNotFound cmd) ->
      [ SendMessage chatId ("⚠️ Command not found: " <> cmd) replyId
      ]
    Left (Commands.DomainError e) ->
      [ SendMessage chatId ("⚠️ " <> e) replyId
      ]
    Right outcomes ->
      outcomes >>= commandOutcomeToAction chatId replyId

processMessage (TgMessage.Message _ TgChat.Chat{chatId} replyId (TgMessage.Sticker fileId)) = do
  return
    [ SendSticker chatId fileId replyId
    , Log $ "Sending a sticker back to chat " <> T.pack (show chatId)
    ]

processMessage (TgMessage.Message author chat _ _) = do
  return
    [ Log $ TgUser.format author <> " sent an unsupported message in " <> T.pack (show chat)
    ]


commandOutcomeToAction :: Integer -> Maybe Integer -> Commands.Outcome -> [Action]
commandOutcomeToAction chatId replyId (Commands.SetRepeatCount n) =
  [ SetRepeatCount{userId=chatId, newRepeatCount=n}
  , SendMessage
    { sendChatId = chatId
    , replyId = Nothing
    , messageText = "Repeat count set to " <> fromString (show n)}
  ]
commandOutcomeToAction chatId replyId (Commands.ShowMessage msg) =
  [ SendMessage
    { sendChatId = chatId
    , replyId = replyId
    , messageText = msg}
  ]


-- Bot state:

data BotState = BotState
  { latestUpdateId :: Integer
  , repeatCount :: Map Integer Integer
  }
  deriving Show

initialBotState :: BotState
initialBotState = BotState
  { latestUpdateId = -100
  , repeatCount = Map.empty
  }


-- Main loop:

pollForever :: App ()
pollForever = do
  config <- ask
  state <- get

  liftIO $ putStrLn $ "Given state" ++ show state
  liftIO $ putStrLn "Getting updates..."

  json <- liftIO $ queryEndpoint $
    buildGetRequest (botToken config) "getUpdates"
    & withParam "limit" "10"
    & withParam "offset" (show $ latestUpdateId state)
    & withParam "timeout" (show $ timeoutSeconds config)

  liftIO $ LBS.putStrLn $ encodePretty json

  let updatesM = do
        updateJsons <- preview (key "result" . _Array) json
        return $ mapMaybe parseUpdate $ toList updateJsons

  liftIO $ print updatesM


  actions <- case updatesM of
        Just updates -> applyUpdates updates
        Nothing -> return []

  liftIO $ performActions config actions

  liftIO $ print actions

  liftIO $ threadDelay 500000
  pollForever


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


type App a = StateT BotState (ReaderT Config IO) a


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

  pollForever `evalStateT` initialBotState `runReaderT` config
