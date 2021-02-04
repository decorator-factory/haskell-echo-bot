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
import qualified Telegram.Keyboard as TgKb

import qualified Commands
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Data.Text (splitOn)

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
    , keyboard :: Maybe TgKb.InlineKeyboard
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
  | AnswerCallbackQuery T.Text
  | Log T.Text
    deriving Show

ifPresent :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> [kv]
ifPresent fieldName = maybeToList . fmap (fieldName .=)




performAction ::  Action -> App ()
performAction
  SendMessage{..} = do
    Config{botToken} <- ask
    let json = object $
          [ "chat_id" .= sendChatId
          , "text"    .= messageText
          ]
          ++ ifPresent "reply_to_message_id" replyId
          ++ ifPresent "reply_markup" ((\x -> object ["inline_keyboard" .= x]) <$> keyboard)
    liftIO $ print json
    _ <- liftIO $ queryEndpoint $
      buildPostRequest botToken "sendMessage"
      & setRequestBodyJSON json
    return ()

performAction
  SendSticker{..} = do
    Config{botToken} <- ask
    let json = object $
          [ "chat_id" .= sendChatId
          , "sticker" .= fileId
          ]
          ++ ifPresent "reply_to_message_id" replyId
    _ <- liftIO $ queryEndpoint $
      buildPostRequest botToken "sendSticker"
      & setRequestBodyJSON json
    return ()

performAction
  (Log text) = do
    liftIO $ TIO.putStrLn $ "LOG | " <> text

performAction
  SetRepeatCount{..} = do
    modify $ \s@BotState{repeatCount} -> s{repeatCount=Map.insert userId newRepeatCount repeatCount}

performAction
  (AnswerCallbackQuery cbQueryId) = do
    Config{botToken} <- ask
    _ <- liftIO $ queryEndpoint $
      buildPostRequest botToken "answerCallbackQuery"
      & setRequestBodyJSON (object ["callback_query_id" .= cbQueryId ])
    return ()


performActions :: [Action] -> App ()
performActions = mapM_ performAction


-- Update type:

data TgUpdate = TgUpdate
  { updateId :: Integer
  , content :: UpdateContent
  }
  deriving Show

data UpdateContent
  = Message TgMessage.Message
  | CallbackQuery TgKb.CallbackQuery
  deriving Show


instance FromJSON TgUpdate where
  parseJSON = withObject "TgUpdate" $ \o -> do
    updateId <- o .: "update_id"
    content <- (Message <$> o .: "message") <|> (CallbackQuery <$> o.: "callback_query")
    return TgUpdate{..}

parseUpdate :: Value -> Maybe TgUpdate
parseUpdate = parseMaybe parseJSON


-- Update logic

applyUpdates :: [TgUpdate] -> App [Action]
applyUpdates updates = do
  actions <- mapM computeActions updates
  return $ concat actions


computeActions :: TgUpdate -> App [Action]
computeActions TgUpdate{updateId, content} = do
  BotState{latestUpdateId} <- get
  actions <- processContent content
  modify $ \s -> s { latestUpdateId = max (updateId + 1) latestUpdateId }
  return actions



processContent :: UpdateContent -> App [Action]
processContent (CallbackQuery cb) = processCallbackQuery cb
processContent (Message msg) = processMessage msg


processCallbackQuery :: TgKb.CallbackQuery -> App [Action]
processCallbackQuery TgKb.CallbackQuery{cbId, cbData, cbUser, cbChat} =
  -- Pressing a button simply emulates calling a command
  let
    result = case T.splitOn " " cbData of
      (cmd:args) -> processMessage $
        TgMessage.Message cbUser cbChat Nothing
        (TgMessage.Text $ "/" <> cmd <> " " <> T.intercalate " " args)

      _          -> return [ Log $ "Invalid callback query data: " <> cbData ]
  in
    (AnswerCallbackQuery cbId :) <$> result



processMessage :: TgMessage.Message -> App [Action]
processMessage (TgMessage.Message TgUser.User{userId} TgChat.Chat{chatId} replyId (TgMessage.Text text)) = do
  state <- get

  Config{initialRepeatCount} <- ask
  let rc = fromMaybe initialRepeatCount $ Map.lookup userId (repeatCount state)

  let foo = Commands.executeCommand rc "/" text

  return $ case foo of
    Left Commands.NotACommand ->
      replicate (fromInteger rc) (SendMessage chatId text replyId Nothing)
      ++
      [ Log $ "Sending " <> text <> " to chat " <> T.pack (show chatId)
      ]
    Left (Commands.CommandNotFound cmd) ->
      [ SendMessage chatId ("⚠️ Command not found: " <> cmd) replyId Nothing
      ]
    Left (Commands.DomainError e) ->
      [ SendMessage chatId ("⚠️ " <> e) replyId Nothing
      ]
    Right outcomes ->
      outcomes >>= commandOutcomeToAction chatId userId

processMessage (TgMessage.Message TgUser.User{userId} TgChat.Chat{chatId} replyId (TgMessage.Sticker fileId)) = do

  state <- get
  Config{initialRepeatCount} <- ask
  let rc = fromMaybe initialRepeatCount $ Map.lookup userId (repeatCount state)

  return $
    replicate (fromInteger  rc) (SendSticker chatId fileId replyId)
    ++
    [ Log $ "Sending a sticker back to chat " <> T.pack (show chatId)
    ]

processMessage (TgMessage.Message author chat _ _) = do
  return
    [ Log $ TgUser.format author <> " sent an unsupported message in " <> T.pack (show chat)
    ]


commandOutcomeToAction :: Integer -> Integer -> Commands.Outcome -> [Action]
commandOutcomeToAction chatId userId (Commands.SetRepeatCount n) =
  [ SetRepeatCount{userId, newRepeatCount=n}
  , SendMessage
    { sendChatId = chatId
    , replyId = Nothing
    , messageText = "Repeat count set to " <> fromString (show n)
    , keyboard = Nothing
    }
  ]
commandOutcomeToAction chatId userId (Commands.ShowMessage msg keyboard) =
  [ SendMessage
    { sendChatId = chatId
    , replyId = Nothing
    , messageText = msg
    , keyboard = TgKb.fromCmdKeyboard <$> keyboard
    }
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

  performActions actions

  liftIO $ print actions

  liftIO $ threadDelay 500000
  pollForever


-- Program configuration

data Config = Config
  { botToken :: String
  , timeoutSeconds :: Integer
  , initialRepeatCount :: Integer
  }
  deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    botToken <- o .: "token"
    timeoutSeconds <- o .:? "timeout" .!= 2
    initialRepeatCount <- o .:? "repeat_count" .!= 1
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
