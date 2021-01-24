{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Network.HTTP.Simple (setRequestBodyJSON, httpJSON, getResponseBody, Request, Query, addToRequestQueryString)
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
import Data.Maybe (maybeToList, fromMaybe, catMaybes, mapMaybe)
import Data.Foldable (Foldable(toList))
import System.Environment (lookupEnv)
import System.IO (hGetContents, IOMode(ReadMode), withFile)
import Data.Aeson.Types (parseFail, Parser, parseMaybe)
import Control.Monad.Writer.Lazy
import Control.Applicative (Alternative((<|>)))


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



-- Message content type

data TgMessageContent
  = Text T.Text
  | Sticker { fileId :: T.Text }
  | Other
  deriving Show

parseMessageContent :: Object -> Parser TgMessageContent
parseMessageContent o = parseTextContent o <|> parseStickerContent o <|> parseOtherContent o where
  parseTextContent o = do
    text <- o .: "text"
    return $ Text text

  parseStickerContent o = do
    sticker <- o .: "sticker"
    fileId <- sticker .: "file_id"
    return $ Sticker{..}

  parseOtherContent _ = pure Other


-- Message type

data TgMessage = TgMessage
  { author :: TgUser
  , chat :: TgChat
  , replyId :: Maybe Integer
  , content :: TgMessageContent
  }
  deriving Show

instance FromJSON TgMessage where
  parseJSON = withObject "TgMessage" $ \o -> do
    author <- o .: "from"
    content <- parseMessageContent o
    chat <- o .: "chat"
    replyId <- (o .:? "reply_to_message") >>= maybe (pure Nothing) (.:? "message_id")
    return TgMessage{..}


-- User type:

data TgUserKind = Human | Bot
  deriving (Show, Eq, Ord)

data TgUser = TgUser
  { username :: Maybe T.Text
  , userFirstName :: T.Text
  , userLastName :: Maybe T.Text
  , userId :: Integer
  , userKind :: TgUserKind
  }
  deriving (Show, Eq)

formatUser :: TgUser -> T.Text
formatUser TgUser{..} = mconcat
  [ T.pack $ show userKind
  , " "
  , fromMaybe "(no username)" username
  , "#" <> T.pack (show userId)
  , " "
  , userFirstName
  , maybe "" (" " <>) userLastName
  ]


instance FromJSON TgUser where
  parseJSON = withObject "TgUser" $ \o -> do
    username <- o .:? "username"
    userFirstName <- o .: "first_name"
    userLastName <- o .:? "last_name"
    userId <- o .: "id"
    isBot <- o .: "is_bot"
    let userKind = if isBot then Bot else Human
    return TgUser{..}


-- Chat info type:

data TgChatInfo
  = TgPrivateChat
    { username :: Maybe T.Text
    , firstName :: T.Text
    , lastName :: Maybe T.Text
    }
  | TgGroup
    { title :: T.Text
    }
  deriving Show

parseTgChatInfo :: Object -> Parser TgChatInfo
parseTgChatInfo o = do
  chatType :: String <- o .: "type"
  case chatType of
    "group" -> parseGroupInfo o
    "supergroup" -> parseGroupInfo o
    "private" -> parsePrivateChatInfo o
    _ -> parseFail $ "Unsupported chat type: " ++ chatType
  where
    parsePrivateChatInfo o = do
      username <- o .:? "username"
      firstName <- o .: "first_name"
      lastName <- o .:? "last_name"
      return TgPrivateChat{..}

    parseGroupInfo o = do
      title <- o .: "title"
      return TgGroup{..}


-- Chat type:

data TgChat = TgChat
  { chatId :: Integer
  , chatInfo :: TgChatInfo
  }
  deriving Show

instance FromJSON TgChat where
  parseJSON = withObject "TgChat" $ \o -> do
    chatId <- o .: "id"
    chatInfo <- parseTgChatInfo o
    return TgChat{..}


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
    queryEndpoint $
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
    queryEndpoint $
      buildPostRequest botToken "sendSticker"
      & setRequestBodyJSON json
    return ()

performAction
  Config{botToken} (Log text) = do
    TIO.putStrLn $ "LOG | " <> text

performActions :: Config -> [Action] -> IO ()
performActions config = mapM_ (performAction config)


-- Update type:

data TgUpdate = TgUpdate
  { updateId :: Integer
  , message :: TgMessage
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

processMessage :: TgMessage -> BotState -> ([Action], BotState)
processMessage (TgMessage _ TgChat{chatId} replyId (Text text)) state = do
  tell
    [ SendMessage chatId text replyId
    , Log $ "Sending " <> text <> " to chat " <> T.pack (show chatId)
    ]
  return state

processMessage (TgMessage _ TgChat{chatId} replyId (Sticker fileId)) state = do
  tell
    [ SendSticker chatId fileId replyId
    , Log $ "Sending a sticker back to chat " <> T.pack (show chatId)
    ]
  return state

processMessage (TgMessage author chat _ _) state = do
  tell
    [ Log $ formatUser author <> " sent an unsupported message in " <> T.pack (show chat)
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
