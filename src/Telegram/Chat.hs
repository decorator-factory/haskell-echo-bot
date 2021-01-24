{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Chat where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (parseFail, Parser)


data ChatInfo
  = PrivateChat
    { username :: Maybe T.Text
    , firstName :: T.Text
    , lastName :: Maybe T.Text
    }
  | Group
    { title :: T.Text
    }
  deriving Show

parseTgChatInfo :: Object -> Parser ChatInfo
parseTgChatInfo o = do
  chatType <- o .: "type"
  case chatType of
    "group" -> parseGroupInfo o
    "supergroup" -> parseGroupInfo o
    "private" -> parsePrivateChatInfo o
    _ -> parseFail $ "Unsupported chat type: " ++ chatType
  where
    parsePrivateChatInfo o' = do
      username <- o' .:? "username"
      firstName <- o' .: "first_name"
      lastName <- o' .:? "last_name"
      return PrivateChat{..}

    parseGroupInfo o' = do
      title <- o' .: "title"
      return Group{..}


data Chat = Chat
  { chatId :: Integer
  , chatInfo :: ChatInfo
  }
  deriving Show

instance FromJSON Chat where
  parseJSON = withObject "TgChat" $ \o -> do
    chatId <- o .: "id"
    chatInfo <- parseTgChatInfo o
    return Chat{..}
