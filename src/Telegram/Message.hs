{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Message where

import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Types (Parser)

import Control.Applicative ( (<|>) )
import Telegram.User as User
import qualified Telegram.Chat as Chat


data MessageContent
  = Text T.Text
  | Sticker { fileId :: T.Text }
  | Other
  deriving Show

parseMessageContent :: Object -> Parser MessageContent
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

data Message = Message
  { author :: User.User
  , chat :: Chat.Chat
  , replyId :: Maybe Integer
  , content :: MessageContent
  }
  deriving Show

instance FromJSON Message where
  parseJSON = withObject "TgMessage" $ \o -> do
    author <- o .: "from"
    content <- parseMessageContent o
    chat <- o .: "chat"
    replyId <- (o .:? "reply_to_message") >>= maybe (pure Nothing) (.:? "message_id")
    return Message{..}
