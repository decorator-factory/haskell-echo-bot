{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Keyboard where

import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Types (Parser)

import qualified Telegram.User as TgUser
import qualified Telegram.Chat as TgChat
import qualified Commands

data CallbackQuery = CallbackQuery
  { cbId :: T.Text
  , cbData :: T.Text
  , cbUser :: TgUser.User
  , cbChat :: TgChat.Chat
  }
  deriving Show

instance FromJSON CallbackQuery where
  parseJSON = withObject "TgCallbackQuery" $ \o -> do
    cbId <- o .: "id"
    cbData <- o .: "data"
    m <- o .: "message"
    cbChat <- m .: "chat"
    cbUser <- o .: "from"
    return CallbackQuery{..}

fromCmdKeyboard :: Commands.Keyboard -> InlineKeyboard
fromCmdKeyboard (Commands.Keyboard source entries) =
  InlineKeyboard source (uncurry InlineKeyboardButton <$> entries)

data InlineKeyboard = InlineKeyboard
  { kbSource :: T.Text
  , kbButtons :: [InlineKeyboardButton]
  }
  deriving Show

data InlineKeyboardButton = InlineKeyboardButton
  { kbLabel :: T.Text
  , kbCallbackData :: T.Text
  }
  deriving Show

serializeBtn :: T.Text -> InlineKeyboardButton -> Value
serializeBtn src InlineKeyboardButton{..} =
  object
  [ "text" .= kbLabel
  , "callback_data" .= (src <> " " <> kbCallbackData)
  ]

instance ToJSON InlineKeyboard where
  toJSON InlineKeyboard{..} =
    toJSON [ serializeBtn kbSource <$> kbButtons ]
