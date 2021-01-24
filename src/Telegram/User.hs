{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.User where

import Data.Aeson
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T

data UserKind = Human | Bot
  deriving (Show, Eq, Ord)

data User = User
  { username :: Maybe T.Text
  , firstName :: T.Text
  , lastName :: Maybe T.Text
  , userId :: Integer
  , userKind :: UserKind
  }
  deriving (Show, Eq)

format :: User -> T.Text
format User{..} = mconcat
  [ T.pack $ show userKind
  , " "
  , fromMaybe "(no username)" username
  , "#" <> T.pack (show userId)
  , " "
  , firstName
  , maybe "" (" " <>) lastName
  ]


instance FromJSON User where
  parseJSON = withObject "TgUser" $ \o -> do
    username <- o .:? "username"
    firstName <- o .: "first_name"
    lastName <- o .:? "last_name"
    userId <- o .: "id"
    isBot <- o .: "is_bot"
    let userKind = if isBot then Bot else Human
    return User{..}
