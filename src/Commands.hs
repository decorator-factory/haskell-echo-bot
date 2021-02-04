{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Commands
  ( executeCommand
  , Outcome (..)
  , Error (..)
  , Keyboard (..)
  )  where

import qualified Data.Text as T
import Text.Read ( readMaybe )

import Data.List ( find )


data Keyboard = Keyboard
  { source :: T.Text
  , entries :: [(T.Text, T.Text)]
  } deriving Show


data Outcome
    = SetRepeatCount Integer
    | ShowMessage T.Text (Maybe Keyboard)
    deriving Show

data Error
    = NotACommand
    | CommandNotFound T.Text
    | DomainError T.Text
    deriving Show


type Handler ctx = ctx -> [T.Text] -> Either T.Text [Outcome]


data Definition ctx = Definition
  { name :: T.Text
  , handler :: Handler ctx
  }


data ParseResult = ParseResult
  { name :: T.Text
  , args :: [T.Text]
  }


parseCommand :: T.Text -> T.Text -> Maybe ParseResult
parseCommand prefix text =
  case T.words <$> T.stripPrefix prefix text of
    Just (name:args) -> Just ParseResult{..}
    _                -> Nothing


helpHandler :: Handler ctx
helpHandler ctx [] = Right [ShowMessage "Help!!!" Nothing]
helpHandler ctx _  = Left "Expected 0 arguments"


repeatHandler :: Handler Integer
repeatHandler repeats [ns] = case readMaybe $ T.unpack ns of
  Nothing -> Left "Not a valid integer"
  Just n  -> if n < 1 || n > 5 then Left "Integer not in range 1..5" else Right [SetRepeatCount n]
repeatHandler repeats [] = Right
  [ ShowMessage
      ("Your current repeat count is "
        <> T.pack (show repeats)
        <> ". Choose the new repeat count")
      (Just $ Keyboard "repeat" [("1", "1"), ("2", "2"), ("3", "3"), ("4", "4"), ("5", "5")])
  ]
repeatHandler repeats _ = Left "Expected 1 argument"


maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing  = Left b


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right b) = Right b
mapLeft f (Left a)  = Left (f a)


executeCommand' :: [Definition ctx] -> ctx -> T.Text -> T.Text -> Either Error [Outcome]
executeCommand' definitions ctx prefix input = do
  ParseResult{name, args} <- maybeToEither NotACommand $ parseCommand prefix input
  Definition{handler} <- maybeToEither (CommandNotFound name) $ find (\Definition{name=name'} -> name' == name) definitions
  mapLeft DomainError $ handler ctx args


executeCommand :: Integer -> T.Text -> T.Text -> Either Error [Outcome]
executeCommand = executeCommand' defaultDefinitions


defaultDefinitions :: [Definition Integer]
defaultDefinitions = uncurry Definition <$>
  [ ("help",   helpHandler)
  , ("repeat", repeatHandler)
  ]
