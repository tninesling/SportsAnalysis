{-# LANGUAGE OverloadedStrings #-}

module Models.Player (
  Player,
  pid,
  firstName,
  lastName,
  position
) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Helpers (readAsInt)

data Player = Player { pid :: Int
                     , lastName :: !Text
                     , firstName :: !Text
                     , position :: !Text
                     , dfsSourceId :: Int
                     } deriving (Show)

instance FromJSON Player where
  parseJSON (Object obj) =
    Player <$> fmap readAsInt (obj .: "ID")
           <*> obj .: "LastName"
           <*> obj .: "FirstName"
           <*> obj .: "Position"
           <*> fmap readAsInt (obj .: "dfsSourceId")
  parseJSON _ = mzero

instance ToJSON Player where
  toJSON (Player pid lastName firstName position dfsSourceId) =
    object [ "ID" .= pid
           , "LastName" .= lastName
           , "Firstname" .= firstName
           , "Position" .= position
           , "dfsSourceId" .= dfsSourceId
           ]
