{-# LANGUAGE OverloadedStrings #-}

module Models.Game (Game) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Helpers (readAsInt)
import Models.Team

data Game = Game { gid :: Int
                 , date :: !Text
                 , time :: !Text
                 , awayTeam :: Team
                 , homeTeam :: Team
                 } deriving (Show)

instance FromJSON Game where
  parseJSON (Object obj) =
    Game <$> fmap readAsInt (obj .: "id")
         <*> obj .: "date"
         <*> obj .: "time"
         <*> obj .: "awayTeam"
         <*> obj .: "homeTeam"
  parseJSON _ = mzero

instance ToJSON Game where
  toJSON (Game gid date time awayTeam homeTeam) =
   object [ "id" .= gid
          , "date" .= date
          , "time" .= time
          , "awayTeam" .= awayTeam
          , "homeTeam" .= homeTeam
          ]
