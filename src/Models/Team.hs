{-# LANGUAGE OverloadedStrings #-}

module Models.Team (Team) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Helpers (readAsInt)

data Team = Team { tid :: Int
                 , city :: !Text
                 , name :: !Text
                 , abbreviation :: !Text
                 } deriving (Show)

instance FromJSON Team where
  parseJSON (Object obj) =
    Team <$> fmap readAsInt (obj .: "ID")
         <*> obj .: "City"
         <*> obj .: "Name"
         <*> obj .: "Abbreviation"
  parseJSON _ = mzero

instance ToJSON Team where
  toJSON (Team tid city name abbreviation) =
    object [ "ID" .= tid
           , "City" .= city
           , "Name" .= name
           , "Abbreviation" .= abbreviation
           ]
