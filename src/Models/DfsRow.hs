{-# LANGUAGE OverloadedStrings #-}

module Models.DfsRow (
  DfsRow,
  player,
  salary,
  fantasyPoints
) where

import Data.Aeson
import Helpers (readAsDouble, readAsInt)
import Models.Game
import Models.Player
import Models.Team

data DfsRow = DfsRow { player :: Maybe Player
                     , team :: Team
                     , game :: Game
                     , salary :: Int
                     , fantasyPoints :: Double
                     } deriving (Show)

instance FromJSON DfsRow where
  parseJSON (Object obj) =
    DfsRow <$> obj .:? "player"
           <*> obj .: "team"
           <*> obj .: "game"
           <*> fmap readAsInt (obj .: "salary")
           <*> fmap readAsDouble (obj .:? "fantasyPoints" .!= "0.0")

instance ToJSON DfsRow where
  toJSON (DfsRow player team game salary fantasyPoints) =
    object [ "player" .= player
           , "team" .= team
           , "game" .= game
           , "salary" .= salary
           , "fantasyPoints" .= fantasyPoints
           ]
