{-# LANGUAGE DeriveGeneric #-}

module Models.MySportsFeedsDfsResponse (
  DfsRow,
  MySportsFeedsDfsResponse,
  firstDfsEntry,
  firstDfsEntryRows,
  firstDfsRow
) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Models.DfsRow


newtype MySportsFeedsDfsResponse =
  MySportsFeedsDfsResponse { dailydfs :: DailyDfs } deriving (Show, Generic)
instance FromJSON MySportsFeedsDfsResponse
instance ToJSON MySportsFeedsDfsResponse


data DailyDfs = DailyDfs { lastUpdatedOn :: !Text
                         , dfsEntries :: [DfsEntry]
                         } deriving (Show, Generic)
instance FromJSON DailyDfs
instance ToJSON DailyDfs


data DfsEntry = DfsEntry { dfsType :: !Text
                         , dfsRows :: [DfsRow]
                         } deriving (Show, Generic)
instance FromJSON DfsEntry
instance ToJSON DfsEntry


firstDfsEntry :: MySportsFeedsDfsResponse -> DfsEntry
firstDfsEntry = head . dfsEntries . dailydfs

firstDfsEntryRows :: MySportsFeedsDfsResponse -> [DfsRow]
firstDfsEntryRows = dfsRows . firstDfsEntry

firstDfsRow :: MySportsFeedsDfsResponse -> DfsRow
firstDfsRow = head . firstDfsEntryRows
