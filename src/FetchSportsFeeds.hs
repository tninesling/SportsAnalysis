{-# LANGUAGE OverloadedStrings #-}

module FetchSportsFeeds (
  fetchDfsFromSportsFeeds,
  fetchGameWeeks
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Format
import Models.DfsRow
import Models.MySportsFeedsDfsResponse
import Network.HTTP.Req
import NflGameWeeks

instance MonadHttp IO where
  handleHttpException = throwIO

fetchGameWeeks :: Integer -> IO [[DfsRow]]
fetchGameWeeks n = traverse fetchGameWeek (getGameWeeks n)

-- fetches all DFS stat rows for a given game week (concatenates the lists from each gameeday in the week)
fetchGameWeek :: [Day] -> IO [DfsRow]
fetchGameWeek gameDays = fmap concat (traverse fetchGameDay gameDays)

fetchGameDay :: Day -> IO [DfsRow]
fetchGameDay gameDay = do
  dfsFeed <- fetchDfsFromSportsFeeds (formatTime defaultTimeLocale "%Y%m%d" gameDay)
  threadDelay 100000 -- delay 10000 microseconds (10 seconds) to avoid API throttling
  return $ firstDfsEntryRows dfsFeed

fetchDfsFromSportsFeeds :: String -> IO MySportsFeedsDfsResponse
fetchDfsFromSportsFeeds dayString = do
  let (url, options) = fromJust (parseUrlHttps $ BSC.concat ["https://api.mysportsfeeds.com/v1.2/pull/nfl/2018-2019-regular/daily_dfs.json?fordate="
                                                            , BSC.pack dayString
                                                            , "&dfstype=draftkings"
                                                            ])
  r <- req GET url NoReqBody jsonResponse $
    basicAuth "ef29e937-e74f-4aa9-99ff-9f928c" "n^wJFE~4YcPE" <>
    options
  return (responseBody r :: MySportsFeedsDfsResponse)
