module NflGameWeeks (
  getGameWeeks
) where

import Data.Maybe (fromJust)
import Data.Time.Calendar
import Data.Time.Format

getGameWeeks :: Integer -> [[Day]]
getGameWeeks 0 = []
getGameWeeks n = gameWeek : getGameWeeks (n - 1) where
  gameWeek = fmap (addDays (7 * n) . fromJust) week1Days -- add n weeks to correctly parsed days
  week1Days = fmap parseDay week1DayStrings
  -- September 6th to 9th is the first Thursday - Sunday gameweek
  week1DayStrings = [ "2018-09-06" ] {-
                    , "2018-09-07"
                    , "2018-09-08"
                    , "2018-09-09"
                    ] -}

parseDay :: String -> Maybe Day
parseDay dayString = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" dayString :: Maybe Day
