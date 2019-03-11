module Main where

import FetchSportsFeeds
import Models.MySportsFeedsDfsResponse

main :: IO ()
main = fetchGameWeeks 1 >>= print
