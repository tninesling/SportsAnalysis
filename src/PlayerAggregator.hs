module PlayerAggregator (
  ScoredPlayer,
  aggregatePlayers,
  averageScore,
  scoreVariance
) where

import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Models.DfsRow as D
import qualified Models.Player as P

data ScoredPlayer = ScoredPlayer { pid :: Int
                                 , firstName :: !Text
                                 , lastName :: !Text
                                 , position :: !Text
                                 , salary :: Int
                                 , averageScore :: Double
                                 , scoreVariance :: Double
                                 }

aggregatePlayers :: [D.DfsRow] -> [ScoredPlayer]
aggregatePlayers dfsRows = fmap aggregatePlayerList groupedByPid where
  groupedByPid = groupBy equalPids dfsRowsWithPlayers
  dfsRowsWithPlayers = filter hasPlayer dfsRows

aggregatePlayerList :: [D.DfsRow] -> ScoredPlayer
aggregatePlayerList dfsRows = createScoredPlayer actualPlayer playerSalary avgPoints pointVariance where
  actualPlayer = fromJust $ D.player (head dfsRows)
  playerSalary = D.salary (head dfsRows)
  numPlayers = fromIntegral $ length dfsRows
  pointVector = fmap D.fantasyPoints dfsRows
  avgPoints = sum pointVector / numPlayers
  differenceVector = fmap (\x -> x - avgPoints) pointVector
  pointVariance = (differenceVector `dot` differenceVector) / numPlayers

equalPids :: D.DfsRow -> D.DfsRow -> Bool
equalPids row1 row2 =
  case (D.player row1, D.player row2) of
    (Just p1, Just p2) -> P.pid p1 == P.pid p2
    _ -> False

hasPlayer :: D.DfsRow -> Bool
hasPlayer dfsRow =
  case D.player dfsRow of
    Just p -> True
    Nothing -> False

createScoredPlayer :: P.Player -> Int -> Double -> Double -> ScoredPlayer
createScoredPlayer player =
  ScoredPlayer (P.pid player) (P.firstName player) (P.lastName player) (P.position player)

dot :: Num a => [a] -> [a] -> a
dot a b = sum $ fmap (uncurry (*)) (zip a b)
