module LineupBuilder where

import Data.LinearProgram
import Data.LinearProgram.GLPK
import Data.LinearProgram.LPMonad
import qualified Data.Map as M
import PlayerAggregator

buildLineup :: [ScoredPlayer] -> [ScoredPlayer]
buildLineup allPlayers = glpSolveVars mipDefaults (lineupLP allPlayers (replicate (length allPlayers) 0))

lineupLP :: [ScoredPlayer] -> [Int] -> LP ScoredPlayer Int
lineupLP players selected = execLPM $ do
  setDirection Max
  setObjective M.fromList $ zip selected players
  equal (varSum selected) 11 -- select 11 players
  mapM_ (`varGeq`  0) players
  mapM_ (`varLeq` 1) players
  mapM_ (`setVarKind` IntVar) players
