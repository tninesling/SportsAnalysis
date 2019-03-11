module Helpers (
  readAsDouble,
  readAsInt
) where

readAsDouble s = read s :: Double

readAsInt s = read s :: Int
