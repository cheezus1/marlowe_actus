module Language.Marlowe.ACTUS.Util.Conventions.DayCount where

-- IPDC
data DCC =  A_AISDA
          | A_360
          | A_365
          | E30_360ISDA
          | E30_360
          | B_252 deriving (Show)
