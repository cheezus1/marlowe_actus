module Language.Marlowe.ACTUS.Util.Conventions.ContractDefault where

-- | CS – Indicates different states of the contract from performance to default
data ContractStatus = CS_PF -- performant
                    | CS_DL -- delayed
                    | CS_DQ -- delinquent
                    | CS_DF -- default
                    deriving (Show, Eq)

contractDefault status =
  if status /= CS_DF then 1.0
  else 0.0
