module Language.Marlowe.ACTUS.Util.Conventions.ContractDefault where

import Language.Marlowe.ACTUS.Definitions

contractDefault status =
  if status /= CS_DF then 1.0
  else 0.0
