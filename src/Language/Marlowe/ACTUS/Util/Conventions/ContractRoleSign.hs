module Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign where

import Language.Marlowe.ACTUS.Definitions

-- R
contractRoleSign role = case role of
    CR_RPA ->  1.0
    CR_RPL -> -1.0
    CR_CLO ->  1.0
    CR_CNO ->  1.0
    CR_COL ->  1.0
    CR_LG  ->  1.0
    CR_ST  -> -1.0
    CR_BUY ->  1.0
    CR_SEL -> -1.0
    CR_RFL ->  1.0
    CR_PFL -> -1.0
    CR_RF  ->  1.0
    CR_PF  -> -1.0
