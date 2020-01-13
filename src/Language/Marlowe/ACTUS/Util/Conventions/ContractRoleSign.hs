module Language.Marlowe.ACTUS.Util.Conventions.ContractRoleSign where

-- CNTRL
data ContractRole = CR_RPA -- Real position asset
                  | CR_RPL -- Real position liability
                  | CR_CLO -- Role of a collateral
                  | CR_CNO -- Role of a close-out-netting
                  | CR_COL -- Role of an underlying to a collateral
                  | CR_LG  -- Long position
                  | CR_ST  -- Short position
                  | CR_BUY -- Protection buyer
                  | CR_SEL -- Protection seller
                  | CR_RFL -- Receive first leg
                  | CR_PFL -- Pay first leg
                  | CR_RF  -- Receive fix leg
                  | CR_PF  -- Pay fix leg
                  deriving (Show, Eq)

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
