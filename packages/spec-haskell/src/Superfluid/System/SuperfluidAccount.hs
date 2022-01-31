{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RecordWildCards        #-}

module Superfluid.System.SuperfluidAccount
    ( SuperfluidAccount (..)
    ) where

import           Superfluid.Agreements.ConstantFlowAgreement as CFA
import           Superfluid.Concepts.Account                 (Account)
import           Superfluid.Concepts.Timestamp               (Timestamp)


class (Account acc liq) => SuperfluidAccount acc liq
    where

    showAt :: acc -> Timestamp -> String

    getCFAAccountData :: Account acc liq => acc -> CFA.CFAAccountData liq
    -- FIXME make a default function of it
    -- getCFAAccountData acc = head $
    --     foldr (++) [] $
    --     map (\aad -> case aad of
    --         -- MkAgreementAccountData
    --         --     (x@CFA.CFAAccountData{..} :: CFA.CFAAccountData liq)
    --         --     -> [x]
    --         _                        -> []
    --     ) $ agreementsOf acc
