{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Superfluid.Testing.SimpleAccount
    ( Address
    , SimpleAccount(..)
    , createSimpleAccount
    , sumAllSimpleAccount) where

import Superfluid
    ( RealtimeBalance(..)
    , Account(..)
    )
import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.SuperAgreement (makeSuperAgreementData)
import Superfluid.Agreements.DummyAgreement (DummyAgreementAccountData(..))
import Superfluid.Agreements.ConstantFlowAgreement (CFAv1AccountData(..))

instance Liquidity Integer where

type Address = String

data SimpleAccount = SimpleAccount
    { address :: Address
    , staticBalance :: Integer
    , dummy :: DummyAgreementAccountData Integer
    , cfa :: CFAv1AccountData Integer
    , lastUpdatedAt :: Timestamp
    }

instance Account SimpleAccount Integer where
    staticBalanceOf a = staticBalance a
    agreementsOf a =
        [ makeSuperAgreementData $ dummy a
        , makeSuperAgreementData $ cfa a
        ]

createSimpleAccount :: String -> RealtimeBalance Integer -> Timestamp -> SimpleAccount
createSimpleAccount a b t = SimpleAccount
    { address = a
    , staticBalance = 0
    , lastUpdatedAt = t
    , dummy = DummyAgreementAccountData "dummy" b
    , cfa = CFAv1AccountData
        { settledAt = t
        , settledBalance = fromInteger(0)
        , netFlowRate = 0
        }
    }

sumAllSimpleAccount :: [SimpleAccount] -> Timestamp -> RealtimeBalance Integer
sumAllSimpleAccount alist t = foldr
    (+)
    (RealtimeBalance 0 0 0)
    (map (flip Superfluid.balanceOf t) alist)
