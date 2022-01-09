module Superfluid.Testing.SimpleAccount
    ( Address
    , SimpleAccount(..)
    , createSimpleAccount
    , sumAllSimpleAccount) where

import Superfluid
    ( RealtimeBalance(..)
    , Account(..)
    )
import Superfluid.Types (Timestamp)
import Superfluid.SuperAgreement
    ( makeSuperAgreementData
    , DummyAgreementAccountData(..)
    )
import Superfluid.Agreements.ConstantFlowAgreement
    ( CFAv1AccountData(..)
    )

type Address = String

data SimpleAccount = SimpleAccount
    { address :: Address
    , staticBalance :: Integer
    , dummy :: DummyAgreementAccountData
    , cfa :: CFAv1AccountData
    , lastUpdatedAt :: Timestamp
    }

instance Account SimpleAccount where
    staticBalanceOf a = staticBalance a
    agreementsOf a =
        [ makeSuperAgreementData $ dummy a
        , makeSuperAgreementData $ cfa a
        ]

createSimpleAccount :: String -> RealtimeBalance -> Timestamp -> SimpleAccount
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

sumAllSimpleAccount :: [SimpleAccount] -> Timestamp -> RealtimeBalance
sumAllSimpleAccount alist t = foldr
    (+)
    (RealtimeBalance 0 0 0)
    (map (flip Superfluid.balanceOf t) alist)
