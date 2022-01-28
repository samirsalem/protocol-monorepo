{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Superfluid.Testing.SimpleAccount
    ( WAD
    , toWad
    , wad4humanN
    , wad4human
    , AccountAddress
    , createAccountAddress
    , SimpleRealtimeBalance
    , SimpleDummyAgreementAccountData
    , SimpleCFAAgreementData
    , SimpleCFAAccountData
    , SimpleAccount(..)
    , createSimpleAccount
    , sumAllSimpleAccount
    ) where

import Superfluid
    ( Liquidity
    , Timestamp
    , RealtimeBalance(..)
    , Account
    , DummyAgreementAccountData(..)
    , CFAAgreementData
    , CFAAccountData(..)
    )
import qualified Superfluid.Core.Account as Account
    ( Account(..)
    )
import Superfluid.Core.Agreement (makeAgreementData)
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
    ( CFAAccountData(..)
    )

import Text.Printf (printf)

{-
# Account address type
-}
newtype AccountAddress = AccountAddress String
createAccountAddress :: String -> AccountAddress
createAccountAddress x = AccountAddress x

instance Eq AccountAddress where
    (==) (AccountAddress a) (AccountAddress b)= a == b
instance Ord AccountAddress where
    compare (AccountAddress a) (AccountAddress b)= compare a b
instance Show AccountAddress where
    show (AccountAddress a )= a

{-
# WAD (18 decimal digit fixed-precision integer) Utilities
-}
newtype WAD = WAD Integer

toWad :: (RealFrac a) => a -> WAD
toWad x = WAD (round $ x * (10 ^ (18::Int)))

wad4humanN :: WAD -> Int -> String
wad4humanN (WAD wad) n
    | n >= 0 && n <= 18 = printf
        ("%0."++(show n)++"f")
        ((fromIntegral wad / (10 ^ (18::Int))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: WAD -> String
wad4human wad = wad4humanN wad 4

instance Num WAD where
    (+) (WAD a) (WAD b)= WAD (a + b)
    (*) (WAD a) (WAD b)= WAD (a * b)
    abs (WAD x)= WAD (abs x)
    signum (WAD x)= WAD (signum x)
    fromInteger = WAD
    negate (WAD x) = WAD (negate x)
instance Eq WAD where
    (==) (WAD a) (WAD b)= a == b
instance Ord WAD where
    compare (WAD a) (WAD b)= compare a b
instance Show WAD where
    show = wad4human

instance Liquidity WAD where

type SimpleRealtimeBalance = RealtimeBalance WAD
type SimpleDummyAgreementAccountData = DummyAgreementAccountData WAD
type SimpleCFAAgreementData = CFAAgreementData WAD
type SimpleCFAAccountData = CFAAccountData WAD

data SimpleAccount = SimpleAccount
    { address :: AccountAddress
    , staticBalance :: WAD
    , dummy :: SimpleDummyAgreementAccountData
    , cfa :: SimpleCFAAccountData
    , lastUpdatedAt :: Timestamp
    }

instance Account SimpleAccount WAD where
    staticBalanceOf a = staticBalance a
    agreementsOf a =
        [ makeAgreementData $ dummy a
        , makeAgreementData $ cfa a
        ]

createSimpleAccount :: String -> WAD -> Timestamp -> SimpleAccount
createSimpleAccount a (WAD wad) t = SimpleAccount
    { address = createAccountAddress a
    , staticBalance = 0
    , lastUpdatedAt = t
    , dummy = DummyAgreementAccountData "dummy" (fromInteger wad)
    , cfa = CFAAccountData
        { CFA.settledAt = t
        , CFA.settledBalance = fromInteger(0)
        , CFA.netFlowRate = 0
        }
    }

sumAllSimpleAccount :: [SimpleAccount] -> Timestamp -> SimpleRealtimeBalance
sumAllSimpleAccount alist t = foldr
    (+)
    (RealtimeBalance 0 0 0)
    (map (flip Account.balanceOf t) alist)
