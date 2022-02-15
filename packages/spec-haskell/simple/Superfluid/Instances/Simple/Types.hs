{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Superfluid.Instances.Simple.Types
    ( Wad
    , toWad
    , wad4humanN
    , wad4human
    , SimpleTimestamp
    , SimpleRealtimeBalance
    , SimpleAddress
    , createSimpleAddress
    , SimpleAccount
    , createSimpleAccount
    , sumAllSimpleAccount
    , SimpleTBAAccountData
    , SimpleCFAContractData
    , SimpleCFAAccountData
    ) where

import           Data.Default
import           Text.Printf                                        (printf)

import           Superfluid.Types
    ( Account
    , Address
    , CFAAccountData
    , CFAContractData
    , Liquidity
    , RealtimeBalance
    , TBAAccountData
    , Timestamp
    )

import qualified Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Superfluid.Concepts.Account                        as Account (Account (..), balanceOf)
import           Superfluid.Concepts.Agreement                      (AnyAgreementAccountData (MkAgreementAccountData))

import           Superfluid.System                                  (SuperfluidAccount (..))


-- ============================================================================
-- Wad type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
--
newtype Wad = Wad Integer deriving (Eq, Ord, Num, Default)

toWad :: (RealFrac a) => a -> Wad
toWad x = Wad (round $ x * (10 ^ (18::Integer)))

wad4humanN :: Wad -> Integer -> String -- TODO use Nat?
wad4humanN (Wad wad) n
    | n >= 0 && n <= 18 = printf
        ("%0."++(show n)++"f")
        ((fromIntegral wad / (10 ^ (18::Integer))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: Wad -> String
wad4human wad = wad4humanN wad 4

instance Show Wad where
    show = wad4human

instance Liquidity Wad where

-- ============================================================================
-- SimpleTimestamp Base Type
--
newtype SimpleTimestamp = SimpleTimestamp Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Default)

instance Timestamp SimpleTimestamp

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t

-- ============================================================================
-- SimpleRealtimeBalance Base Type
--
type SimpleRealtimeBalance = RealtimeBalance Wad

-- ============================================================================
-- SimpleAddress Base Type
--
newtype SimpleAddress = SimpleAddress String deriving (Eq, Ord, Address)

-- SimpleAddress public constructor
createSimpleAddress :: String -> SimpleAddress
createSimpleAddress = SimpleAddress -- TODO some simple address rules

instance Show SimpleAddress where
    show (SimpleAddress a) = a

-- ============================================================================
-- Simple Types for Agreements
--
type SimpleTBAAccountData = TBAAccountData Wad SimpleTimestamp
type SimpleCFAContractData = CFAContractData Wad SimpleTimestamp
type SimpleCFAAccountData = CFAAccountData Wad SimpleTimestamp

-- ============================================================================
-- SimpleAccount Type and Operations (is SuperfluidAccount)
--
data SimpleAccount = SimpleAccount
    { address       :: SimpleAddress
    , tba           :: SimpleTBAAccountData
    , cfa           :: SimpleCFAAccountData
    , lastUpdatedAt :: SimpleTimestamp
    }

-- SimpleAccount public constructor
createSimpleAccount :: SimpleAddress -> Wad -> SimpleTimestamp -> SimpleAccount
createSimpleAccount toAddress initBalance t = SimpleAccount
    { address = toAddress
    , lastUpdatedAt = t
    , tba = (def :: SimpleTBAAccountData){ TBA.liquidity = initBalance }
    , cfa = def
    }

instance Account SimpleAccount Wad SimpleTimestamp SimpleAddress where
    address = address

    agreementsOf a =
        [ MkAgreementAccountData $ tba a
        , MkAgreementAccountData $ cfa a
        ]

instance SuperfluidAccount SimpleAccount Wad SimpleTimestamp SimpleAddress where
    showAt a t =
        "Account: " ++ show(address a) ++
        "\n  Balance: " ++ show((Account.balanceOf a t) :: SimpleRealtimeBalance) ++
        "\n  TBA Data: " ++ show(tba a) ++
        "\n  CFA Data: " ++ show(cfa a) ++
        "\n  last Updated: " ++ show(lastUpdatedAt a)

    getTBAAccountData :: SimpleAccount -> SimpleTBAAccountData
    getTBAAccountData = tba

    updateTBAAccountData :: SimpleAccount -> SimpleTBAAccountData -> SimpleAccount
    updateTBAAccountData acc tba' = acc { tba = tba' }

    getCFAAccountData :: SimpleAccount -> SimpleCFAAccountData
    getCFAAccountData = cfa

    updateCFAAccountData :: SimpleAccount -> SimpleCFAAccountData -> SimpleAccount
    updateCFAAccountData acc cfa' = acc { cfa = cfa' }

sumAllSimpleAccount :: [SimpleAccount] -> SimpleTimestamp -> SimpleRealtimeBalance
sumAllSimpleAccount alist t = foldr
    (+)
    (def :: SimpleRealtimeBalance)
    (map (flip Account.balanceOf t) alist)
