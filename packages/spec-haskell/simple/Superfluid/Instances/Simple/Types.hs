{-# LANGUAGE DerivingVia                #-}
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
    , SimpleTBAAccountData
    , SimpleCFAContractData
    , SimpleCFAAccountData
    ) where

import           Data.Char
import           Data.Default
import           Text.Printf                                        (printf)

import           Superfluid.BaseTypes                               (Address, Liquidity, Timestamp)
import qualified Superfluid.Concepts.Account                        as ACC
import           Superfluid.Concepts.Agreement                      (AnyAgreementAccountData (MkAgreementAccountData))
import qualified Superfluid.Concepts.RealtimeBalance                as RTB
--
import           Superfluid.Agreements.ConstantFlowAgreement        (CFAAccountData, CFAContractData)
import           Superfluid.Agreements.TransferableBalanceAgreement (TBAAccountData)

import qualified Superfluid.System                                  as SF


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
newtype SimpleTimestamp = SimpleTimestamp Int deriving (Enum, Eq, Ord, Num, Real, Integral, Default)

instance Timestamp SimpleTimestamp

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t

-- ============================================================================
-- SimpleRealtimeBalance Base Type
--
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { availableBalance :: Wad
    , deposit          :: Wad
    , owedDeposit      :: Wad
    }
    deriving Num via RTB.RealtimeBalanceAsNum SimpleRealtimeBalance Wad

instance Default SimpleRealtimeBalance where
    def = SimpleRealtimeBalance { availableBalance = def, deposit = def, owedDeposit = def }

instance RTB.RealtimeBalance SimpleRealtimeBalance Wad where
    availableBalance = availableBalance
    toBalanceVector x = map (flip id x) [availableBalance, deposit, owedDeposit]
    fromBalanceVector v = if length v == 3
        then SimpleRealtimeBalance (v!!0) (v!!1) (v!!2)
        else error "wrong balance vector"
    liquidityToRTB x = SimpleRealtimeBalance x o o where o = fromInteger(0)

instance Show SimpleRealtimeBalance where
    show (SimpleRealtimeBalance avb d od) =
        "("
        ++ show avb ++ "@avb, "
        ++ show d ++ "@d, "
        ++ show od ++ "@od"
        ++ ")"

-- ============================================================================
-- SimpleAddress Base Type
--
newtype SimpleAddress = SimpleAddress String deriving (Eq, Ord, Address)

-- SimpleAddress public constructor
createSimpleAddress :: String -> Maybe SimpleAddress
createSimpleAddress a = if (all isAlpha a) then Just $ SimpleAddress a else Nothing

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
createSimpleAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
createSimpleAccount toAddress t = SimpleAccount
    { address = toAddress
    , lastUpdatedAt = t
    , tba = def
    , cfa = def
    }

instance ACC.Account SimpleAccount Wad SimpleTimestamp SimpleRealtimeBalance SimpleAddress where
    addressOf = address

    agreementsOf a =
        [ MkAgreementAccountData $ tba a
        , MkAgreementAccountData $ cfa a
        ]

instance SF.SuperfluidAccount SimpleAccount Wad SimpleTimestamp SimpleRealtimeBalance SimpleAddress where
    showAt a t =
        "Account @" ++ show(address a) ++
        "\n  Balance: " ++ show((ACC.balanceOfAt a t) :: SimpleRealtimeBalance) ++
        "\n  TBA: " ++ show(tba a) ++
        "\n  CFA: " ++ show(cfa a) ++
        "\n  Last Update: " ++ show(lastUpdatedAt a)

    -- getTBAAccountData :: SimpleAccount -> SimpleTBAAccountData
    getTBAAccountData = tba

    -- updateTBAAccountData :: SimpleAccount -> SimpleTimestamp -> SimpleTBAAccountData -> SimpleAccount
    updateTBAAccountData acc t' tba' = acc { tba = tba', lastUpdatedAt = t' }

    -- getCFAAccountData :: SimpleAccount -> SimpleCFAAccountData
    getCFAAccountData = cfa

    -- updateCFAAccountData :: SimpleAccount -> SimpleTimestamp -> SimpleCFAAccountData -> SimpleAccount
    updateCFAAccountData acc t' cfa' = acc { cfa = cfa', lastUpdatedAt = t' }
