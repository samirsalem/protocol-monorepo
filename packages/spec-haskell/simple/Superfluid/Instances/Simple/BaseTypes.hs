{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Superfluid.Instances.Simple.BaseTypes
    ( Wad(..)
    , toWad
    , wad4humanN
    , wad4human
    , SimpleTimestamp
    , SimpleRealtimeBalance
    , SimpleAddress
    , createSimpleAddress
    , SimpleTBAAccountData
    , SimpleCFAContractData
    , SimpleCFAAccountData
    ) where

import           Data.Default
import           Text.Printf       (printf)

import           Superfluid
    ( CFAAccountData
    , CFAContractData
    , Liquidity
    , RealtimeBalance
    , TBAAccountData
    , Timestamp
    )
import qualified Superfluid.System as SF


-- ============================================================================
-- Wad type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
-- ============================================================================

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
-- SimpleTimestamp Type
-- ============================================================================

newtype SimpleTimestamp = SimpleTimestamp Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Default)

instance Timestamp SimpleTimestamp

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t

-- ============================================================================
-- SimpleAddress Type
-- ============================================================================

newtype SimpleAddress = SimpleAddress String deriving (Eq, Ord, SF.SuperfluidAddress)

instance Show SimpleAddress where
    show (SimpleAddress a) = a

createSimpleAddress :: String -> SimpleAddress
createSimpleAddress = SimpleAddress

-- ============================================================================
-- Other Simple Type Aliases
-- ============================================================================
type SimpleRealtimeBalance = RealtimeBalance Wad
type SimpleTBAAccountData = TBAAccountData Wad SimpleTimestamp
type SimpleCFAContractData = CFAContractData Wad SimpleTimestamp
type SimpleCFAAccountData = CFAAccountData Wad SimpleTimestamp
