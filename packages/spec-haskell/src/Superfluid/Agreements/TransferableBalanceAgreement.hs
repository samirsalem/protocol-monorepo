{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.TransferableBalanceAgreement
    ( TBAAccountData
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.SuperfluidTypes (Liquidity, RealtimeBalance, Timestamp, liquidityToRTB)


data (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => TBAAccountData lq ts rtb = TBAAccountData
    { settledAt :: ts
    , liquidity :: lq
    }

-- ============================================================================
-- | TBAAccountData Type (is AgreementAccountData)
--
instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Default (TBAAccountData lq ts rtb) where
    def = TBAAccountData { settledAt = def, liquidity = def }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => AgreementAccountData (TBAAccountData lq ts rtb) lq ts rtb where
    providedBalanceOfAgreement a _ = liquidityToRTB $ liquidity a

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Show (TBAAccountData lq ts rtb) where
    show x = printf "{ settledAt = %s, liquidity = %s }" (show $ settledAt x) (show $ liquidity x)

-- ============================================================================
-- TBA Operations
--
mintLiquidity
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => TBAAccountData lq ts rtb -> lq -> TBAAccountData lq ts rtb
mintLiquidity a l = a { liquidity = (liquidity a) + l }

burnLiquidity
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => TBAAccountData lq ts rtb -> lq -> TBAAccountData lq ts rtb
burnLiquidity a l = a { liquidity = (liquidity a) - l }

transferLiquidity
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => (TBAAccountData lq ts rtb, TBAAccountData lq ts rtb) -> lq
    -> (TBAAccountData lq ts rtb, TBAAccountData lq ts rtb)
transferLiquidity (from, to) l =
    ( from { liquidity = (liquidity from) - l }
    , to   { liquidity = (liquidity   to) + l })
