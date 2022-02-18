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

import           Superfluid.BaseTypes                (Liquidity, Timestamp)
import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance, liquidityToRTB)


data (Liquidity lq, Timestamp ts) => TBAAccountData lq ts = TBAAccountData
    { settledAt :: ts
    , liquidity :: lq
    }

-- ============================================================================
-- | TBAAccountData Type (is AgreementAccountData)
--
instance (Liquidity lq, Timestamp ts)
    => Default (TBAAccountData lq ts) where
    def = TBAAccountData { settledAt = def, liquidity = def }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => AgreementAccountData (TBAAccountData lq ts) lq ts rtb where
    providedBalanceOf a _ = liquidityToRTB $ liquidity a

instance (Liquidity lq, Timestamp ts)
    => Show (TBAAccountData lq ts) where
    show x = printf "{ settledAt = %s, liquidity = %s }" (show $ settledAt x) (show $ liquidity x)

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: (Liquidity lq, Timestamp ts) => TBAAccountData lq ts -> lq -> TBAAccountData lq ts
mintLiquidity a l = a { liquidity = (liquidity a) + l }

burnLiquidity :: (Liquidity lq, Timestamp ts) => TBAAccountData lq ts -> lq -> TBAAccountData lq ts
burnLiquidity a l = a { liquidity = (liquidity a) - l }

transferLiquidity :: (Liquidity lq, Timestamp ts)
    => (TBAAccountData lq ts, TBAAccountData lq ts) -> lq
    -> (TBAAccountData lq ts, TBAAccountData lq ts)
transferLiquidity (from, to) l =
    ( from { liquidity = (liquidity from) - l }
    , to   { liquidity = (liquidity   to) + l })
