{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.TransferableBalanceAgreement
    ( TBAAccountData (..)
    ) where

import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (liquidityToRTB)
import           Superfluid.Concepts.Timestamp       (Timestamp)


data (Liquidity lq, Timestamp ts) => TBAAccountData lq ts = TBAAccountData
    { settledAt :: ts
    , liquidity :: lq
    }

instance (Liquidity lq, Timestamp ts) => Default (TBAAccountData lq ts) where
    def = TBAAccountData { settledAt = def, liquidity = def }

instance (Liquidity lq, Timestamp ts) => AgreementAccountData (TBAAccountData lq ts) lq ts where
    providedBalanceOf a _ = liquidityToRTB $ liquidity a

instance (Liquidity lq, Timestamp ts) => Show (TBAAccountData lq ts) where
    show x = printf "{ settledAt = %s, liquidity = %s }" (show $ settledAt x) (show $ liquidity x)
