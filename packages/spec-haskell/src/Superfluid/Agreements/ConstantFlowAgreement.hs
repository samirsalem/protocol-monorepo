{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , updateFlow
    ) where

import           Data.Default
import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (integralToLiquidity,
                                                      liquidityToRTB)
import           Superfluid.Concepts.Timestamp       (Timestamp)


data CFAContractData liq = CFAContractData
    { flowLastUpdatedAt :: Timestamp
    , flowRate          :: liq
    }

data CFAAccountData liq = CFAAccountData
    { settledAt      :: Timestamp
    , settledBalance :: liq
    , netFlowRate    :: liq
    }

instance (Liquidity liq) => Default (CFAAccountData liq) where
    def = CFAAccountData
        { settledAt = def
        , settledBalance = def
        , netFlowRate = def
        }

instance Liquidity liq => Show (CFAAccountData liq) where
    show CFAAccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        } = "net flowrate " ++ (show r)
        ++  ", settled balance " ++ (show b_s)
        ++  ", settled at " ++ (show t_s)

instance Liquidity liq
    => AgreementAccountData (CFAAccountData liq) liq where
    providedBalanceOf
        CFAAccountData
            { netFlowRate = r
            , settledBalance = b_s
            , settledAt = t_s
            }
        t = liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

_updateFlowRate :: Liquidity liq =>
    CFAAccountData liq -> liq -> Timestamp -> CFAAccountData liq
_updateFlowRate
    CFAAccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        }
    r_delta t =
    CFAAccountData
        { netFlowRate = r + r_delta
        , settledBalance = b_s + integralToLiquidity(t - t_s) * r
        , settledAt = t
        }

updateFlow :: Liquidity liq =>
    CFAContractData liq ->
    CFAAccountData liq ->
    CFAAccountData liq ->
    liq ->
    Timestamp ->
    (CFAContractData liq, CFAAccountData liq, CFAAccountData liq)
updateFlow cfa sender receiver newFlowRate t =
    let flowRateDelta = newFlowRate - (flowRate cfa)
    in
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , _updateFlowRate sender (negate flowRateDelta) t
    , _updateFlowRate receiver flowRateDelta t
    )
