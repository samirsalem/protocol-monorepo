{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.System.SuperfluidToken
    ( SuperfluidTokenUpdate (..)
    , SuperfluidToken (..)
    ) where

import           Control.Monad                               (Monad)

import           Superfluid.Agreements.ConstantFlowAgreement as CFA
import           Superfluid.Concepts.Liquidity               (Liquidity)
import           Superfluid.Concepts.Timestamp               (Timestamp)
import           Superfluid.System.SuperfluidAccount         (SuperfluidAccount (..))

-- TODO: Update addr type
data SuperfluidTokenUpdate liq addr where
    UpdateFlow
        :: Liquidity liq
        => CFA.CFAContractData liq -> SuperfluidTokenUpdate liq addr
    UpdateAccountFlow
        :: (Eq addr, Liquidity liq)
        => (addr, CFA.CFAAccountData liq) -> SuperfluidTokenUpdate liq addr

{- Superfluid Token type class

@param tk - token type, instance of Monad
@param liq - liquidity type, instance of Liquidity
@param addr - address type, instance of Eq
@param acc - account type, instance of Account
-}
class (Monad tk, Eq addr, SuperfluidAccount acc liq)
    => SuperfluidToken tk liq addr acc
    | tk -> liq, tk -> addr, tk -> acc
    where

    getAccount :: addr -> tk acc

    --
    -- CFA functions
    --

    getFlow :: addr -> addr -> tk (CFA.CFAContractData liq)

    _updateFlow :: addr -> addr -> liq -> Timestamp -> tk [SuperfluidTokenUpdate liq addr]
    _updateFlow senderAddr receiverAddr newFlowRate t = do
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flow <- getFlow senderAddr receiverAddr
        let (flow', senderAccountFlow', receiverAccountFlow') = CFA.updateFlow
                flow
                (getCFAAccountData senderAccount)
                (getCFAAccountData receiverAccount)
                newFlowRate
                t
        return [ UpdateFlow flow'
               , UpdateAccountFlow (senderAddr, senderAccountFlow')
               , UpdateAccountFlow (receiverAddr, receiverAccountFlow')
               ]

    updateFlow :: addr -> addr -> liq -> Timestamp -> tk ()
