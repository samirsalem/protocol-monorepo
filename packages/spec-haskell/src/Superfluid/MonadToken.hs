{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GADTs #-}

module Superfluid.MonadToken
    ( MonadTokenUpdate(..)
    , MonadToken(..)
    ) where

import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.Account (Account)
import Superfluid.Agreements.ConstantFlowAgreement as CFA

import Control.Monad (Monad)


data MonadTokenUpdate liq where
    UpdateFlow :: Liquidity liq => CFA.CFAAgreementData liq -> MonadTokenUpdate liq
    UpdateAccountFlow :: Liquidity liq => CFA.CFAAccountData liq -> MonadTokenUpdate liq

class (Eq addr, Monad tk, Liquidity liq, Account a liq) =>
    MonadToken tk liq addr a | tk -> a where

    getAccountFlow :: addr -> tk (CFA.CFAAccountData liq)

    getFlow :: addr -> addr -> tk (CFA.CFAAgreementData liq)

    updateFlow :: addr -> addr -> liq -> Timestamp -> tk [MonadTokenUpdate liq]
    updateFlow sender receiver newFlowRate t = do
        senderAccountFlow <- getAccountFlow sender
        receiverAccountFlow <- getAccountFlow receiver
        cfaAgreement <- getFlow sender receiver
        let (cfaAgreement', senderAccountFlow', receiverAccountFlow') = CFA.updateFlow
                cfaAgreement
                senderAccountFlow
                receiverAccountFlow
                newFlowRate
                t
        return [ UpdateFlow cfaAgreement'
               , UpdateAccountFlow senderAccountFlow'
               , UpdateAccountFlow receiverAccountFlow'
               ]
