{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Superfluid.System
    ( SuperfluidAddress
    , SuperfluidAccount (..)
    , SuperfluidStorageInstruction (..)
    , SuperfluidToken (..)
    ) where

import           Control.Monad                                      (Monad)

import           Superfluid.Agreements.ConstantFlowAgreement        as CFA
import           Superfluid.Agreements.TransferableBalanceAgreement as TBA
import           Superfluid.Concepts.Account                        (Account)
import qualified Superfluid.Concepts.Account                        as Account
import           Superfluid.Concepts.Liquidity                      (Liquidity)
import           Superfluid.Concepts.RealtimeBalance                (RealtimeBalance)
import           Superfluid.Concepts.Timestamp                      (Timestamp)


--- SuperfluidAddress type class
---
class (Eq addr, Show addr) => SuperfluidAddress addr

--- SuperfluidAccount type class
---
class (Liquidity lq, Timestamp ts, Account acc lq ts)
    => SuperfluidAccount acc lq ts where
    showAt :: acc -> ts -> String
    getTBAAccountData :: acc -> TBA.TBAAccountData lq ts
    getCFAAccountData :: acc -> CFA.CFAAccountData lq ts

--- SuperfluidStorageInstruction sum type
---
data SuperfluidStorageInstruction lq ts addr where
    UpdateFlow :: (Liquidity lq, Timestamp ts)
        => CFA.CFAContractData lq ts -> SuperfluidStorageInstruction lq ts addr
    UpdateAccountFlow :: (Liquidity lq, Timestamp ts, SuperfluidAddress addr)
        => (addr, CFA.CFAAccountData lq ts) -> SuperfluidStorageInstruction lq ts addr

-- SuperfluidToken type class
--
-- Naming conventions:
--   * Type name: tk
--   * Term name: *TK
--
-- NOTE:
--
-- * Superfluid token should be a monad, where it provides:
--   * addressable account data,
--   * and agreement (TBA/CFA/GDA) operations.
--
-- * To implement agreement write operations, one should use the '*Pure' helper functions which returns storage
--   instructions for you to commit the changSimpleTimestampes.
--
class (Monad tk
      , Liquidity (LQ tk)
      , Timestamp (TS tk)
      , SuperfluidAddress (ADDR tk)
      , SuperfluidAccount (ACC tk) (LQ tk) (TS tk))
    => SuperfluidToken tk where

    -- Associated type families
    type LQ tk
    type TS tk
    type ADDR tk
    type ACC tk

    --
    -- Account operations
    --

    getAccount :: ADDR tk -> tk (ACC tk)

    balanceOf :: ADDR tk -> TS tk -> tk (RealtimeBalance (LQ tk))
    balanceOf addr t = do
        account <- getAccount addr
        return $ Account.balanceOf account t

    --
    -- CFA functions
    --

    getFlow :: ADDR tk -> ADDR tk -> tk (CFA.CFAContractData (LQ tk) (TS tk))

    updateFlowPure
        :: ADDR tk -> ADDR tk -> LQ tk -> TS tk
        -> tk [SuperfluidStorageInstruction (LQ tk) (TS tk) (ADDR tk)]
    updateFlowPure senderAddr receiverAddr newFlowRate t = do
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        let (flowACD', senderFlowAAD', receiverFlowAAD') = CFA.updateFlow
                (flowACD, (getCFAAccountData senderAccount), (getCFAAccountData receiverAccount))
                newFlowRate t
        return [ UpdateFlow flowACD'
               , UpdateAccountFlow (senderAddr, senderFlowAAD')
               , UpdateAccountFlow (receiverAddr, receiverFlowAAD')
               ]

    updateFlow :: ADDR tk -> ADDR tk -> LQ tk -> TS tk -> tk ()
