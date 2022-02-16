{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Superfluid.System
    ( SuperfluidAccount (..)
    , SuperfluidStorageInstruction (..)
    , SuperfluidToken (..)
    ) where

import           Control.Monad                                      (Monad)

import           Superfluid.Agreements.ConstantFlowAgreement        as CFA
import           Superfluid.Agreements.TransferableBalanceAgreement as TBA
import           Superfluid.BaseTypes                               (Address, Liquidity, Timestamp)
import           Superfluid.Concepts.Account                        (Account)
import qualified Superfluid.Concepts.Account                        as Account
import           Superfluid.Concepts.RealtimeBalance                (RealtimeBalance)


-- ============================================================================
-- | SuperfluidAccount
--
class (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr, Account acc lq ts rtb addr)
    => SuperfluidAccount acc lq ts rtb addr where
    showAt :: acc -> ts -> String
    getTBAAccountData :: acc -> TBA.TBAAccountData lq ts
    updateTBAAccountData :: acc -> TBA.TBAAccountData lq ts -> acc
    getCFAAccountData :: acc -> CFA.CFAAccountData lq ts
    updateCFAAccountData :: acc -> CFA.CFAAccountData lq ts -> acc

-- ============================================================================
-- | SuperfluidStorageInstruction Sum Type
--
data SuperfluidStorageInstruction lq ts addr where
    UpdateFlow :: (Liquidity lq, Timestamp ts)
        => CFA.CFAContractData lq ts -> SuperfluidStorageInstruction lq ts addr
    UpdateAccountFlow :: (Liquidity lq, Timestamp ts, Address addr)
        => (addr, CFA.CFAAccountData lq ts) -> SuperfluidStorageInstruction lq ts addr

-- ============================================================================
-- | SuperfluidToken Type Class
--
-- Naming conventions:
--   * Type name: tk
--   * Term name: *Token
--
-- Notes:
--
-- * SuperfluidToken is a monadic type, where all its functions run within the monadic context.
-- * SuperfluidToken provides:
--   * addressable account,
--   * and agreement (TBA/CFA/GDA) operations.
-- * To implement agreement write operations, one should use the '*Pure' helper functions which returns storage
--   instructions for you to commit the changSimpleTimestampes.
--
class ( Monad tk
      , Liquidity (LQ tk)
      , Timestamp (TS tk)
      , Address (ADDR tk)
      , SuperfluidAccount (ACC tk) (LQ tk) (TS tk) (RTB tk)(ADDR tk))
    => SuperfluidToken tk where

    -- Associated type families
    type LQ tk :: *
    type TS tk :: *
    type RTB tk :: *
    type ADDR tk :: *
    type ACC tk :: *

    --
    -- Account operations
    --
    getAccount :: ADDR tk -> tk (ACC tk)

    balanceOf :: ADDR tk -> TS tk -> tk (RTB tk)
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
