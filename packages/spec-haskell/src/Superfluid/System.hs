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

import           Superfluid.BaseTypes                               (Address, Liquidity, Timestamp)
import           Superfluid.Concepts.Account                        (Account)
import qualified Superfluid.Concepts.Account                        as SF_ACC
import           Superfluid.Concepts.RealtimeBalance                (RealtimeBalance)
--
import           Superfluid.Agreements.ConstantFlowAgreement        as CFA
import           Superfluid.Agreements.TransferableBalanceAgreement as TBA


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
    UpdateLiquidity :: (Liquidity lq, Timestamp ts)
        => (addr, TBA.TBAAccountData lq ts) -> SuperfluidStorageInstruction lq ts addr
    UpdateFlow :: (Liquidity lq, Timestamp ts)
        => (addr, addr, CFA.CFAContractData lq ts) -> SuperfluidStorageInstruction lq ts addr
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
-- * Instructions for write operations are executed in `execStorageInstructions`.
--
class ( Monad tk
      , Liquidity (SF_LQ tk)
      , Timestamp (SF_TS tk)
      , Address (SF_ADDR tk)
      , SuperfluidAccount (SF_ACC tk) (SF_LQ tk) (SF_TS tk) (SF_RTB tk)(SF_ADDR tk))
    => SuperfluidToken tk where

    -- Associated type families
    type SF_LQ tk :: *
    type SF_TS tk :: *
    type SF_RTB tk :: *
    type SF_ADDR tk :: *
    type SF_ACC tk :: *

    execStorageInstructions :: [SuperfluidStorageInstruction (SF_LQ tk) (SF_TS tk) (SF_ADDR tk)] -> tk ()

    --
    -- Account operations
    --
    getAccount :: SF_ADDR tk -> tk (SF_ACC tk)

    balanceOf :: SF_ADDR tk -> SF_TS tk -> tk (SF_RTB tk)
    balanceOf addr t = do
        account <- getAccount addr
        return $ SF_ACC.balanceOf account t

    --
    -- TBA functions
    --
    mintLiquidity :: SF_ADDR tk -> SF_LQ tk -> tk ()
    mintLiquidity addr liquidity = do
        account <- getAccount addr
        let account' = (TBA.mintLiquidity . getTBAAccountData) account liquidity
        execStorageInstructions [ UpdateLiquidity (addr, account') ]

    --
    -- CFA functions
    --
    getFlow :: SF_ADDR tk -> SF_ADDR tk -> tk (CFA.CFAContractData (SF_LQ tk) (SF_TS tk))

    updateFlow :: SF_ADDR tk -> SF_ADDR tk -> SF_LQ tk -> SF_TS tk -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate t = do
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        let (flowACD', senderFlowAAD', receiverFlowAAD') = CFA.updateFlow
                (flowACD, (getCFAAccountData senderAccount), (getCFAAccountData receiverAccount))
                newFlowRate t
        execStorageInstructions
            [ UpdateFlow (senderAddr, receiverAddr, flowACD')
            , UpdateAccountFlow (senderAddr, senderFlowAAD')
            , UpdateAccountFlow (receiverAddr, receiverFlowAAD')
            ]
