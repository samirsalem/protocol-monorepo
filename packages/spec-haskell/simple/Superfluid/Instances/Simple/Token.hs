{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Superfluid.Instances.Simple.Token
    ( SimpleTokenData
    , SimpleTokenState
    , createSimpleToken
    , addAccount
    , listAccounts)
    where

import           Control.Monad.State
import           Data.Default
import qualified Data.Map                                    as M

import           Superfluid                                  (SuperfluidToken)
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.System                           as SF

import           Superfluid.Instances.Simple.Account         (SimpleAccount)
import qualified Superfluid.Instances.Simple.Account         as SimpleAccount
import           Superfluid.Instances.Simple.BaseTypes
    ( SimpleAddress
    , SimpleCFAContractData
    , SimpleTimestamp
    , Wad
    )


data SimpleTokenData = SimpleTokenData
    { accounts      :: M.Map SimpleAddress SimpleAccount
    , cfaAgreements :: M.Map String SimpleCFAContractData
    }

instance Default SimpleTokenData
    where def = SimpleTokenData { accounts = def, cfaAgreements = def }

createSimpleToken :: [(SimpleAddress, SimpleAccount)] -> SimpleTokenData
createSimpleToken alist = SimpleTokenData
    { accounts = M.fromList alist
    , cfaAgreements = M.fromList []
    }

type SimpleTokenState = State SimpleTokenData

instance SuperfluidToken SimpleTokenState where

    type LQ SimpleTokenState = Wad
    type TS SimpleTokenState = SimpleTimestamp
    type ADDR SimpleTokenState = SimpleAddress
    type ACC SimpleTokenState = SimpleAccount

    getAccount :: SimpleAddress -> SimpleTokenState SimpleAccount
    getAccount a = get >>= \s -> return $
        case M.lookup a (accounts s) of
            Just value -> value
            Nothing    -> error "No such address"

    getFlow :: SimpleAddress -> SimpleAddress -> SimpleTokenState SimpleCFAContractData
    getFlow a b = get >>= \s -> return $
        case M.lookup (show(a)++":"++show(b)) (cfaAgreements s) of
            Just value -> value
            Nothing    -> CFA.CFAContractData 0 0

    updateFlow :: SimpleAddress -> SimpleAddress -> Wad -> SimpleTimestamp -> SimpleTokenState ()
    updateFlow sender receiver newFlowRate t = do
        updates <- SF.updateFlowPure sender receiver newFlowRate t
        mapM_ (\u -> case u of
            SF.UpdateFlow flow' -> do
                modify (\vs -> vs {
                    cfaAgreements = M.insert
                        (show(sender)++":"++show(receiver))
                        flow'
                        (cfaAgreements vs)
                })
            SF.UpdateAccountFlow (accountAddr, accountFlow') -> do
                account <- SF.getAccount accountAddr
                modify (\vs -> vs {
                    accounts = M.insert
                        accountAddr
                        (account { SimpleAccount.cfa = accountFlow' })
                        (accounts vs)
                })
            ) updates

addAccount :: SimpleAddress -> SimpleAccount -> SimpleTokenState ()
addAccount accountAddr account = modify (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: SimpleTokenState [(SimpleAddress, SimpleAccount)]
listAccounts = get >>= \s -> return $ M.toList (accounts s)

-- FIXME create a SuperfluidTokenT
