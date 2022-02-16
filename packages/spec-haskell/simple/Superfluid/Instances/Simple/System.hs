{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Superfluid.Instances.Simple.System
    ( SimpleTokenData
    , SimpleTokenStateT
    , initSimpleToken
    , addAccount
    , listAccounts
    , runSimpleTokenStateT
    , evalSimpleTokenStateT
    , withSimpleTokenStateT)
    where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Default
import qualified Data.Map                                    as M

import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.System                           as SF

import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleCFAContractData
    , SimpleRealtimeBalance
    , SimpleTimestamp
    , Wad
    , createSimpleAccount
    , toWad
    )


-- ============================================================================
-- | SimpleTokenData Type
--
data SimpleTokenData = SimpleTokenData
    { accounts      :: M.Map SimpleAddress SimpleAccount
    , cfaAgreements :: M.Map String SimpleCFAContractData
    }

instance Default SimpleTokenData
    where def = SimpleTokenData { accounts = def, cfaAgreements = def }

-- ============================================================================
-- | SimpleTokenStateT Type is a MonadTrans instance
--
newtype SimpleTokenStateT m a = SimpleTokenStateT
    { runSimpleTokenStateT :: SimpleTokenData -> m (a, SimpleTokenData) }

instance (Monad m) => Functor (SimpleTokenStateT m) where
    fmap f m = SimpleTokenStateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runSimpleTokenStateT m s

instance (Monad m) => Applicative (SimpleTokenStateT m) where
    pure a = SimpleTokenStateT $ \ s -> return (a, s)
    SimpleTokenStateT mf <*> SimpleTokenStateT mx = SimpleTokenStateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')

instance (Monad m) => Monad (SimpleTokenStateT m) where
    m >>= k = SimpleTokenStateT $ \s -> do
        ~(a, s') <- runSimpleTokenStateT m s
        runSimpleTokenStateT (k a) s'

instance (MonadIO m) => MonadIO (SimpleTokenStateT m) where
    liftIO = lift . liftIO

instance MonadTrans SimpleTokenStateT where
    lift c = SimpleTokenStateT $ \s -> c >>= (\x -> return (x, s))

evalSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleTokenData -> m a
evalSimpleTokenStateT m s = runSimpleTokenStateT m s >>= return . fst

withSimpleTokenStateT :: (Monad m)
    => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m a -> SimpleTokenStateT m a
withSimpleTokenStateT f m = SimpleTokenStateT $ runSimpleTokenStateT m . f >>= return

-- | SimpleTokenStateT State Operations
--
getSimpleTokenData :: (Monad m) => SimpleTokenStateT m SimpleTokenData
getSimpleTokenData = SimpleTokenStateT (return . \s -> (s, s))

putSimpleTokenData :: (Monad m) => SimpleTokenData -> SimpleTokenStateT m ()
putSimpleTokenData s = SimpleTokenStateT (return . \_ -> ((), s))

modifySimpleTokenData :: (Monad m) => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m ()
modifySimpleTokenData f = SimpleTokenStateT (return . \s -> ((), f s))

initSimpleToken :: (Monad m) => [(SimpleAddress, SimpleAccount)] -> SimpleTokenStateT m ()
initSimpleToken alist = putSimpleTokenData SimpleTokenData
    { accounts = M.fromList alist
    , cfaAgreements = M.fromList []
    }

-- | SimpleTokenStateT m is a SuperfluidToken instance
--
instance (Monad m) => SF.SuperfluidToken (SimpleTokenStateT m) where

    type LQ (SimpleTokenStateT m) = Wad
    type TS (SimpleTokenStateT m) = SimpleTimestamp
    type RTB (SimpleTokenStateT m) = SimpleRealtimeBalance
    type ADDR (SimpleTokenStateT m) = SimpleAddress
    type ACC (SimpleTokenStateT m) = SimpleAccount

    getAccount a = getSimpleTokenData >>= \s -> return $
        case M.lookup a (accounts s) of
            Just value -> value
            Nothing    -> createSimpleAccount a (toWad (0 :: Double)) 0

    getFlow a b = getSimpleTokenData >>= \s -> return $
        case M.lookup (show(a)++":"++show(b)) (cfaAgreements s) of
            Just value -> value
            Nothing    -> CFA.CFAContractData 0 0

    updateFlow sender receiver newFlowRate t = do
        updates <- SF.updateFlowPure sender receiver newFlowRate t
        mapM_ (\u -> case u of
            SF.UpdateFlow flow' -> do
                modifySimpleTokenData (\vs -> vs {
                    cfaAgreements = M.insert
                        (show(sender)++":"++show(receiver))
                        flow'
                        (cfaAgreements vs)
                })
            SF.UpdateAccountFlow (accountAddr, accountFlow') -> do
                account <- SF.getAccount accountAddr
                modifySimpleTokenData (\vs -> vs {
                    accounts = M.insert
                        accountAddr
                        (SF.updateCFAAccountData account accountFlow')
                        (accounts vs)
                })
            ) updates

-- | Other SimpleTokenStateT Operations
--
addAccount :: (Monad m) => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accountAddr account = modifySimpleTokenData (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: (Monad m) => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = getSimpleTokenData >>= \s -> return $ M.toList (accounts s)
