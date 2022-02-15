module Superfluid.Validator.SimState
    ( SimMonad
    , runSimMonad
    , initSimState
    , getCurrentTime
    , timeTravel
    , getAccountByAlias
    , printAccount
    , printAccountByAlias
    , printSimState
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Default

import qualified Superfluid.System                  as SF

import           Superfluid.Instances.Simple.System
    ( SimpleTokenStateT
    , createSimpleToken
    , evalSimpleTokenStateT
    , listAccounts
    , saveCurrentSimpleToken
    )
import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleRealtimeBalance
    , SimpleTimestamp
    , createSimpleAddress
    , sumAllSimpleAccount
    )


-- ============================================================================
-- | Simulation Monad Stack
--
--   SimpleTokenState | SimState | IO
--
data SimData = SimData
    { currentTime  :: SimpleTimestamp
    }

instance Default SimData where
    def = SimData { currentTime = def }

type SimStateT = StateT SimData

type SimMonad = SimpleTokenStateT (SimStateT IO)

runSimMonad :: SimMonad () -> IO ()
runSimMonad = (flip evalStateT def) . (flip evalSimpleTokenStateT def)

-- ============================================================================
-- | Simulation Operations
--
initSimState :: SimpleTimestamp -> [(SimpleAddress, SimpleAccount)] -> SimMonad ()
initSimState t alist = do
    lift $ modify (\_ -> SimData { currentTime = t })
    saveCurrentSimpleToken (createSimpleToken alist)

getCurrentTime :: SimMonad SimpleTimestamp
getCurrentTime = do
    s <- lift $ get
    return (currentTime s)

timeTravel :: Integer -> SimMonad SimpleTimestamp
timeTravel d = do
    s <- lift $ get
    let t' = fromInteger $ d + toInteger (currentTime s)
    lift $ modify (\vs -> vs { currentTime = t' })
    return t'

getAccountByAlias :: String -> SimMonad SimpleAccount
getAccountByAlias alias = SF.getAccount $ createSimpleAddress alias

printAccount :: SimpleAccount -> SimMonad ()
printAccount acc = do
    s <- lift $ get
    liftIO $ putStrLn $ SF.showAt acc (currentTime s)

printAccountByAlias :: String -> SimMonad ()
printAccountByAlias alias = getAccountByAlias alias >>= printAccount

sumTotalLiquidity :: SimMonad SimpleRealtimeBalance
sumTotalLiquidity = do
    s <- lift $ get
    accounts <- listAccounts
    return $ sumAllSimpleAccount (map snd accounts) (currentTime s)

printSimState :: SimMonad ()
printSimState = do
    let banner = 80 `replicate` '='
    liftIO $ putStrLn banner
    liftIO $ putStrLn $ "Accounts: "
    accounts <- listAccounts
    mapM_ (printAccount . snd) accounts
    totalLiquidtySum <- sumTotalLiquidity
    liftIO $ putStrLn $ "Total Balance: " ++ (show totalLiquidtySum)
    liftIO $ putStrLn (banner ++ "\n")
