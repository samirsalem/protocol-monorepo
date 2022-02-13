module Superfluid.Validator.SimState
    ( SimState
    , SimStateMonad
    , initSimState
    , getCurrentTime
    , timeTravel
    , getAccountByAlias
    , printAccount
    , printAccountByAlias
    , printSimState
    , execTokenStateOp
    ) where

import           Control.Monad.State
import           Data.Default

import qualified Superfluid.System           as SF

import           Superfluid.Instances.Simple
    ( SimpleAccount
    , SimpleAddress
    , SimpleRealtimeBalance
    , SimpleTimestamp
    , SimpleTokenData
    , SimpleTokenState
    , createSimpleAddress
    , createSimpleToken
    , listAccounts
    , sumAllSimpleAccount
    )



data SimState = SimState
    { currentTime  :: SimpleTimestamp
    , defaultToken :: SimpleTokenData
    }

type SimStateMonad = StateT SimState IO

instance Default SimState
    where def = SimState { currentTime = def, defaultToken = undefined }

initSimState :: SimpleTimestamp -> [(SimpleAddress, SimpleAccount)] -> SimStateMonad ()
initSimState t alist = modify (\_ -> SimState
        { currentTime = t
        , defaultToken = createSimpleToken alist
        })

getCurrentTime :: SimStateMonad SimpleTimestamp
getCurrentTime = do
    s <- get
    return (currentTime s)

timeTravel :: Integer -> SimStateMonad SimpleTimestamp
timeTravel d = do
    s <- get
    let t' = fromInteger $ d + toInteger (currentTime s)
    modify (\vs -> vs { currentTime = t' })
    return t'

getAccountByAlias :: String -> SimStateMonad SimpleAccount
getAccountByAlias alias = get >>= \s ->
    return $ evalState (SF.getAccount addr) (defaultToken s)
    where addr = createSimpleAddress alias

printAccount :: SimpleAccount -> SimStateMonad ()
printAccount acc = do
    s <- get
    liftIO $ putStrLn $ SF.showAt acc (currentTime s)

printAccountByAlias :: String -> SimStateMonad ()
printAccountByAlias alias = getAccountByAlias alias >>= printAccount

sumTotalLiquidity :: SimStateMonad SimpleRealtimeBalance
sumTotalLiquidity = do
    s <- get
    let accounts = evalState listAccounts (defaultToken s)
    return $ sumAllSimpleAccount (map snd accounts) (currentTime s)

printSimState :: SimStateMonad ()
printSimState = do
    s <- get
    let banner = 80 `replicate` '='
    let accounts = evalState listAccounts (defaultToken s)
    liftIO $ putStrLn banner
    liftIO $ putStrLn $ "Accounts: "
    -- mapM_ ((printAccountByAlias s) . show . fst) accounts
    mapM_ (printAccount . snd) accounts
    totalLiquidtySum <- sumTotalLiquidity
    liftIO $ putStrLn $ "Total Balance: " ++ (show totalLiquidtySum)
    liftIO $ putStrLn (banner ++ "\n")

execTokenStateOp :: (SimpleTokenState ()) -> SimStateMonad ()
execTokenStateOp op = modify (\s -> s {
        defaultToken = execState op (defaultToken s)
    })
