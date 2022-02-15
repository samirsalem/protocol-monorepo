module Superfluid.Validator.Simulation
    -- SimMonad operations
    ( SimMonad
    , runSimMonad
    , getCurrentTime
    , timeTravel
    , runTokenMonad
    , runTokenMonadWithSimData
    , createToken
    -- TokenMonad operations
    , TokenMonad
    , getAccountByAlias
    , printAccount
    , printAccountByAlias
    , printTokenState
    ) where

import GHC.Stack
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
import qualified Data.Map                           as M

import qualified Superfluid.System                  as SF

import           Superfluid.Instances.Simple.System
    ( SimpleTokenData
    , SimpleTokenStateT
    , initSimpleToken
    , listAccounts
    , runSimpleTokenStateT
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
-- | Simulation Monad Stacks
--
--   SimMonad    : SimState | IO
--   TokenMonad  : SimpleTokenState | IO
data SimData = SimData
    { currentTime :: SimpleTimestamp
    , tokens      :: M.Map String SimpleTokenData
    }
type SimMonad = StateT SimData IO

type TokenMonad = SimpleTokenStateT IO

runSimMonad :: SimpleTimestamp -> SimMonad () -> IO ()
runSimMonad t = flip evalStateT SimData { currentTime = t, tokens = def }

-- ============================================================================
-- | SimMonad Operations
--
getCurrentTime :: HasCallStack => SimMonad SimpleTimestamp
getCurrentTime = do
    s <- get
    return (currentTime s)

timeTravel :: HasCallStack => Integer -> SimMonad SimpleTimestamp
timeTravel d = do
    s <- get
    let t' = fromInteger $ d + toInteger (currentTime s)
    modify (\vs -> vs { currentTime = t' })
    return t'

runTokenMonadWithSimData :: HasCallStack => String -> (SimData -> TokenMonad a) -> SimMonad a
runTokenMonadWithSimData tokenId mf = do
    s <- get
    (a, token') <- case M.lookup tokenId (tokens s) of
                        Just token -> liftIO $ runSimpleTokenStateT (mf s) token
                        Nothing    -> error $ "No such tokenId: " ++ tokenId
    put s { tokens = M.insert tokenId token' (tokens s) }
    return a

runTokenMonad :: HasCallStack => String -> TokenMonad a -> SimMonad a
runTokenMonad tokenId m = runTokenMonadWithSimData tokenId (const m)

createToken :: HasCallStack => String -> [(SimpleAddress, SimpleAccount)] -> SimMonad ()
createToken tokenId alist = do
    modify (\vs -> vs { tokens = M.insert tokenId def (tokens vs) })
    runTokenMonad tokenId $ initSimpleToken alist

-- ============================================================================
-- | TokenMonad Operations
--
getAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad SimpleAccount
getAccountByAlias alias _= SF.getAccount $ createSimpleAddress alias

printAccount :: HasCallStack => SimpleAccount -> SimData -> TokenMonad ()
printAccount acc s = do
    liftIO $ putStrLn $ SF.showAt acc (currentTime s)

printAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad ()
printAccountByAlias alias s = getAccountByAlias alias s >>= flip printAccount s

sumTotalLiquidity :: HasCallStack => SimData -> TokenMonad SimpleRealtimeBalance
sumTotalLiquidity s = do
    accounts <- listAccounts
    return $ sumAllSimpleAccount (map snd accounts) (currentTime s)

printTokenState :: HasCallStack => SimData -> TokenMonad ()
printTokenState s = do
    let banner = 80 `replicate` '='
    liftIO $ putStrLn banner
    liftIO $ putStrLn $ "Accounts: "
    accounts <- listAccounts
    mapM_ (flip (printAccount . snd) s) accounts
    totalLiquidtySum <- sumTotalLiquidity s
    liftIO $ putStrLn $ "Total Balance: " ++ (show totalLiquidtySum)
    liftIO $ putStrLn (banner ++ "\n")
