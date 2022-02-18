module Superfluid.Validator.Simulation
    -- SimMonad operations
    ( SimMonad
    , runSimMonad
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

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
import qualified Data.Map                           as M
import           Data.Maybe
import           GHC.Stack

import qualified Superfluid.Instances.Simple.System as SF
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
    { tokens      :: M.Map String SF.SimpleTokenData
    }
type SimMonad = StateT SimData IO

type TokenMonad = SF.SimpleTokenStateT IO

runSimMonad :: SimMonad () -> IO ()
runSimMonad = flip evalStateT SimData { tokens = def }

-- ============================================================================
-- | SimMonad Operations
--
runTokenMonadWithSimData :: HasCallStack => String -> (SimData -> TokenMonad a) -> SimMonad a
runTokenMonadWithSimData tokenId mf = do
    s <- get
    (a, token') <- case M.lookup tokenId (tokens s) of
                        Just token -> liftIO $ SF.runSimpleTokenStateT (mf s) token
                        Nothing    -> error $ "No such tokenId: " ++ tokenId
    put s { tokens = M.insert tokenId token' (tokens s) }
    return a

runTokenMonad :: HasCallStack => String -> TokenMonad a -> SimMonad a
runTokenMonad tokenId m = runTokenMonadWithSimData tokenId (const m)

createToken :: HasCallStack => SimpleTimestamp -> String -> [(SimpleAddress, SimpleAccount)] -> SimMonad ()
createToken t tokenId alist = modify (\vs -> vs {
        tokens = M.insert tokenId (SF.createSimpleToken t alist) (tokens vs)
    })

-- ============================================================================
-- | TokenMonad Operations
--
getAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad SimpleAccount
getAccountByAlias alias _= SF.getAccount $ fromJust $ createSimpleAddress alias

printAccount :: HasCallStack => SimpleAccount -> SimData -> TokenMonad ()
printAccount acc _ = do
    t <- SF.getCurrentTime
    liftIO $ putStrLn $ SF.showAt acc t ++ "\n"

printAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad ()
printAccountByAlias alias s = getAccountByAlias alias s >>= flip printAccount s

sumTotalLiquidity :: HasCallStack => SimData -> TokenMonad SimpleRealtimeBalance
sumTotalLiquidity _ = do
    t <- SF.getCurrentTime
    accounts <- SF.listAccounts
    return $ sumAllSimpleAccount (map snd accounts) t

printTokenState :: HasCallStack => SimData -> TokenMonad ()
printTokenState s = do
    let banner = 60 `replicate` '='
    liftIO $ putStrLn banner
    liftIO $ putStrLn $ "## Accounts\n"
    accounts <- SF.listAccounts
    mapM_ (flip (printAccount . snd) s) accounts
    totalLiquidtySum <- sumTotalLiquidity s
    liftIO $ putStrLn $ "## Token Info\n"
    liftIO $ putStrLn $ "Total Balance: " ++ (show totalLiquidtySum)
    liftIO $ putStrLn (banner ++ "\n")
