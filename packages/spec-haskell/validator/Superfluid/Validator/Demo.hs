module Superfluid.Validator.Demo (demo) where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Time.Clock.POSIX              (getPOSIXTime)
import           GHC.Stack

import qualified Superfluid.Instances.Simple.System as SF
import           Superfluid.Instances.Simple.Types
    ( SimpleTimestamp
    , Wad
    , createSimpleAccount
    , createSimpleAddress
    , toWad
    )

import           Superfluid.Validator.Simulation


now :: IO SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: SimpleTimestamp)

initBalance :: Wad
initBalance = toWad (100.0 :: Double)

demo :: HasCallStack => SimMonad ()
demo = do
    let token = "DAI"
    t0 <- liftIO $ now
    liftIO $ putStrLn $ "# T0: initial state"

    let t1 = t0
    liftIO $ putStrLn $ "# T1: create test accounts & flows" ++ (show (t1 - t0))
    let alice = fromJust $ createSimpleAddress "alice"
    let bob = fromJust $ createSimpleAddress "bob"
    let carol = fromJust $ createSimpleAddress "carol"
    let accounts = map (\a -> (a, createSimpleAccount a t1)) [alice, bob, carol]
    createToken t0 token accounts
    runTokenMonad token $ mapM_ (\(a, _) -> do SF.mintLiquidity a initBalance) accounts
    runTokenMonad token $ SF.updateFlow alice bob (toWad (0.0001::Double))
    runTokenMonad token $ SF.updateFlow alice carol (toWad (0.0002::Double))
    runTokenMonadWithSimData token printTokenState

    t2 <- runTokenMonad token $ SF.timeTravel $ 3600 * 24
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ (show (t2 - t0))
    runTokenMonadWithSimData token printTokenState
