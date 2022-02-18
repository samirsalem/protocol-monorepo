module Superfluid.Validator.Demo (demo) where

import           Control.Monad.IO.Class
import           Data.Maybe

import qualified Superfluid.Instances.Simple.System as SF
import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleTimestamp
    , createSimpleAccount
    , createSimpleAddress
    , toWad
    )

import           Superfluid.Validator.Simulation

createTestAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a t -- where b = toWad (100.0 :: Double)

demo :: SimMonad ()
demo = do
    let token = "DAI"
    t0 <- getCurrentTime
    liftIO $ putStrLn $ "# T0: initial state"

    let t1 = t0
    let alice = fromJust $ createSimpleAddress "alice"
    let bob = fromJust $ createSimpleAddress "bob"
    let carol = fromJust $ createSimpleAddress "carol"
    let accounts = map (\a -> (a, createTestAccount a t1)) [alice, bob, carol]
    liftIO $ putStrLn $ "# T1: create test accounts & flows" ++ (show (t1 - t0))
    createToken token $ accounts
    runTokenMonad token $ SF.updateFlow alice bob (toWad (0.0001::Double)) t1
    runTokenMonad token $ SF.updateFlow alice carol (toWad (0.0002::Double)) t1
    runTokenMonadWithSimData token printTokenState

    t2 <- timeTravel $ 3600 * 24
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ (show (t2 - t0))
    runTokenMonadWithSimData token printTokenState
