module Superfluid.Validator.Demo (demo) where

import           Control.Monad.IO.Class

import qualified Superfluid.System                 as SF

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
createTestAccount a t = createSimpleAccount a b t
    where b = toWad (100.0 :: Double)

demo :: SimMonad ()
demo = do
    let token = "DAI"
    t0 <- getCurrentTime
    liftIO $ putStrLn $ "# T0: initial state"

    let t1 = t0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"
    liftIO $ putStrLn $ "# T1: create test accounts & flows" ++ (show (t1 - t0))
    createToken token $ (map (\a -> (a, createTestAccount a t1)) [alice, bob, carol])
    runTokenMonad token $ SF.updateFlow alice bob (toWad (0.0001::Double)) t1
    runTokenMonad token $ SF.updateFlow alice carol (toWad (0.0002::Double)) t1
    runTokenMonadWithSimData token printTokenState

    t2 <- timeTravel $ 3600 * 24
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ (show (t2 - t0))
    runTokenMonadWithSimData token printTokenState
