module SuperfluidTest where

import Test.HUnit
import qualified Superfluid

createSuperfluidAccount :: String -> Superfluid.Timestamp -> Superfluid.Account
createSuperfluidAccount address t = Superfluid.Account address (100*(10^(18::Integer))) [
    (Superfluid.CFA_v1 t 0 0)
    ]

sumAll :: [Superfluid.Account] -> Superfluid.Timestamp -> Superfluid.RealtimeBalance
sumAll accounts t = foldl Superfluid.add (Superfluid.RealtimeBalance 0 0 0) (map (flip Superfluid.balanceOf t) accounts)

simpleScenarioTest :: Test
simpleScenarioTest = TestCase (do
    let t1 = 0
    let t2 = 100
    let alice1 = createSuperfluidAccount "alice" t1
    let bob1 = createSuperfluidAccount "bob" t1
    let (alice2, bob2) = Superfluid.updateFlow alice1 bob1 (Superfluid.toWad (0.0001 :: Double)) t1
    assertEqual "balance sum" (Superfluid.availableBalance $ sumAll [alice2, bob2] t2) (200*(10^(18::Integer)))
    )

tests :: [Test]
tests = [TestLabel "Simple Scenario Test" simpleScenarioTest]
