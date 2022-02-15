module SuperfluidTest where

import           Control.Monad.IO.Class
import           Test.HUnit

import qualified Superfluid.System                  as SF

import           Superfluid.Instances.Simple.System (addAccount, listAccounts)
import           Superfluid.Instances.Simple.Types  (createSimpleAddress, toWad)

import           Utils


simple1to1ScenarioTest :: SimpleTokenTest ()
simple1to1ScenarioTest = do
    -- T0: test initial condition
    let t0 = 0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"
    expectAccountBalanceTo "alice should have no money" alice (== zeroWad) t0
    expectAccountBalanceTo "bob should have no money" bob (== zeroWad) t0
    expectAccountBalanceTo "carol should have no money" carol (== zeroWad) t0
    expectCFANetFlowRateTo "alice should have no flow" alice (== zeroWad)
    expectCFANetFlowRateTo "bob should have no flow" bob (== zeroWad)
    expectCFANetFlowRateTo "carol should have no flow" carol (== zeroWad)
    expeceTotalBalanceTo "total balance zero" (== zeroWad) t0
    accounts <- listAccounts
    liftIO $ assertEqual "expected zero account" 0 (length accounts)
    -- creating test accounts
    addAccount alice (createTestAccount alice t0)
    addAccount bob (createTestAccount bob t0)
    addAccount carol (createTestAccount carol t0)
    accounts' <- listAccounts
    liftIO $ assertEqual "expected number of accounts" 3 (length accounts')
    expeceTotalBalanceTo "total balance zero" (== 3 * initBalance) t0
    -- creating flow: alice -> bob @ 0.0001/s
    SF.updateFlow alice bob (toWad (0.0001 :: Double)) t0
    expectCFANetFlowRateTo "alice should have -1x net flowrate" alice (== toWad(-0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have zero net flowrate" carol (== toWad(0.0000 :: Double))

    -- T1: move time forward and test balance moves
    let t1 = t0 + 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< initBalance) t1
    expectAccountBalanceTo "bob should receive money" bob (> initBalance) t1
    expectAccountBalanceTo "carol should be the same" carol (== initBalance) t1
    expeceTotalBalanceTo "total balance stays the same" (== 3 * initBalance) t1

simple1to2ScenarioTest :: SimpleTokenTest ()
simple1to2ScenarioTest = do
    -- T0: test initial condition
    let t0 = 0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"
    -- creating test accounts & flows: alice -> bob @ 0.0001/s & alice -> carol @ 0.0001/s
    addAccount alice (createTestAccount alice t0)
    addAccount bob (createTestAccount bob t0)
    addAccount carol (createTestAccount carol t0)
    SF.updateFlow alice bob (toWad (0.0001 :: Double)) t0
    SF.updateFlow alice carol (toWad (0.0001 :: Double)) t0
    expectCFANetFlowRateTo "alice should have -2x net flowrate" alice (== toWad(-0.0002 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" carol (== toWad(0.0001 :: Double))

    -- T1: move time forward and test balance moves
    let t1 = t0 + 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< initBalance) t1
    expectAccountBalanceTo "bob should receive money" bob (> initBalance) t1
    expectAccountBalanceTo "carol should also receive money" carol (> initBalance) t1
    expeceTotalBalanceTo "total balance stays the same" (== 3 * initBalance) t1

tests :: Test
tests = TestList $ map (uncurry createSimpleTokenTestCase)
    [ ("Simple 1to1 Scenario Test", simple1to1ScenarioTest)
    , ("Simple 1to2 Scenario Test", simple1to2ScenarioTest)
    ]
