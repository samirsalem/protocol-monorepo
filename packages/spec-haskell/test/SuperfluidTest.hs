module SuperfluidTest where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Test.HUnit

import qualified Superfluid.Instances.Simple.System as SF
import           Superfluid.Instances.Simple.Types  (createSimpleAddress, toWad)

import           Utils


simple1to1ScenarioTest :: SimpleTokenTest ()
simple1to1ScenarioTest = do
    -- T0: test initial condition
    _ <- SF.getCurrentTime
    let alice = fromJust $ createSimpleAddress "alice"
    let bob = fromJust $ createSimpleAddress "bob"
    let carol = fromJust $ createSimpleAddress "carol"
    expectAccountBalanceTo "alice should have no money" alice (== cZERO_WAD)
    expectAccountBalanceTo "bob should have no money" bob (== cZERO_WAD)
    expectAccountBalanceTo "carol should have no money" carol (== cZERO_WAD)
    expectCFANetFlowRateTo "alice should have no flow" alice (== cZERO_WAD)
    expectCFANetFlowRateTo "bob should have no flow" bob (== cZERO_WAD)
    expectCFANetFlowRateTo "carol should have no flow" carol (== cZERO_WAD)
    expeceTotalBalanceTo "total balance zero" (== cZERO_WAD)
    accounts <- SF.listAccounts
    liftIO $ assertEqual "expected zero account" 0 (length accounts)
    -- creating test accounts
    SF.addAccount alice =<< createTestAccount alice
    SF.addAccount bob =<< createTestAccount bob
    SF.addAccount carol =<< createTestAccount carol
    accounts' <- SF.listAccounts
    liftIO $ assertEqual "expected number of accounts" 3 (length accounts')
    expeceTotalBalanceTo "total balance 3 x cINIT_BALANCE" (== 3 * cINIT_BALANCE)
    -- creating flow: alice -> bob @ 0.0001/s
    SF.updateFlow alice bob (toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -1x net flowrate" alice (== toWad(-0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have zero net flowrate" carol (== toWad(0.0000 :: Double))

    -- T1: move time forward and test balance moves
    _ <- SF.timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< cINIT_BALANCE)
    expectAccountBalanceTo "bob should receive money" bob (> cINIT_BALANCE)
    expectAccountBalanceTo "carol should be the same" carol (== cINIT_BALANCE)
    expeceTotalBalanceTo "total balance stays the same" (== 3 * cINIT_BALANCE)

simple1to2ScenarioTest :: SimpleTokenTest ()
simple1to2ScenarioTest = do
    -- T0: test initial condition
    -- t0 <- SF.getCurrentTime
    let alice = fromJust $ createSimpleAddress "alice"
    let bob = fromJust $ createSimpleAddress "bob"
    let carol = fromJust $ createSimpleAddress "carol"
    -- creating test accounts & flows: alice -> bob @ 0.0001/s & alice -> carol @ 0.0001/s
    SF.addAccount alice =<< createTestAccount alice
    SF.addAccount bob =<< createTestAccount bob
    SF.addAccount carol =<< createTestAccount carol
    SF.updateFlow alice bob (toWad (0.0001 :: Double))
    SF.updateFlow alice carol (toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -2x net flowrate" alice (== toWad(-0.0002 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" carol (== toWad(0.0001 :: Double))

    -- T1: move time forward and test balance moves
    _ <- SF.timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< cINIT_BALANCE)
    expectAccountBalanceTo "bob should receive money" bob (> cINIT_BALANCE)
    expectAccountBalanceTo "carol should also receive money" carol (> cINIT_BALANCE)
    expeceTotalBalanceTo "total balance stays the same" (== 3 * cINIT_BALANCE)

tests :: Test
tests = TestList $ map (uncurry createSimpleTokenTestCase)
    [ ("Simple 1to1 Scenario Test", simple1to1ScenarioTest)
    , ("Simple 1to2 Scenario Test", simple1to2ScenarioTest)
    ]
