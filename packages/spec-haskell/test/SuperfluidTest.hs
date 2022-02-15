module SuperfluidTest where

import           Control.Monad.IO.Class
import           Data.Default
import           Test.HUnit

import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.Concepts.Account                 as Account
import qualified Superfluid.Concepts.RealtimeBalance         as RTB
import qualified Superfluid.System                           as SF

import           Superfluid.Instances.Simple.System
    ( SimpleTokenStateT
    , addAccount
    , evalSimpleTokenStateT
    , listAccounts
    )
import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleTimestamp
    , Wad
    , createSimpleAccount
    , createSimpleAddress
    , sumAllSimpleAccount
    , toWad
    )

initBalance :: Wad
initBalance = toWad (100.0 :: Double)

createTestAccount :: String -> SimpleTimestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = initBalance

type SimpleTokenTest = SimpleTokenStateT IO

createSimpleTokenTestCase :: String -> SimpleTokenTest () -> Test
createSimpleTokenTestCase label testCase =
    TestLabel label $ TestCase $ evalSimpleTokenStateT testCase def

-- updateTokenState :: (SimpleTokenState ()) -> SimpleTokenTest ()
-- updateTokenState op = modify $ execState op

expectCFANetFlowRate :: SimpleAddress -> Double -> SimpleTokenTest ()
expectCFANetFlowRate addr expected = do
    account <- SF.getAccount addr
    liftIO $ assertEqual ((show addr) ++ " net flow rate")
        (toWad expected)
        (CFA.netFlowRate . SF.getCFAAccountData $ account)

simpleScenarioTest :: SimpleTokenTest ()
simpleScenarioTest = do
    let t0 = 0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"

    -- FIXME auto create account
    -- s0 <- get
    -- let accounts = evalState SimpleToken.listAccounts s0
    -- liftIO $ assertEqual "num accounts" 3 (length accounts)

    addAccount alice (createTestAccount "alice" t0)
    addAccount bob (createTestAccount "bob" t0)
    addAccount carol (createTestAccount "carol" t0)
    accounts <- listAccounts
    liftIO $ assertEqual "num accounts" 3 (length accounts)

    SF.updateFlow alice bob (toWad(0.0001::Double)) t0

    -- s2 <- getCurrentSimpleToken
    expectCFANetFlowRate alice (-0.0001::Double)
    expectCFANetFlowRate bob (0.0001::Double)
    expectCFANetFlowRate carol (0.0000::Double)

    let t1 = t0 + 3600 * 24

    aliceAccount <- SF.getAccount alice
    liftIO $ assertBool
        "balance sum"
        ((RTB.liquidityFromRTB $ Account.balanceOf aliceAccount t1)
        < initBalance)

    liftIO $ assertEqual
        "balance sum"
        (RTB.availableBalance $ sumAllSimpleAccount (map snd accounts) t1)
        (3 * initBalance)

tests :: Test
tests = TestList
    [ createSimpleTokenTestCase "Simple Scenario Test" simpleScenarioTest
    ]
