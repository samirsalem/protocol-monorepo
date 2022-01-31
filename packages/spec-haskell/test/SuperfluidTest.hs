module SuperfluidTest where

import           Control.Monad.State
import           Data.Default
import           Test.HUnit

import           Superfluid                                  (Timestamp)

import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.Concepts.Account                 as Account
import qualified Superfluid.Concepts.RealtimeBalance         as RTB

import qualified Superfluid.System.SuperfluidToken           as SFToken

import           Superfluid.Instances.Simple                 (SimpleAccount,
                                                              SimpleToken,
                                                              SimpleTokenState,
                                                              Wad,
                                                              createSimpleAccount,
                                                              createSimpleAddress,
                                                              sumAllSimpleAccount,
                                                              toWad)
import qualified Superfluid.Instances.Simple.Account         as SimpleAccount
import qualified Superfluid.Instances.Simple.Token           as SimpleToken

initBalance :: Wad
initBalance = toWad (100.0 :: Double)

createTestAccount :: String -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = initBalance

type SimpleTokenTest = StateT SimpleToken IO

createSimpleTokenTestCase :: String -> SimpleTokenTest () -> Test
createSimpleTokenTestCase label testCase =
    TestLabel label $ TestCase $ evalStateT testCase def

updateTokenState :: (SimpleTokenState ()) -> SimpleTokenTest ()
updateTokenState op = modify $ execState op

simpleScenarioTest :: SimpleTokenTest ()
simpleScenarioTest = do
    let t0 = 0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"

    -- FIXME auto create account
    updateTokenState $ SimpleToken.addAccount alice (createTestAccount "alice" t0)
    updateTokenState $ SimpleToken.addAccount bob (createTestAccount "bob" t0)
    updateTokenState $ SimpleToken.addAccount carol (createTestAccount "carol" t0)

    s1 <- get
    let accounts = evalState SimpleToken.listAccounts s1
    liftIO $ assertEqual "num accounts" 3 (length accounts)

    updateTokenState $ SFToken.updateFlow alice bob (toWad(0.0001::Double)) t0
    s2 <- get
    liftIO $ assertEqual
        "alice net flow rate"
        (toWad (-0.0001::Double))
        (CFA.netFlowRate . SimpleAccount.cfa $ (evalState (SFToken.getAccount alice) s2))
    liftIO $ assertEqual
        "bob flow rate"
        (toWad (0.0001::Double))
        (CFA.netFlowRate . SimpleAccount.cfa $ (evalState (SFToken.getAccount bob) s2))
    liftIO $ assertEqual
        "carol flow rate"
        (toWad (0.0000::Double))
        (CFA.netFlowRate . SimpleAccount.cfa $ (evalState (SFToken.getAccount carol) s2))

    let t1 = t0 + 3600 * 24
    liftIO $ assertBool
        "balance sum"
        ((RTB.liquidityFromRTB $ (Account.balanceOf (evalState (SFToken.getAccount alice) s2) t1))
        < initBalance)
    liftIO $ assertEqual
        "balance sum"
        (RTB.availableBalance $ sumAllSimpleAccount (map snd accounts) t1)
        (3 * initBalance)

tests :: Test
tests = TestList
    [ createSimpleTokenTestCase "Simple Scenario Test" simpleScenarioTest
    ]
