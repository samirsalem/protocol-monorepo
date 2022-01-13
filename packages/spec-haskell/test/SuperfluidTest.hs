module SuperfluidTest where

import Test.HUnit

import Superfluid (Timestamp)
import qualified Superfluid.Core.RealtimeBalance as RealtimeBalance
    ( RealtimeBalance(..)
    )
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.Testing.SimpleAccount as SimpleAccount
import Superfluid.Testing.SimpleAccount
    ( toWad
    , SimpleAccount
    , createSimpleAccount
    , sumAllSimpleAccount)

createTestAccount :: String -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = toWad (100.0 :: Double)

simpleScenarioTest :: Test
simpleScenarioTest = TestLabel "Simple Scenario Test" $ TestCase (do
    let t1 = 0
    let g1 = CFA.CFAAgreementData 0 0
    let alice1 = createTestAccount "alice" t1
    let bob1 = createTestAccount "bob" t1
    let (g2, alice2cfa, bob2cfa) = CFA.updateFlow
            g1
            (SimpleAccount.cfa alice1) (SimpleAccount.cfa bob1)
            (toWad(0.0001::Double)) t1
    let alice2 = alice1 { SimpleAccount.cfa = alice2cfa }
    let bob2 = bob1 { SimpleAccount.cfa = bob2cfa }
    assertEqual
        "flow rate"
        (CFA.flowRate g2)
        (toWad (0.0001::Double))
    let t2 = t1 + 3600 * 24
    assertEqual
        "balance sum"
        (RealtimeBalance.availableBalance $ sumAllSimpleAccount [alice2, bob2] t2)
        (200*(10^(18::Integer)))
    )

tests :: Test
tests = TestList
    [ simpleScenarioTest
    ]
