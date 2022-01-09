module SuperfluidTest where

import Test.Framework
import Test.Framework.Providers.HUnit

import Superfluid.Types (Timestamp, toWad)
import Superfluid (RealtimeBalance(..))
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFAv1
import qualified Superfluid.Testing.SimpleAccount as SimpleAccount
import Superfluid.Testing.SimpleAccount
    ( Address
    , SimpleAccount
    , createSimpleAccount
    , sumAllSimpleAccount)

createTestAccount :: Address -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = fromInteger (toWad (100.0::Double))

simpleScenarioTest :: Test
simpleScenarioTest = testCase "Simple Scenario Test" (do
    let t1 = 0
    let g1 = CFAv1.CFAv1AgreementData 0 0
    let alice1 = createTestAccount "alice" t1
    let bob1 = createTestAccount "bob" t1
    let (g2, _, _) = CFAv1.updateFlow
            g1
            (SimpleAccount.cfa alice1) (SimpleAccount.cfa bob1)
            (toWad(0.0001::Double)) t1
    assertEqual
        (CFAv1.flowRate g2)
        11
    -- assertEqual
    --     "balance sum"
    --     (availableBalance $ sumAllSimpleAccount [alice2, bob2] t2)
    --     (200*(10^(18::Integer)))
    )

tests :: [Test]
tests =
    [simpleScenarioTest]
