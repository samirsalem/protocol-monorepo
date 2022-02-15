module Utils where

import           Control.Monad.IO.Class
import           Data.Default
import           Test.HUnit

import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.Concepts.Account                 as Account
import qualified Superfluid.Concepts.RealtimeBalance         as RTB
import qualified Superfluid.System                           as SF

import           Superfluid.Instances.Simple.System          (SimpleTokenStateT, evalSimpleTokenStateT, listAccounts)
import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleTimestamp
    , Wad
    , createSimpleAccount
    , sumAllSimpleAccount
    , toWad
    )


type SimpleTokenTest = SimpleTokenStateT IO

zeroWad :: Wad
zeroWad = toWad (0 :: Double)

initBalance :: Wad
initBalance = toWad (100.0 :: Double)

createTestAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = initBalance

createSimpleTokenTestCase :: String -> SimpleTokenTest () -> Test
createSimpleTokenTestCase label testCase =
    TestLabel label $ TestCase $ evalSimpleTokenStateT testCase def

expectAccountBalanceTo :: String -> SimpleAddress -> (Wad -> Bool) -> SimpleTimestamp -> SimpleTokenTest ()
expectAccountBalanceTo label addr expr t = do
    account <- SF.getAccount addr
    liftIO $ assertBool label (expr . RTB.liquidityFromRTB $ Account.balanceOf account t)

expeceTotalBalanceTo :: String -> (Wad -> Bool) -> SimpleTimestamp -> SimpleTokenTest ()
expeceTotalBalanceTo label expr t = do
    accounts <- listAccounts
    liftIO $ assertBool label (expr . RTB.availableBalance $ sumAllSimpleAccount (map snd accounts) t)

expectCFANetFlowRateTo :: String -> SimpleAddress -> (Wad -> Bool) -> SimpleTokenTest ()
expectCFANetFlowRateTo label addr expr = do
    account <- SF.getAccount addr
    liftIO $ assertBool label (expr . CFA.netFlowRate . SF.getCFAAccountData $ account)
