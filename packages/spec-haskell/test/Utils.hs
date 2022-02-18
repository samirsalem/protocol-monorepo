module Utils where

import           Control.Monad.IO.Class
import           Test.HUnit

import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import qualified Superfluid.Concepts.RealtimeBalance         as RTB

import qualified Superfluid.Instances.Simple.System          as SF
import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , Wad
    , sumAllSimpleAccount
    , toWad
    )


type SimpleTokenTest = SF.SimpleTokenStateT IO

cZERO_WAD :: Wad
cZERO_WAD = toWad (0 :: Double)

cINIT_BALANCE :: Wad
cINIT_BALANCE = toWad (100.0 :: Double)

createSimpleTokenTestCase :: String -> SimpleTokenTest () -> Test
createSimpleTokenTestCase label testCase = TestLabel label $ TestCase $
    SF.evalSimpleTokenStateT testCase (SF.createSimpleToken 0 [])

createTestAccount :: SimpleAddress -> SimpleTokenTest SimpleAccount
createTestAccount a = do
    SF.mintLiquidity a cINIT_BALANCE
    SF.getAccount a

expectAccountBalanceTo :: String -> SimpleAddress -> (Wad -> Bool) -> SimpleTokenTest ()
expectAccountBalanceTo label addr expr = do
    balance <- SF.balanceOf addr
    liftIO $ assertBool label (expr . RTB.liquidityFromRTB $ balance)

expeceTotalBalanceTo :: String -> (Wad -> Bool) -> SimpleTokenTest ()
expeceTotalBalanceTo label expr = do
    t <- SF.getCurrentTime
    accounts <- SF.listAccounts
    liftIO $ assertBool label (expr . RTB.availableBalance $ sumAllSimpleAccount (map snd accounts) t)

expectCFANetFlowRateTo :: String -> SimpleAddress -> (Wad -> Bool) -> SimpleTokenTest ()
expectCFANetFlowRateTo label addr expr = do
    account <- SF.getAccount addr
    liftIO $ assertBool label (expr . CFA.getNetFlowRate . SF.getCFAAccountData $ account)
