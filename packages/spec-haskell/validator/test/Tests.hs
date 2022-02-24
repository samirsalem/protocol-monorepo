import           System.Exit
import           Test.HUnit

import qualified Superfluid.System_Test

main :: IO ()
main = do
    results <- runTestTT
        ( Superfluid.System_Test.tests
        )
    if (errors results + failures results == 0)
    then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
