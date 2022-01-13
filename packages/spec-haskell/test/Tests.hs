import System.Exit
import Test.HUnit
import qualified SuperfluidTest

main :: IO ()
main = do
    results <- runTestTT $
        SuperfluidTest.tests
    if (errors results + failures results == 0)
    then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
