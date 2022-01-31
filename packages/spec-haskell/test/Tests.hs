import qualified SuperfluidTest
import           System.Exit
import           Test.HUnit

main :: IO ()
main = do
    results <- runTestTT SuperfluidTest.tests
    if (errors results + failures results == 0)
    then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
