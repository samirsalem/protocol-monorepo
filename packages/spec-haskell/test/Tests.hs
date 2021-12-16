import Test.HUnit

import qualified SuperfluidTest

tests :: Test
tests = TestList SuperfluidTest.tests

main :: IO Counts
main = do
  runTestTT tests
