import Test.Framework
import Test.Framework.Providers.HUnit
import qualified SuperfluidTest

main :: IO ()
main = defaultMainWithOpts
  SuperfluidTest.tests
  mempty
