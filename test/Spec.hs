import Data.Monoid ((<>))

import Control.Monad ((>>=))

import Test.HUnit

import System.IO (IO(..), print)

import qualified JoScript.Util.TextTest
import qualified JoScript.Util.StringsTest
import qualified JoScript.Util.JsonTest

allTests = JoScript.Util.TextTest.tests
        <> JoScript.Util.StringsTest.tests
        <> JoScript.Util.JsonTest.tests

main :: IO ()
main = runTestTT (TestList allTests) >>= print

