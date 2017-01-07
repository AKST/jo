import Protolude

import Test.HUnit

import qualified JoScript.Util.TextTest
import qualified JoScript.Util.StringsTest
import qualified JoScript.Util.JsonTest
import qualified JoScript.Pass.ParseTest

allTests = JoScript.Util.TextTest.tests
        <> JoScript.Util.StringsTest.tests
        <> JoScript.Util.JsonTest.tests
        <> JoScript.Pass.ParseTest.tests

main :: IO ()
main = runTestTT (TestList allTests) >>= print

