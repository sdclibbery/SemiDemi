import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Good parse errors: line number and sub-parser error message
* Main app to load/parse a demi file, and apply to an input string, returning the userdata for the matching matcher.
* Tool to generate Descs etc from existing regression data
* Regression suite: automated tests
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]