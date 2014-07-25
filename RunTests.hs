import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Main app to load/parse a demi file, and apply to an input string, returning the userdata for the matching matcher.
 x Create temp test .demi file
* Tool to generate Descs etc from existing regression data
* Regression suite: automated tests
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]