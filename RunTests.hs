import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Regression suite: automated tests
* Refine Demi file generation to pass all regression tests...
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]