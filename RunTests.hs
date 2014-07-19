import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests

{- TODO:
x Documentation
* Main app to load and parse a suite of matchers from a file, and apply to an input string
* Tool to generate Descs etc from existing regression data
* Regression suite
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        , BestMatchTests.tests
        ]