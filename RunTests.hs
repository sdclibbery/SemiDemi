import Test.HUnit
import qualified MatcherTests
import qualified ParserTests

{- TODO:
* Core system to apply a collection of Matchers to an input string and pick the best
* Main app to load and parse a suite of matchers from a file, and apply to an input string
* Tool to generate Descs etc from existing regression data
* Regression suite
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        ]