import Test.HUnit
import qualified MatcherTests
import qualified ParserTests

{- TODO:
* Core system to apply a collection of Matchers to an input string and pick the best
* Tool to generate Descs etc from existing regression data
* Rgeression suite
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        ]