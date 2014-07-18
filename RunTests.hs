import Test.HUnit
import qualified MatcherTests
import qualified ParserTests

{- TODO:
x Switch to other delimiters: open/close must be distinct
* Need to support escaping of delimiters
* File loader to load and parse a suite of matchers from a file
* Core system to apply a collection of Matchers to an input string and pick the best
* Tool to generate Descs etc from existing regression data
* Rgeression suite
-}

main = runTestTT $ TestList
        [ ParserTests.tests
        , MatcherTests.tests
        ]