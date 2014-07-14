import Test.HUnit
import qualified MatcherTests

{- TODO:
* Still not fast enough :-(
* Parser to generate Desc from marked up input string
* Core system to apply a collection of Matchers to an input string and pick the best
* Tool to generate Descs etc from existing regression data
* Rgeression suite
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        ]