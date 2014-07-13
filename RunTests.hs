import Test.HUnit
import qualified MatcherTests

{- TODO:
* Need to memoize fuzzyMatch to improve performance :-)
* Rename Matcher to Scorer
* Parser to generate ScorerDesc from marked up input string
* Matcher to apply a collection of Scorers to an input string and pick the best
* Tool to generate Descs etc from existing regression data
* Rgeression suite
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        ]