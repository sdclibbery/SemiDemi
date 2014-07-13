import Test.HUnit
import qualified MatcherTests

{- TODO:
* Need a new fuzzy matcher. Run Lev like algo ONCE. For the last match, it returns the editDistance+remainingNeedleLength and the remaining haystack
* Rename Matcher to Scorer
* Parser to generate ScorerDesc from marked up input string
* Matcher to apply a collection of Scorers to an input string and pick the best
* Tool to generate Descs etc from existing regression data
* Rgeression suite
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        ]