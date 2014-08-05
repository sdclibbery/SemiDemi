import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
* Suggest:
 * Do full markup, with exact, disallowed, version and fuzzy-default
 * Version should markup the surrounding context as well
 * Then split matching in two:
  * First apply Exact and Disallowed to get shortlist
  * If theres only one, success!
  * If more than one, score
   * First, normalise by removing versions using the context info
   * Then do a full editDistance on the full strings to get the score...

* Disambiguate multiple matches
 x Matcher.hs has score function, just does editDistance for now
 * BestMatch sends multiple matches back for scoring and then picks the best
  * Test: same Exact matchers, but correct one matches because of string compare
* Try regression now...
* Support version normalisation
 * Must have a test with two matchers differing by a single space, then match against a string differing by several version chars
 * Matcher data for version
 * Parse version, including context
 * Matcher handles version normalisation before scoring
* Try regression again
 * DemiMaker adds version markup

-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]