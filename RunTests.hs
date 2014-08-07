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


x Try regression now...
 x Shorter matchers seem to always win!
 x Also, too slow..?
* Remove generic smarttv matcher from .demi file, but use it as the default result when nothing else matches
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