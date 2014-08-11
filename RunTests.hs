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

x When PS tests all pass, update DemiMaker.hs to produce the required matchers...
* Try regression again
 * DemiMaker adds version markup
* Remove generic smarttv matcher from .demi file, but use it as the default result when nothing else matches

-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]