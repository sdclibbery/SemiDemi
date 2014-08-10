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

!! playstation is a pain in the bum!
! The one that now has generic version matching eats all the others :-/
? Do we need some kind of numeric-aware version matching? EG [<4.00] or something? This would need to be applied at the match (not score) phase

x Need to score for closeness to version number
 x Matcher Version must have second string for original version
 x Parser parses the second string
 x Score between 0-1 for distance from each version
* Need to be able to mark up some versions as contributing to the score, and others not
 * In PS case, Mozilla version is irrelevant, but PS version matters...
 * Can do entirely in the parser:   [Va1] -> Version "a" "1"   while  [va1] -> Version "a" ""
 * Then update tvs.demi
* When PS tests all pass, update DemiMaker.hs to produce the required matchers...
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