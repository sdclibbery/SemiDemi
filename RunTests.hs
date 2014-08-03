import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
? Will we need to have version markup? How would this play with the single editDistance calculation?
 ? Can we use the version markup in normalisation?
* Matcher must keep a copy of the original string
* In BestMatch, if multiple matches occur, do a single editDistance to find the best
* Must have a test with two matchers differing by a single space, then match against a string differing by several version chars
? Modify editDistance to score digit <-> digit differences less than other differences
 ? Or 'normalise' by removing versions from both strings before editDistance matching...
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]