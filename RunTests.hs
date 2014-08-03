import Test.HUnit
import qualified MatcherTests
import qualified ParserTests
import qualified BestMatchTests
import qualified DemiParserTests

{- TODO:
x Disable the DemiParser duplicate validation
* Remove 'dubious' DemiMaker matches
* In BestMatch, if multiple matches occur, do a single editDistance to find the best
-}

main = runTestTT $ TestList
        [ MatcherTests.tests
        , ParserTests.tests
        , BestMatchTests.tests
        , DemiParserTests.tests
        ]